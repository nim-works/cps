import std/[macros]
import cps/[spec, transform, rewrites, hooks]
export Continuation, ContinuationProc
export cpsCall, cpsMagicCall, cpsVoodooCall, cpsMustJump

# we only support arc/orc due to its eager expr evaluation qualities
when not(defined(gcArc) or defined(gcOrc)):
  {.warning: "cps supports --gc:arc or --gc:orc only; " &
             "see https://github.com/nim-lang/Nim/issues/18099".}

# we only support panics because we don't want to run finally on defect
when not defined(nimPanics):
  {.warning: "cps supports --panics:on only; " &
             " see https://github.com/disruptek/cps/issues/110".}

type
  State* {.pure.} = enum
    ## Representation of the state of a continuation.
    Running    ## The continuation is active and running and can be resumed
    Dismissed  ## The continuation is currently somewhere else
    Finished   ## The continuation is finished and can no longer be resumed

proc state*(c: Continuation): State =
  ## Get the current state of a continuation
  if c == nil:
    Dismissed
  elif c.fn == nil:
    Finished
  else:
    Running

template running*(c: Continuation): bool =
  ## `true` if the continuation is running.
  c.state == Running

template finished*(c: Continuation): bool =
  ## `true` if the continuation is finished.
  c.state == Finished

proc trampoline*[T: Continuation](c: T): T =
  ## This is the basic trampoline: it will continue the continuation
  ## until it is no longer in 'running' state
  var c: Continuation = c
  while c.running:
    c = c.fn(c)
  result = T c

template dismissed*(c: Continuation): bool =
  ## `true` if the continuation was dimissed.
  c.state == Dismissed

macro cps*(T: typed, n: typed): untyped =
  ## This is the .cps. macro performing the proc transformation
  when defined(nimdoc):
    n
  else:
    cpsTransformProc(T, n)

proc makeErrorShim(n: NimNode): NimNode =
  ## Upgrades a procedure to serve as a CPS primitive, generating
  ## errors out of `.cps.` context and taking continuations as input.
  expectKind(n, nnkProcDef)

  # create a Nim-land version of the proc that throws an exception when called
  # from outside of CPS-land.
  var m = copyNimTree n

  del(m.params, 1)
  m.body = newStmtList:
    nnkRaiseStmt.newTree:
      newCall(
        bindSym"newException",
        nnkBracketExpr.newTree(bindSym"typedesc", bindSym"Defect"),
        newLit($n.name & "() is only valid in {.cps.} context")
      )

  result = newStmtList()
  when not defined(nimdoc):
    result.add n
  result.add m

macro cpsMagic*(n: untyped): untyped =
  let shim = n.makeErrorShim()
  shim[1].params[0] = newEmptyNode()
  shim[1].addPragma ident"cpsMustJump"
  shim[1].addPragma ident"cpsMagicCall"
  shim

macro cpsVoodoo*(n: untyped): untyped =
  let shim = n.makeErrorShim()
  shim[1].addPragma ident"cpsVoodooCall"
  shim

proc bootstrapSymbol(n: NimNode): NimNode =
  case n.kind
  of nnkProcDef:
    for n in n.pragma.items:
      if n.kind == nnkExprColonExpr:
        if $n[0] == "cpsBootstrap":
          if result.isNil:
            result = n[1]
          else:
            result = n.errorAst "redundant bootstrap pragmas?"
    if result.isNil:
      result = n.errorAst "welping malfunction"
  of nnkCallKinds:
    result = bootstrapSymbol(getImpl n[0])
  else:
    result = newCall(ident"typeOf", n)

proc doWhelp(n: NimNode; args: seq[NimNode]): NimNode =
  let sym = bootstrapSymbol n
  result = sym.newCall args

template whelpIt*(input: typed; body: untyped): untyped =
  var n = normalizingRewrites input
  if n.kind in nnkCallKinds:
    let p = getImpl n[0]
    if p.hasPragma "cpsBootstrap":
      var it {.inject.}: NimNode = doWhelp(p, n[1..^1])
      body
      it
    else:
      n.errorAst "the input to whelpIt must be a .cps. call"
  else:
    n.errorAst "the input to whelpIt must be a .cps. call"

macro whelp*(call: typed): Continuation =
  ## Instantiate the given continuation call but do not begin
  ## running it; instead, return the continuation as a value.
  result = whelpIt call:
    it = Head.hook(it)

macro whelp*(parent: Continuation; call: typed): Continuation =
  ## As in `whelp(call(...))`, but also links the new continuation to the
  ## supplied parent for the purposes of exception handling and similar.
  let sym = bootstrapSymbol call
  result = whelpIt call:
    it =
      newCall ident"Continuation":
        Tail.hook(newCall(ident"Continuation", parent),
                  sym.ensimilate it) #newCall(ident"Continuation", it))

template head*[T: Continuation](first: T): T {.used.} =
  ## This symbol may be reimplemented to configure a continuation
  ## for use when there is no parent continuation available.
  ## The return value specifies the continuation.
  first

proc tail*[T: Continuation](parent: Continuation; child: T): T {.used, inline.} =
  ## This symbol may be reimplemented to configure a continuation for
  ## use when it has been instantiated from inside another continuation;
  ## currently, this means assigning the parent to the child's `mom`
  ## field. The return value specifies the child continuation.
  ##
  ## NOTE: If you implement this as a template, be careful that you
  ##       assign the child to a variable before manipulating its fields,
  ##       as it may be an expression...
  result = child
  result.mom = parent

template coop*[T: Continuation](c: T): T {.used.} =
  ## This symbol may be reimplemented as a `.cpsMagic.` to introduce
  ## a cooperative yield at appropriate continuation exit points.
  ## The return value specifies the continuation.
  c

template boot*[T: Continuation](c: T): T {.used.} =
  ## This symbol may be reimplemented to refine a continuation after
  ## it has been allocated but before it is first run.
  ## The return value specifies the continuation.
  c

template pass*[T: Continuation](source, destination: T): T {.used.} =
  ## This symbol may be reimplemented to introduce logic during
  ## the transfer of control between parent and child continuations.
  ## The return value specifies the destination continuation.
  destination

template trace*(c: Continuation; fun: string; where: LineInfo) {.used.} =
  ## This symbol may be reimplemented to introduce control-flow
  ## tracing of the entry to each continuation leg.
  discard

proc alloc*[T: Continuation](c: typedesc[T]): T {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## allocation.
  new c

proc alloc*[T: Continuation](root: typedesc[T]; c: typedesc): c {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## allocation.
  new c

template dealloc*[T: Continuation](t: typedesc[T];
                                   c: sink Continuation) {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## deallocation.
  discard
