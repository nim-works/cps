import std/[macros]
import cps/[spec, transform, rewrites]
export Continuation, ContinuationProc
export cpsCall, cpsMagicCall, cpsVoodooCall, cpsMustJump

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

proc trampoline*(c: Continuation): Continuation =
  ## This is the basic trampoline: it will continue the continuation
  ## until it is no longer in 'running' state
  result = c
  while result.running:
    result = result.fn(result)

template dismissed*(c: Continuation): bool =
  ## `true` if the continuation was dimissed.
  c.state == Dismissed

macro cps*(T: typed, n: typed): untyped =
  ## This is the .cps. macro performing the proc transformation
  when defined(nimdoc):
    result = n
  else:
    result = cpsTransformProc(T, n)

    # we only support arc/orc due to its eager expr evaluation qualities
    when not(defined(gcArc) or defined(gcOrc)):
      once:
        result.add:
          nnkPragma.newTree:
            ident"warning".newColonExpr:
              newLit "cps supports --gc:arc or --gc:orc only; see https://github.com/nim-lang/Nim/issues/18099"

    # we only support panics because we don't want to run finally on defect
    when not defined(nimPanics):
      once:
        result.add:
          nnkPragma.newTree:
            ident"warning".newColonExpr:
              newLit "cps supports --panics:on only; see https://github.com/disruptek/cps/issues/110"

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

proc doWhelp(n: NimNode; args: seq[NimNode]): NimNode =
  for n in n.pragma.items:
    if n.kind == nnkExprColonExpr:
      if $n[0] == "cpsBootstrap":
        result = n[1].newCall args
  if result.isNil:
    result = n.errorAst "welping malfunction"

macro whelp*(n: typed): Continuation =
  ## Instantiate the given continuation call but do not begin
  ## running it; instead, return the continuation as a value.
  var n = normalizingRewrites n
  if n.kind in nnkCallKinds:
    let p = getImpl n[0]
    if p.hasPragma "cpsBootstrap":
      return doWhelp(p, n[1..^1])
  error "the input to whelp must be a .cps. call", n

template coop*(c: Continuation): Continuation {.used.} =
  ## This symbol may be reimplemented as a `.cpsMagic.` to introduce
  ## a cooperative yield at appropriate continuation exit points.
  c

template boot*(c: Continuation): Continuation {.used.} =
  ## This symbol may be reimplemented to refine a continuation after
  ## it has been allocated but before it is first run.
  c

template pass*(c: Continuation; to: Continuation): Continuation {.used.} =
  ## This symbol may be reimplemented to introduce logic during
  ## the calling of new child continuations; it will return the
  ## continuation to pass control to.
  to

template trace*(c: Continuation; fun: string; where: LineInfo) {.used.} =
  ## This symbol may be reimplemented to introduce control-flow
  ## tracing of the entry to each continuation leg.
  discard

template alloc*[T: Continuation](c: typedesc[T]): T {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## allocation.
  new c

template dealloc*[T: Continuation](t: typedesc[T];
                                   c: sink Continuation) {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## deallocation.
  discard
