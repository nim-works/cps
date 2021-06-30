import std/[macros]
import cps/[spec, transform, rewrites, hooks, exprs]
export Continuation, ContinuationProc, State
export cpsCall, cpsMagicCall, cpsVoodooCall, cpsMustJump

# exporting some symbols that we had to bury for bindSym reasons
from cps/returns import pass
export pass, trampoline

# we only support arc/orc due to its eager expr evaluation qualities
when not(defined(gcArc) or defined(gcOrc)):
  {.warning: "cps supports --gc:arc or --gc:orc only; " &
             "see https://github.com/nim-lang/Nim/issues/18099".}

# we only support panics because we don't want to run finally on defect
when not defined(nimPanics):
  {.warning: "cps supports --panics:on only; " &
             " see https://github.com/disruptek/cps/issues/110".}

proc state*(c: Continuation): State =
  ## Get the current state of a continuation
  if c == nil:
    State.Dismissed
  elif c.fn == nil:
    State.Finished
  else:
    State.Running

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

template running*(c: Continuation): bool =
  ## `true` if the continuation is running.
  (Continuation c).state == State.Running

template finished*(c: Continuation): bool =
  ## `true` if the continuation is finished.
  (Continuation c).state == State.Finished

template dismissed*(c: Continuation): bool =
  ## `true` if the continuation was dimissed.
  (Continuation c).state == State.Dismissed

{.pop.}

macro trampolineIt*[T: Continuation](supplied: T; body: untyped) =
  ## This trampoline allows the user to interact with the continuation
  ## prior to each leg of its execution.  The continuation will be
  ## exposed by a variable named `it` inside the `body`.
  #
  # this is a lame workaround for the fact that the compiler pukes
  # on the conversions in the template version...
  result = quote:
    var c: Continuation = `supplied`
    while c.running:
      var it {.inject.}: `T` = c
      `body`
      c = c.fn(c)

macro cps*(T: typed, n: typed): untyped =
  ## This is the .cps. macro performing the proc transformation
  when defined(nimdoc):
    n
  else:
    case n.kind
    of nnkProcDef:
      # Typically we would add these as pragmas, however it appears
      # that the compiler will run through macros in proc pragmas
      # one-by-one without re-seming the body in between...
      {.warning: "compiler bug workaround, see: https://github.com/nim-lang/Nim/issues/18349".}
      result =
        # Add the main transform phase
        newCall(bindSym"cpsTransform", T):
          # Add the flattening phase which will be run first
          newCall(bindSym"cpsFlattenExpr"):
            n
    else:
      result = getAst(cpsTransform(T, n))

proc makeErrorShim(n: NimNode): NimNode =
  ## Upgrades a procedure to serve as a CPS primitive, generating
  ## errors out of `.cps.` context and taking continuations as input.
  expectKind(n, nnkProcDef)

  # Create a version of the proc that lacks a first argument or return
  # value.  While this version will throw an exception at runtime, it
  # may be used inside CPS as magic(); for better programmer ergonomics.
  var shim = copyNimTree n
  del(shim.params, 1)               # delete the 1st Continuation argument
  let msg = newLit($n.name & "() is only valid in {.cps.} context")
  shim.body =                       # raise a defect when invoked directly
    quote:
      raise Defect.newException: `msg`
  result = shim

macro cpsMagic*(n: untyped): untyped =
  ## Applied to a procedure to generate a version which lacks the first
  ## argument and return value, which are those of a `Continuation`.
  ##
  ## This new magical will compile correctly inside CPS procedures though
  ## it never takes a `Continuation` argument and produces no return value.
  expectKind(n, nnkProcDef)
  result = newStmtList n            # preserve the original proc
  var shim = makeErrorShim n        # create the shim
  shim.params[0] = newEmptyNode()   # wipe out the return value

  # we use these pragmas to identify the primitive and rewrite it inside
  # CPS so that it again binds to the version that takes and returns a
  # continuation.
  shim.addPragma ident"cpsMustJump"
  shim.addPragma ident"cpsMagicCall"
  result.add shim

macro cpsVoodoo*(n: untyped): untyped =
  ## Similar to a `cpsMagic` where the first argument is concerned, but
  ## may specify a return value which is usable inside the CPS procedure.
  expectKind(n, nnkProcDef)
  result = newStmtList n            # preserve the original proc
  var shim = makeErrorShim n        # create the shim

  # we use this pragma to identify the primitive and rewrite it inside
  # CPS so that it again binds to the version that takes a continuation.
  shim.addPragma ident"cpsVoodooCall"
  result.add shim

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

macro whelp*(call: typed): untyped =
  ## Instantiate the given continuation call but do not begin
  ## running it; instead, return the continuation as a value.
  let sym = bootstrapSymbol call
  let base = enbasen:  # find the parent type of the environment
    (getImpl sym).pragmaArgument"cpsEnvironment"
  result = whelpIt call:
    it =
      sym.ensimilate:
        Head.hook:
          newCall(base, it)

macro whelp*(parent: Continuation; call: typed): untyped =
  ## As in `whelp(call(...))`, but also links the new continuation to the
  ## supplied parent for the purposes of exception handling and similar.
  let sym = bootstrapSymbol call
  let base = enbasen:  # find the parent type of the environment
    (getImpl sym).pragmaArgument"cpsEnvironment"
  result = whelpIt call:
    it =
      sym.ensimilate:
        Tail.hook(newCall(ident"Continuation", parent),
                  newCall(base, it))

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

template trace*(c: Continuation; fun: string; where: LineInfo) {.used.} =
  ## This symbol may be reimplemented to introduce control-flow
  ## tracing of the entry to each continuation leg.
  discard

proc alloc*[T: Continuation](root: typedesc[T]; c: typedesc): c {.used, inline.} =
  ## This symbol may be reimplemented to customize continuation
  ## allocation.
  new c

template dealloc*[T: Continuation](t: typedesc[T];
                                   c: sink Continuation) {.used.} =
  ## This symbol may be reimplemented to customize continuation
  ## deallocation.
  discard

{.push experimental: "callOperator".}
template `()`(c: Continuation): untyped {.used.} =
  ## Returns the result, i.e. the return value, of a continuation.
  discard
{.pop.}
