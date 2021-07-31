import std/[genasts, deques]
import cps/[spec, transform, rewrites, hooks, exprs, normalizedast]
import std/macros except newStmtList, newTree
export Continuation, ContinuationProc, State
export cpsCall, cpsMagicCall, cpsVoodooCall, cpsMustJump, cpsMagic

# exporting some symbols that we had to bury for bindSym reasons
from cps/returns import pass
export pass, trampoline, unwind

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

proc doWhelp(n: NormNode; args: seq[NormNode]): Call =
  let sym = bootstrapSymbol n
  result = sym.newCall args

template whelpIt*(input: typed; body: untyped): untyped =
  var n = normalizeCall input
  if n.kind in nnkCallKinds:
    let p = asCallKind(n).impl
    if p.hasPragma "cpsBootstrap":
      var it {.inject.} = doWhelp(p, n[1..^1])
      body
      NimNode it
    else:
      n.errorAst "the input to whelpIt must be a .cps. call"
  else:
    n.errorAst "the input to whelpIt must be a .cps. call"

macro whelp*(call: typed): untyped =
  ## Instantiate the given continuation call but do not begin
  ## running it; instead, return the continuation as a value.
  let
    sym = bootstrapSymbol call
    base = enbasen:  # find the parent type of the environment
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
        Tail.hook(newCall(ident"Continuation", parent).NormNode,
                  newCall(base, it))

template head*[T: Continuation](first: T): T {.used.} =
  ## Reimplement this symbol to configure a continuation
  ## for use when there is no parent continuation available.
  ## The return value specifies the continuation.
  first

proc tail*[T: Continuation](parent: Continuation; child: T): T {.used, inline.} =
  ## Reimplement this symbol to configure a continuation for
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
  ## Reimplement this symbol as a `.cpsMagic.` to introduce
  ## a cooperative yield at appropriate continuation exit points.
  ## The return value specifies the continuation.
  c

template boot*[T: Continuation](c: T): T {.used.} =
  ## Reimplement this symbol to refine a continuation after
  ## it has been allocated but before it is first run.
  ## The return value specifies the continuation.
  c

{.experimental: "dynamicBindSym".}
proc initFrame(hook: Hook; fun: string; info: LineInfo): NimNode =
  ## prepare a tracing frame constructor
  result = nnkObjConstr.newTree bindSym"TraceFrame"
  result.add: "hook".colon newCall(bindSym"Hook", hook.ord.newLit)
  result.add: "info".colon info.makeLineInfo
  result.add: "fun".colon fun

proc addFrame(c: NimNode; frame: NimNode): NimNode =
  ## add a frame object to the deque
  genAst(c, frame):
    if not c.isNil:
      while c.frames.len >= cpsTraceDequeSize:
        discard popLast(c.frames)
      addFirst(c.frames, frame)

proc cpsTraceDeque*(hook: Hook; c, n: NimNode; fun: string;
                    info: LineInfo, body: NimNode): NimNode {.used.} =
  ## This is the default tracing implementation which can be
  ## reused when implementing your own `trace` macros.
  let frame = initFrame(hook, fun, info)
  case hook
  of Trace:
    addFrame(c, frame)
  of Pass, Tail:
    newStmtList(addFrame(c, frame), addFrame(body, frame))
  else:
    addFrame(body, frame)

proc looksLegit(n: NimNode): bool =
  case n.kind
  of nnkNilLit: false
  of nnkEmpty: false
  of nnkSym: n.repr != "nil"
  else: true

macro trace*[T](hook: static[Hook]; source, target: typed;
                fun: string; info: LineInfo; body: T): untyped {.used.} =
  ## Reimplement this symbol to introduce control-flow tracing of each
  ## hook and entry to each continuation leg. The `fun` argument holds a
  ## simple stringification of the `target` that emitted the trace, while
  ## `target` holds the symbol itself.

  ## The `source` argument varies with the hook; for `Pass`, `Tail`,
  ## `Unwind`, and `Trace` hooks, it will represent a source continuation.
  ## Its value will be `nil` for `Boot`, `Coop`, and `Head` hooks, as
  ## these hooks operate on a single target continuation.

  ## The `Alloc` hook takes a user's typedesc and a CPS environment type
  ## as arguments, while the `Dealloc` hook takes as arguments the live
  ## continuation to deallocate and its CPS environment type.

  ## The second argument to the `Unwind` hook is the user's typedesc.

  ## The `trace` macro receives the _output_ of the traced hook as its
  ## `body` argument.

  var body = body
  result = newStmtList()
  if cpsHasTraceDeque:
    var tipe = getTypeInst body
    if tipe.looksLegit:
      let continuation = nskLet.genSym"continuation"
      result.add:
        # assign the input to a variable that can be repeated evaluated
        nnkLetSection.newTree:
          nnkIdentDefs.newTree(continuation, tipe, body)
      body = continuation
    result.add:
      # pass that input to the trace along with the other params
      cpsTraceDeque(hook, source, target, fun = fun.strVal,
                    info = info.makeLineInfo, body)
  result.add:
    # the final result of the statement list is the input
    body.nilAsEmpty

proc alloc*[T: Continuation](U: typedesc[T]; E: typedesc): E {.used, inline.} =
  ## Reimplement this symbol to customize continuation allocation; `U`
  ## is the type supplied by the user as the `cps` macro argument,
  ## while `E` is the type of the environment composed for the specific
  ## continuation.
  new E

proc dealloc*[T: Continuation](c: sink T; E: typedesc[T]): E {.used, inline.} =
  ## Reimplement this symbol to customize continuation deallocation;
  ## `c` is the continuation to be deallocated, while `E` is the type of
  ## its environment.  This procedure should generally return `nil`, as
  ## its result may be assigned to another continuation reference.
  nil

{.push experimental: "callOperator".}
template `()`(c: Continuation): untyped {.used.} =
  ## Returns the result, i.e. the return value, of a continuation.
  discard
{.pop.}
