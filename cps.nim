import std/[genasts, deques]
import cps/[spec, transform, rewrites, hooks, exprs, normalizedast]
import std/macros except newStmtList, newTree

export Continuation, ContinuationProc, State, cpsCallOperatorSupported
export cpsCall, cpsMagicCall, cpsVoodooCall, cpsMustJump
export cpsMagic, cpsVoodoo, trampoline, trampolineIt, call, recover
export writeStackFrames, writeTraceDeque
export renderStackFrames, renderTraceDeque

# exporting some symbols that we had to bury for bindSym reasons
from cps/returns import pass, dismiss
export pass, unwind, dismiss

# we only support arc/orc due to its eager expr evaluation qualities
when not(defined(gcArc) or defined(gcOrc)):
  {.warning: "cps supports --gc:arc or --gc:orc only; " &
             "see https://github.com/nim-lang/Nim/issues/18099".}

# we only support panics because we don't want to run finally on defect
when not defined(nimPanics):
  {.warning: "cps supports --panics:on only; " &
             " see https://github.com/nim-works/cps/issues/110".}

when (NimMajor, NimMinor, NimPatch) < (1, 7, 3):
  # we recommend against threads:on without define:useMalloc
  when not defined(useMalloc) and compileOption"threads":
    {.warning:
      "cps recommends against --threads:on without --define:useMalloc".}

proc state*(c: Continuation): State {.inline.} =
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
  (when c is Continuation: c else: Continuation c).state == State.Running

template finished*(c: Continuation): bool =
  ## `true` if the continuation is finished.
  (when c is Continuation: c else: Continuation c).state == State.Finished

template dismissed*(c: Continuation): bool =
  ## `true` if the continuation was dimissed.
  (when c is Continuation: c else: Continuation c).state == State.Dismissed

{.pop.}

macro cpsTyped(tipe: typed, n: typed): untyped =
  ## This is the typed CPS transformation pass which follows the untyped pass below.
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
        newCall(bindSym"cpsTransform", tipe):
          # Add the flattening phase which will be run first
          newCall(bindSym"cpsFlattenExpr"):
            n
    of nnkProcTy:
      # converting a cps callback
      result = cpsCallbackTypeDef(tipe, n)
    else:
      result = getAst(cpsTransform(tipe, n))

macro cps*(tipe: typed, n: untyped): untyped =
  ## When applied to a procedure, rewrites the procedure into a
  ## continuation form. When applied to a procedure type definition,
  ## rewrites the type into a callback form.
  result = n
  when not defined(nimdoc):
    # add the application of the typed transformation pass
    var n = newCall(bindSym"cpsTyped", tipe):
      n
    # let the untyped pass do what it will with this input
    # XXX: currently disabled because it's a slipperly slope of regret
    #result = performUntypedPass(T, n)
    result = n

proc adaptArguments(sym: NormNode; args: seq[NormNode]): seq[NormNode] =
  ## convert any arguments in the list as necessary to match those of
  ## the provided callable symbol.
  var i = 0
  for n in sym.getImpl.asProcDef.callingParams:
    result.add:
      if sameType(n.typ, args[i].getTypeInst):
        args[i]
      else:
        newCall(n.typ, args[i])
    inc i

proc doWhelp(n: NormNode; args: seq[NormNode]): Call =
  let sym = bootstrapSymbol n
  # convert arguments to the bootstrap's as necessary, (think: .borrow.)
  let args = adaptArguments(sym, args)
  result = sym.newCall args

template whelpIt*(input: typed; body: untyped): untyped =
  ## Instantiate the given continuation call and inject `it` in the body.
  var n = normalizeCall input
  if n.kind in NormalCallNodes:
    var it {.inject.} = doWhelp(n[0], n[1..^1])
    body
    NimNode it
  else:
    n.errorAst "the input to whelpIt must be a .cps. call"

template wrapWhelpIt(call: typed; logic: untyped): untyped =
  ## discover the bootstrap symbol `sym` and parent environment type `base`
  ## and inject them into the `logic`, which is passed as the `body` argument
  ## of a whelpIt `call` ...  get it?
  let sym {.inject.} = bootstrapSymbol call
  if sym.kind != nnkSym:
    # assume it's an error of some sort and pass it through
    NimNode sym
  else:
    let base {.inject.} =
      enbasen:  # find the parent type of the environment
        (getImpl sym).pragmaArgument"cpsEnvironment"
    whelpIt call:
      logic

macro whelp*(call: typed): untyped =
  ## Instantiate the given continuation call but do not begin running it;
  ## instead, return the continuation as a value.
  ##
  ## If you pass `whelp` a continuation procedure *symbol* instead, the
  ## result is a `Callback` which you can use to create many individual
  ## continuations or recover the `result` of an extant continuation.
  result =
    case call.kind
    of nnkSym:
      createCallback call
    else:
      wrapWhelpIt call:
        it =
          sym.ensimilate:
            Head.hook:
              newCall(base, it)

macro whelp*(parent: Continuation; call: typed): untyped =
  ## As in `whelp(call(...))`, but also links the new continuation to the
  ## supplied parent for the purposes of exception handling and similar.
  wrapWhelpIt call:
    it =
      sym.ensimilate:
        Tail.hook(parent.NormNode, newCall(base, it))

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

proc addFrame(continuation: NimNode; frame: NimNode): NimNode =
  ## add `frame` to `continuation`'s `.frames` dequeue
  genAst(frame, c = continuation):
    if not c.isNil:
      while len(c.frames) >= traceDequeSize:
        discard popLast(c.frames)
      addFirst(c.frames, frame)

proc addToContinuations(frame: NimNode; conts: varargs[NimNode]): NimNode =
  ## add `frame` to the `.frames` deque of `conts` continuations
  result = newStmtList()
  for c in conts.items:
    result.add:
      c.addFrame frame

proc traceDeque*(hook: Hook; c, n: NimNode; fun: string;
                 info: LineInfo, body: NimNode): NimNode {.used.} =
  ## This is the default tracing implementation which can be
  ## reused when implementing your own `trace` macros.
  initFrame(hook, fun, info).
    addToContinuations:
      case hook
      of Trace:        @[c]
      of Pass, Tail:   @[c, body]
      else:            @[body]

template isNilOrVoid(n: NimNode): bool =
  ## true if the node, a type, is nil|void;
  ##
  ## NOTE: if you name your type `nil`, you deserve what you get
  case n.kind
  of nnkNilLit, nnkEmpty:    true
  of nnkSym, nnkIdent:       n.repr == "nil"
  else:                      false

template capturedContinuation(result, c, body: untyped): untyped {.used.} =
  ## if the input node `c` appears to be typed as not nil|empty,
  ## then produce a pattern where this expression is stashed in
  ## a temporary and injected into the body before being assigned
  ## to the first argument. this work is stored in `result`.
  let tipe = getTypeInst c
  if not tipe.isNilOrVoid:
    # assign the continuation to a variable to prevent re-evaluation
    let continuation {.inject.} = nskLet.genSym"continuation"
    result.add:
      # assign the input to a variable that can be repeated evaluated
      nnkLetSection.newTree:
        nnkIdentDefs.newTree(continuation, tipe, c)
    body
    # use the `continuation` variable to prevent re-evaluation of `c`
    c = continuation

macro stack*[T: Continuation](frame: TraceFrame; target: T): T {.used.} =
  ## Reimplement this symbol to alter the recording of "stack" frames.
  ## The return value evaluates to the continuation.
  result = newStmtList()
  var target = target
  when cpsStackFrames:
    result.capturedContinuation target:
      result.add:
        # assign the frame to the continuation's "stack"
        newAssignment(continuation.dot "stack", frame)
  # the final result is the input continuation
  result.add target

macro trace*(hook: static[Hook]; source, target: typed;
             fun: string; info: LineInfo; body: typed): untyped {.used.} =
  ## Reimplement this symbol to introduce control-flow tracing of each
  ## hook and entry to each continuation leg. The `fun` argument holds a
  ## simple stringification of the `target` that emitted the trace, while
  ## `target` holds the symbol itself.

  ## The `source` argument varies with the hook; for `Pass`, `Tail`,
  ## `Unwind`, and `Trace` hooks, it will represent a source continuation.
  ## Its value will be `nil` for `Boot`, `Coop`, and `Head` hooks, as
  ## these hooks operate on a single target continuation.

  ## The second argument to the `Unwind` hook is the user's typedesc.

  ## The `Alloc` hook takes a user's typedesc and a CPS environment type
  ## as arguments, while the `Dealloc` hook takes as arguments the live
  ## continuation to deallocate and its CPS environment type.

  ## The `Stack` hook takes a `frame` object and a `target` continuation
  ## in which to record the frame.

  ## The `trace` macro receives the _output_ of the traced hook as its
  ## `body` argument.

  result = newStmtList()
  var body = body
  when cpsTraceDeque:
    result.capturedContinuation body:
      result.add:
        # pass the continuation to the trace along with the other params
        traceDeque(hook, source, target, fun = fun.strVal,
                   info = info.makeLineInfo, continuation)
  result.add:
    # the final result of the statement list is the input
    body.nilAsEmpty

proc disarm*[T: Continuation](c: T) {.used, inline.} =
  ## Reimplement this symbol to customize preparation of a continuation
  ## for deallocation.
  if c.dismissed:
    raise Defect.newException "disarm called upon dismissed continuation"
  else:
    c.mom = nil

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
  disarm c

template recover*(c: Continuation): untyped {.used.} =
  ## Returns the result, i.e. the return value, of a continuation.
  discard

when cpsCallOperatorSupported and not defined cpsNoCallOperator:
  {.push experimental: "callOperator".}
  macro `()`*[C; R; P](callback: Callback[C, R, P]; arguments: varargs[typed]): R =
    ## Allows for natural use of call syntax to invoke a callback and
    ## recover its result in a single statement, inside a continuation.
    let call = macros.newCall(macros.bindSym"call", callback)
    for argument in arguments.items:
      call.add argument
    let mutable = genSymVar("continuation", callback.NormNode).NimNode
    result = macros.newStmtList()
    result.add:
      macros.newTree nnkVarSection:
        macros.newTree(nnkIdentDefs, mutable, newEmptyNode(), call)
    result.add:
      macros.newCall(macros.bindSym"recover", callback, mutable)
  {.pop.}
