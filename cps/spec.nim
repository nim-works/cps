##[

boring utilities likely useful to multiple pieces of cps machinery

]##

import std/[hashes, sequtils, deques]
import std/macros except newStmtList, newTree

when (NimMajor, NimMinor) < (1, 5):
  {.fatal: "requires nim-1.5".}

const
  cpsCallOperatorSupported* =
    when (NimMajor, NimMinor) < (1, 6):
      false
    elif (NimMajor, NimMinor) == (1, 6) and NimPatch < 11:
      false
    elif (NimMajor, NimMinor) == (1, 7) and NimPatch < 3:
      false
    else:
      true

import cps/[rewrites, help, normalizedast]
export errorAst, desym, isEmpty, genField

template cpsLift*() {.pragma.}          ## lift this proc|type
template cpsCall*() {.pragma.}          ## a cps call
template cpsMagicCall*() {.pragma.}     ## a magic call
template cpsVoodooCall*() {.pragma.}    ## a voodoo call
template cpsMustJump*() {.pragma.}      ## cps calls and magic calls jump
template cpsPending*() {.pragma.}       ## this is the last continuation
template cpsBreak*(label: typed = nil) {.pragma.} ##
## this is a break statement in a cps block
template cpsContinue*() {.pragma.}      ##
## this is a continue statement in a cps block
template cpsCont*() {.pragma.}          ## this is a continuation
template cpsBootstrap*(whelp: typed) {.pragma.}  ##
## the symbol for creating a continuation -- technically, a whelp()
template cpsCallback*() {.pragma.}          ## this is a callback typedef
template cpsCallbackShim*(whelp: typed) {.pragma.}  ##
## the symbol for creating a continuation which returns a continuation base
template cpsEnvironment*(tipe: typed) {.pragma.}  ##
## the environment type that composed the target
template cpsResult*(result: typed) {.pragma.}  ##
## the procedure that returns the result of the continuation
template cpsReturnType*(tipe: typed) {.pragma.}  ##
## the return type of the continuation
template cpsTerminate*() {.pragma.}     ## this is the end of this procedure
template cpsHasException*(cont, ex: typed) {.pragma.}  ##
## the continuation has an exception stored in `ex`, with `cont` being the
## continuation symbol used.

const
  cpsStackFrames* {.booldefine, used.} = compileOption"stacktrace"
  cpsTraceDeque* {.booldefine, used.} = compileOption"stacktrace"
  traceDequeSize* {.intdefine, used.} = 4_096

type
  ContinuationObj* = object of RootObj
    fn*: proc(c: sink Continuation): Continuation {.nimcall.} ##
    ## The `fn` points to the next continuation leg.
    mom*: Continuation  ##
    ## If this Continuation was invoked by another Continuation,
    ## the `mom` will hold that parent Continuation to form a
    ## linked-list approximating a stack.
    ex*: ref Exception ## The unhandled exception of the continuation.
    when cpsTraceDeque:
      frames*: Deque[TraceFrame]    ## Tracing for all prior hooks
    when cpsStackFrames:
      stack*: TraceFrame            ## Stack-like semantic record
  Continuation* = ref object of ContinuationObj

  ContinuationProc*[T] = proc(c: sink T): T {.nimcall.}

  Callback*[C; R; P] = object
    fn*: P                            ##
    ## the bootstrap for continuation C
    rs*: proc (c: sink C): R {.nimcall.}   ##
    ## the result fetcher for continuation C

  TraceFrame* = object ## a record of where the continuation has been
    hook*: Hook        ## the hook that provoked the trace entry
    fun*: string       ## a short label for the notable symbol
    info*: LineInfo    ## the source of the notable symbol

  Hook* = enum ##
    ## these are hook procedure names; the string value matches the name
    ## of the symbol we'll call to perform the hook.
    Coop    = "coop"      ## returns control to the dispatcher
    Trace   = "trace"     ## executed at entry to each continuation leg
    Alloc   = "alloc"     ## performs allocation of a new continuation
    Dealloc = "dealloc"   ## performs deallocation of a continuation
    Pass    = "pass"      ## transfers control-flow between continuations
    Boot    = "boot"      ## prepares a continuation for initial use
    Unwind  = "unwind"    ## controlled "bubble-up" for exception handling
    Head    = "head"      ## invoked when a new continuation has no parent
    Tail    = "tail"      ## invoked when a new continuation has a parent
    Stack   = "stack"     ## invoked to annotate stack semantics

proc `=copy`(dest: var ContinuationObj; src: ContinuationObj) {.error.} =
  discard

proc `=destroy`(dest: var ContinuationObj) =
  for key, value in dest.fieldPairs:
    reset value

template dot*(a, b: NimNode): NimNode =
  ## for constructing foo.bar
  newDotExpr(a, b)

template dot*(a: NimNode; b: string): NimNode =
  ## for constructing `.`(foo, "bar")
  dot(a, ident(b))

template eq*(a, b: NimNode): NimNode =
  ## for constructing foo=bar in a call
  nnkExprEqExpr.newNimNode(a).add(a).add(b)

template eq*(a: string; b: NimNode): NimNode =
  ## for constructing foo=bar in a call
  eq(ident(a), b)

template colon*(a, b: NimNode): NimNode =
  ## for constructing foo: bar in a ctor
  nnkExprColonExpr.newNimNode(a).add(a).add(b)

template colon*(a: string; b: NimNode): NimNode =
  ## for constructing foo: bar in a ctor
  colon(ident(a), b)

template colon*(a: string | NimNode; b: string | int): NimNode =
  ## for constructing foo: bar in a ctor
  colon(a, newLit(b))

proc filterPragma*(ns: seq[PragmaAtom], liftee: Name): NormNode =
  ## given a seq of pragmas, omit a match and return Pragma or Empty
  newPragmaStmt(filterIt(ns, it.getPragmaName != liftee))

proc stripPragma*(n: PragmaStmt, s: static[string]): PragmaStmt =
  ## filter a pragma with the matching name
  PragmaStmt filterPragma(toSeq items(asPragmaStmt(n)), bindName(s))

proc stripPragma*(n: NormNode; s: static[string]): NormNode =
  ## filter a pragma with the matching name from various nodes
  case n.kind
  of nnkPragma:
    result = filterPragma(toSeq items(asPragmaStmt(n)), bindName(s))
  of RoutineNodes:
    let n = asRoutineDef(n)
    n.pragma = stripPragma(n.pragma, s)
    result = n
  of nnkObjectTy:
    n[0] = filterPragma(toSeq items(asPragmaStmt(n[0])), bindName(s))
    result = n
  of nnkRefTy:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeDef:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeSection:
    result = NormNode newNimNode(n.kind, n)
    for item in items(n):
      result.add stripPragma(item, s)
  else:
    result = n

proc hash*(n: NimNode): Hash =
  ## Hash a NimNode via it's representation
  var h: Hash = 0
  h = h !& hash(repr n)
  result = !$h

func newCpsPending*(): PragmaStmt =
  ## Produce a {.cpsPending.} annotation
  newPragmaStmt(bindName"cpsPending")

func isCpsPending*(n: NormNode): bool =
  ## Return whether a node is a {.cpsPending.} annotation
  n.kind == nnkPragma and n.len == 1 and n.asPragmaStmt.hasPragma("cpsPending")

func newCpsBreak*(n: NormNode, label = newNilLit().NormNode): NormNode =
  ## Produce a {.cpsBreak.} annotation with the given label
  let label =
    if label.kind == nnkEmpty:
      newNilLit().NormNode
    else:
      label

  newPragmaStmtWithInfo(n, newPragmaColonExpr("cpsBreak", label))

proc isCpsBreak*(n: NormNode): bool =
  ## Return whether a node is a {.cpsBreak.} annotation
  n.kind == nnkPragma and n.len == 1 and asPragmaStmt(n).hasPragma("cpsBreak")

func newCpsContinue*(n: NormNode): NormNode =
  ## Produce a {.cpsContinue.} annotation
  newPragmaStmtWithInfo(n, asPragmaAtom(bindName"cpsContinue"))

func isCpsContinue*(n: NormNode): bool =
  ## Return whether a node is a {.cpsContinue.} annotation
  n.kind == nnkPragma and n.len == 1 and asPragmaStmt(n).hasPragma("cpsContinue")

proc breakLabel*(n: NormNode): NormNode =
  ## Return the break label of a `break` statement or a `cpsBreak` annotation
  if n.isCpsBreak():
    if n[0].len > 1 and n[0][1].kind != nnkNilLit:
      n[0][1]
    else:
      newEmptyNormNode()
  elif n.kind == nnkBreakStmt:
    n[0]
  else:
    raise newException(Defect, "this node is not a break: " & $n.kind)

proc isCpsCont*(n: NormNode): bool =
  ## Return whether the given procedure is a cps continuation
  n.kind in RoutineNodes and n.asRoutineDef.hasPragma("cpsCont")

proc getContSym*(n: NormNode): Name =
  ## Retrieve the continuation symbol from `n`, provided that
  ## `n` is a cpsCont.
  if n.isCpsCont:
    asRoutineDef(n).firstCallParam.name
  else:
    nil.Name

proc newCpsTerminate*(): NormNode =
  ## Create a new node signifying early termination of the procedure
  newPragmaStmt(bindName"cpsTerminate")

proc isCpsTerminate*(n: NormNode): bool =
  ## Return whether `n` is a cpsTerminate annotation
  n.kind == nnkPragma and n.len == 1 and asPragmaStmt(n).hasPragma("cpsTerminate")

proc isScopeExit*(n: NormNode): bool =
  ## Return whether the given node signify a CPS scope exit
  n.isCpsPending or n.isCpsBreak or n.isCpsContinue or n.isCpsTerminate

template rewriteIt*(n: typed; body: untyped): NormNode =
  var it {.inject.} = normalizingRewrites:
    macros.newStmtList n
  body
  workaroundRewrites it

template debugAnnotation*(s: typed; n: NimNode; body: untyped) {.dirty.} =
  debug(astToStr s, n, Original)
  result = rewriteIt n:
    body
  debug(astToStr s, result, Transformed, n)

func matchCpsBreak*(label: NormNode): NormMatcher =
  ## create a matcher matching cpsBreak with the given label
  ## and cpsBreak without any label
  result =
    func (n: NormNode): bool =
      if n.isCpsBreak:
        let breakLabel = n.breakLabel
        breakLabel.kind == nnkEmpty or breakLabel == label
      else:
        false

func matchCpsBreak*(): NormMatcher =
  ## create a matcher matching cpsBreak with an empty label
  matchCpsBreak(newEmptyNode().NormNode)

func wrappedFinally*(n, final: NormNode): NormNode =
  ## rewrite a try/except/finally into try/try-except/finally
  # create a copy of the try statement minus finally
  let newTry = copyNimNode(n).add(n[0 .. ^2])

  # wrap the try-finally outside of `nc`
  result = copyNimNode n
  result.add(newStmtList(newTry), final)

proc isVoodooCall*(n: NormNode): bool =
  ## true if this is a call to a voodoo procedure
  ifCallThenIt n:
    if it.hasImpl:
      result = it.impl.hasPragma "cpsVoodooCall"

proc isCpsCall*(n: NormNode): bool =
  ## true if this node holds a call to a cps procedure
  ifCallThenIt n:
    if it.hasImpl:
      # guard issuing hasPragma on a typedef in the case
      # where we're looking at a call such as MyType(foo).
      # XXX: we might actually need to unwrap the symbol
      #      recursively to ensure it doesn't ultimately
      #      represent a type...
      if it[0].kind != nnkSym or it[0].symKind != nskType:
        # what we're looking for here is a jumper; it could
        # be a magic or it could be another continuation leg
        # or it could be a completely new continuation
        result = it.impl.hasPragma "cpsMustJump"

proc isCpsConvCall*(n: NormNode): bool =
  ## true if this node holds a cps call that might be nested within one or more
  ## conversions.
  case n.kind
  of nnkConv:
    isCpsConvCall(n.last)
  else:
    isCpsCall(n)

proc isCpsBlock*(n: NormNode): bool =
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
  case n.kind
  of nnkForStmt, nnkBlockStmt, nnkBlockExpr, nnkElse, nnkElseExpr,
     nnkOfBranch, nnkExceptBranch, nnkFinally, ConvNodes, nnkExprColonExpr,
     nnkPragmaBlock, nnkIdentDefs, nnkVarSection, nnkLetSection,
     nnkDiscardStmt:
    return n.last.isCpsBlock
  of nnkStmtList, nnkStmtListExpr, nnkIfStmt, nnkIfExpr, nnkCaseStmt,
     nnkWhileStmt, nnkElifBranch, nnkElifExpr, nnkTryStmt, nnkAsgn,
     nnkVarTuple, AccessNodes - AtomicNodes, ConstructNodes:
    for n in n.items:
      if n.isCpsBlock:
        return true
  of CallNodes:
    if n.isCpsCall:
      return true

    for n in n.items:
      if n.isCpsBlock:
        return true
  else:
    return false

proc pragmaArgument*(n: NormNode; s: string): NormNode =
  ## from foo() or proc foo() {.some: Pragma.}, retrieve Pragma
  case n.kind
  of nnkProcDef:
    let p = asProcDef(n)
    for n in p.pragma.items:
      case n.kind
      of nnkExprColonExpr:
        if $n[0] == s:
          if result.isNil:
            result = n[1]
          else:
            result = n.errorAst "redundant " & s & " pragmas?"
      else:
        discard
    if result.isNil:
      result = n.errorAst "failed to find expected " & s & " form"
  of nnkCallKinds:
    result = pragmaArgument(asCall(n).impl, s)
  else:
    result = n.errorAst "unsupported pragmaArgument target: " & $n.kind

proc bootstrapSymbol*(n: NimNode): NormNode =
  ## find the return type of the bootstrap
  let n = NormNode n
  case n.kind
  of nnkCallKinds:
    bootstrapSymbol n[0]
  of nnkSym:
    bootstrapSymbol n.getImpl
  of nnkProcDef:
    if n.hasPragma "borrow":
      bootstrapSymbol n.last
    else:
      pragmaArgument(n, "cpsBootstrap")
  else:
    ## XXX: darn ambiguous calls
    normalizedast.newCall("typeOf", n)

proc enbasen*(n: NimNode): TypeExpr =
  ## find the parent type of the given symbol/type
  case n.kind
  of nnkOfInherit:
    TypeExpr n[0]
  of nnkObjectTy:
    enbasen: n[1]
  of nnkRefTy:
    enbasen: n[0]
  of nnkTypeDef:
    enbasen: n.last
  of nnkSym:
    enbasen: getImpl n
  else:
    TypeExpr n

type
  State* {.pure.} = enum
    ## Representation of the state of a continuation.
    Running    ## The continuation is active and running and can be resumed
    Dismissed  ## The continuation is currently somewhere else
    Finished   ## The continuation is finished and can no longer be resumed

proc makeErrorShim*(n: NimNode): NimNode =
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
  ##
  ## The target procedure of a cpsMagic pragma returns the `Continuation`
  ## to which control-flow should return; this is usually the same value
  ## passed into the procedure, but this is not required nor is it checked!
  expectKind(n, nnkProcDef)
  result = newStmtList n.NormNode       # preserve the original proc
  var shim = makeErrorShim n            # create the shim
  shim.params[0] = newEmptyNode()       # wipe out the return value

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
  result = newStmtList n.NormNode   # preserve the original proc
  var shim = makeErrorShim n        # create the shim

  # we use this pragma to identify the primitive and rewrite it inside
  # CPS so that it again binds to the version that takes a continuation.
  shim.addPragma ident"cpsVoodooCall"
  result.add shim

when cpsTraceDeque or cpsStackFrames:
  from std/strformat import `&`
when cpsStackFrames:
  from std/algorithm import reverse

proc renderStackFramesImpl(c: Continuation): seq[string] =
  if c.isNil:
    return @["dismissed continuations have no stack trace"]
  else:
    when not cpsStackFrames:
      return @["compile with --stackTrace:on or --define:cpsStackFrames=on"]
    else:
      var c = c
      while not c.isNil:
        template frame: TraceFrame = c.stack
        result.add:
          &"{frame.info.filename}({frame.info.line}) {frame.fun}"
        c = c.mom
      reverse result

proc renderStackFrames*(c: Continuation): seq[string] {.cpsVoodoo.} =
  ## Render a "stack" trace for the continuation as a sequence of lines.
  renderStackFramesImpl c

proc renderTraceDeque*(c: Continuation): seq[string] {.cpsVoodoo.} =
  ## Render a traceback for the continuation as a sequence of lines.
  if c.isNil:
    return @["dismissed continuations have no trace deque"]
  when not cpsTraceDeque:
    return @["compile with --stackTrace:on or --define:cpsTraceDeque=on"]
  else:
    for index in 0 ..< c.frames.len:
      template frame: TraceFrame = c.frames[c.frames.len - index - 1]
      result.add:
        &"{frame.info.filename}({frame.info.line}) {frame.fun} <{frame.hook}>"

proc writeStackFramesImpl(c: Continuation) =
  for line in c.renderStackFramesImpl.items:
    stdmsg().writeLine line

proc writeStackFrames*(c: Continuation) {.cpsVoodoo.} =
  ## Write a "stack" trace for the continuation.
  writeStackFramesImpl c

proc writeTraceDeque*(c: Continuation) {.cpsVoodoo.} =
  ## Write a traceback for the continuation.
  for line in c.renderTraceDeque.items:
    stdmsg().writeLine line

proc trampoline*[T: Continuation](c: sink T): T =
  ## This is the basic trampoline: it will run the continuation
  ## until the continuation is no longer in the `Running` state.
  var c: Continuation = move c
  while not c.isNil and not c.fn.isNil:
    try:
      var y = c.fn
      var x = y(c)
      c = x
    except Exception:
      if not c.dismissed:
        writeStackFramesImpl c
      raise
  result = T c

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
      var it {.used, inject.}: `T` = c
      `body`
      try:
        var y = c.fn
        var x = y(c)
        c = x
      except Exception:
        if not c.dismissed:
          writeStackFramesImpl c
        raise

proc ensimilate*(source, destination: NormNode): Call =
  ## perform a call to convert the destination to the source's type;
  ## the source can be any of a few usual suspects...
  let typ = TypeExpr getTypeImpl source
  block unfound:
    if typ.isNil:
      break unfound
    else:
      case typ.kind
      of nnkEmpty:
        break unfound
      of nnkProcTy:
        result = newCall(typ[0][0], destination)
      of nnkRefTy:
        result = newCall(typ[0], destination)
      elif typ.kind == nnkSym and $typ == "void":
        break unfound
      else:
        result = newCall(typ, destination)
      return

  # fallback to typeOf
  result = newCall(newCall(bindName"typeOf", source), destination)

proc nilAsEmpty*(n: NimNode): NimNode =
  ## normalize nil, nnkNilLit to nnkEmpty
  if n.isNil or n.kind == nnkNilLit:
    newEmptyNode()
  else:
    n

proc emptyAsNil*(n: NimNode): NimNode =
  ## normalize nil, nnkEmpty to nnkNilLit
  if n.isNil or n.kind == nnkEmpty:
    newNilLit()
  else:
    n

macro etype*(e: enum): string =
  ## Coop -> "Coop", not "coop"
  for sym in (getTypeImpl e)[1..^1]:
    if sym.intVal == e.intVal:
      return newLit sym.strVal
  error "unexpected"

proc copyOrVoid*(n: NimNode): NimNode =
  ## if the node is empty, `ident"void"`; else, a copy of the node
  if n.isEmpty:
    ident"void"
  else:
    copyNimTree n

proc createCallback*(sym: NimNode): NimNode =
  ## create a new Callback object construction
  let fn = sym.getImpl.ProcDef.pragmaArgument"cpsCallbackShim"
  let impl = fn.getImpl.ProcDef                     # convenience
  let rs = impl.pragmaArgument"cpsResult"
  let tipe = nnkBracketExpr.newTree bindSym"Callback"
  tipe.add impl.returnParam # the base cps environment type
  tipe.add:                 # the return type of the result fetcher
    copyOrVoid impl.pragmaArgument"cpsReturnType"
  var params = copyNimTree impl.formalParams # prepare params list
  # consider desym'ing foo(a: int; b = a) before deleting this loop
  for defs in impl.callingParams:
    params = desym(params, defs.name)
  tipe.add:      # the proc() type of the bootstrap
    nnkProcTy.newTree(params, nnkPragma.newTree ident"nimcall")
  result =
    NimNode:
      nnkObjConstr.newTree(tipe, "fn".colon fn.NimNode, "rs".colon rs.NimNode)

proc cpsCallbackTypeDef*(tipe: NimNode, n: NimNode): NimNode =
  ## looks like cpsTransformProc but applies to proc typedefs;
  ## this is where we create our calling convention concept
  let params = copyNimTree n[0]
  let r = copyOrVoid params[0]
  params[0] = tipe
  let p = nnkProcTy.newTree(params,
                            nnkPragma.newTree(ident"nimcall", bindSym"cpsCallback"))
  result = nnkBracketExpr.newTree(bindSym"Callback", tipe, r, p)
  result = workaroundRewrites result.NormNode

proc recover*[C, R, P](callback: Callback[C, R, P]; continuation: C): R =
  ## Using a `callback`, recover the `result` of the given `continuation`.
  ## This is equivalent to running `()` on a continuation which was
  ## created with `whelp` against a procedure call.
  ##
  ## If the continuation is in the `running` `State`, this operation will
  ## `trampoline` the continuation until it is `finished`. The `result`
  ## will then be recovered from the continuation environment.
  ##
  ## It is a `Defect` to attempt to recover the `result` of a `dismissed`
  ## `continuation`.
  callback.rs(continuation)

macro call*[C; R; P](callback: Callback[C, R, P]; arguments: varargs[typed]): C =
  ## Invoke a `callback` with the given `arguments`; returns a continuation.
  result = newCall(callback.dot ident"fn")
  for argument in arguments.items:
    result.add argument
