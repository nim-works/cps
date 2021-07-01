##[

boring utilities likely useful to multiple pieces of cps machinery

]##

import std/[hashes, sequtils, macros]

when (NimMajor, NimMinor) < (1, 5):
  {.fatal: "requires nim-1.5".}

import cps/[rewrites, help, normalizedast]
export errorAst, desym, isEmpty, genField

template cpsLift*() {.pragma.}          ## lift this proc|type
template cpsCall*() {.pragma.}          ## a cps call
template cpsMagicCall*() {.pragma.}     ## a cps call
template cpsVoodooCall*() {.pragma.}    ## a voodoo call
template cpsMustJump*() {.pragma.}      ## cps calls and magic calls jump
template cpsPending*() {.pragma.}       ## this is the last continuation
template cpsBreak*(label: typed = nil) {.pragma.} ##
## this is a break statement in a cps block
template cpsContinue*() {.pragma.}      ##
## this is a continue statement in a cps block
template cpsCont*() {.pragma.}          ## this is a continuation
template cpsBootstrap*(whelp: typed) {.pragma.}  ##
## the symbol for creating a continuation
template cpsEnvironment*(tipe: typed) {.pragma.}  ##
## the environment type that composed the target
template cpsTerminate*() {.pragma.}     ## this is the end of this procedure
template cpsHasException*(cont, ex: typed) {.pragma.}  ##
## the continuation has an exception stored in `ex`, with `cont` being the
## continuation symbol used.

type
  Continuation* = ref object of RootObj
    fn*: proc(c: Continuation): Continuation {.nimcall.} ##
    ## The `fn` points to the next continuation leg.
    mom*: Continuation  ##
    ## If this Continuation was invoked by another Continuation,
    ## the `mom` will hold that parent Continuation to form a
    ## linked-list approximating a stack.
    ex*: ref Exception ##
    ## The unhandled exception of the continuation.

  ContinuationProc*[T] = proc(c: T): T {.nimcall.}

const
  ConvNodes* = {nnkHiddenStdConv..nnkConv}
    ## Conversion nodes in typed AST

  AccessNodes* = AtomicNodes + {nnkDotExpr, nnkDerefExpr, nnkHiddenDeref,
                                nnkAddr, nnkHiddenAddr}
    ## AST nodes for operations accessing a resource

  ConstructNodes* = {nnkBracket, nnkObjConstr, nnkTupleConstr}
    ## AST nodes for construction operations

proc getPragmaName(n: NimNode): NimNode =
  ## retrieve the symbol/identifier from the child node of a nnkPragma
  case n.kind
  of nnkCall, nnkExprColonExpr:
    n[0]
  else:
    n

func hasPragma*(n: NimNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  case n.kind
  of nnkPragma:
    for p in n.items:
      # just skip ColonExprs, etc.
      result = p.getPragmaName.eqIdent s
      if result:
        break
  of RoutineNodes:
    result = hasPragma(n.pragma, s)
  of nnkObjectTy:
    result = hasPragma(n[0], s)
  of nnkRefTy:
    result = hasPragma(n.last, s)
  of nnkTypeDef:
    result = hasPragma(n.last, s)
  of nnkTypeSection:
    result = anyIt(toSeq items(n), hasPragma(it, s))
  else:
    result = false

proc filterPragma*(ns: seq[NimNode], liftee: NimNode): NimNode =
  ## given a seq of pragmas, omit a match and return Pragma or Empty
  var pragmas = nnkPragma.newNimNode
  for p in filterIt(ns, it.getPragmaName != liftee):
    pragmas.add p
    copyLineInfo(pragmas, p)
  if len(pragmas) > 0:
    pragmas
  else:
    newEmptyNode()

proc stripPragma*(n: NimNode; s: static[string]): NimNode =
  ## filter a pragma with the matching name from various nodes
  case n.kind
  of nnkPragma:
    result = filterPragma(toSeq n, bindSym(s))
  of RoutineNodes:
    n.pragma = stripPragma(n.pragma, s)
    result = n
  of nnkObjectTy:
    n[0] = filterPragma(toSeq n[0], bindSym(s))
    result = n
  of nnkRefTy:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeDef:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeSection:
    result = newNimNode(n.kind, n)
    for item in items(n):
      result.add stripPragma(item, s)
  else:
    result = n

proc hash*(n: NimNode): Hash =
  ## Hash a NimNode via it's representation
  var h: Hash = 0
  h = h !& hash(repr n)
  result = !$h

func newCpsPending*(): NimNode =
  ## Produce a {.cpsPending.} annotation
  nnkPragma.newTree:
    bindSym"cpsPending"

proc isCpsPending*(n: NimNode): bool =
  ## Return whether a node is a {.cpsPending.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsPending")

func newCpsBreak*(n: NimNode; label: NimNode = newNilLit()): NimNode =
  ## Produce a {.cpsBreak.} annotation with the given label
  let label =
    if label.kind == nnkEmpty:
      newNilLit()
    else:
      label

  nnkPragma.newNimNode(n).add:
    newColonExpr(bindSym"cpsBreak", label)

proc isCpsBreak*(n: NimNode): bool =
  ## Return whether a node is a {.cpsBreak.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsBreak")

func newCpsContinue*(n: NimNode): NimNode =
  ## Produce a {.cpsContinue.} annotation
  nnkPragma.newNimNode(n).add:
    bindSym"cpsContinue"

proc isCpsContinue*(n: NimNode): bool =
  ## Return whether a node is a {.cpsContinue.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsContinue")

proc breakLabel*(n: NimNode): NimNode =
  ## Return the break label of a `break` statement or a `cpsBreak` annotation
  if n.isCpsBreak():
    if n[0].len > 1 and n[0][1].kind != nnkNilLit:
      n[0][1]
    else:
      newEmptyNode()
  elif n.kind == nnkBreakStmt:
    n[0]
  else:
    raise newException(Defect, "this node is not a break: " & $n.kind)

proc isCpsCont*(n: NimNode): bool =
  ## Return whether the given procedure is a cps continuation
  n.kind in RoutineNodes and n.hasPragma("cpsCont")

proc getContSym*(n: NimNode): NimNode =
  ## Retrieve the continuation symbol from `n`, provided that
  ## `n` is a cpsCont.
  if n.isCpsCont:
    n.params[1][0]
  else:
    nil

proc newCpsTerminate*(): NimNode =
  ## Create a new node signifying early termination of the procedure
  nnkPragma.newTree:
    bindSym"cpsTerminate"

proc isCpsTerminate*(n: NimNode): bool =
  ## Return whether `n` is a cpsTerminate annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsTerminate")

proc isScopeExit*(n: NimNode): bool =
  ## Return whether the given node signify a CPS scope exit
  n.isCpsPending or n.isCpsBreak or n.isCpsContinue or n.isCpsTerminate

template rewriteIt*(n: typed; body: untyped): NimNode =
  var it {.inject.} = normalizingRewrites:
    newStmtList n
  body
  workaroundRewrites it

template debugAnnotation*(s: typed; n: NimNode; body: untyped) {.dirty.} =
  debug(astToStr s, n, Original)
  result = rewriteIt n:
    body
  debug(astToStr s, result, Transformed, n)

func matchCpsBreak*(label: NimNode): Matcher =
  ## create a matcher matching cpsBreak with the given label
  ## and cpsBreak without any label
  result =
    proc (n: NimNode): bool =
      if n.isCpsBreak:
        let breakLabel = n.breakLabel
        breakLabel.kind == nnkEmpty or breakLabel == label
      else:
        false

func wrappedFinally*(n: NimNode; final: NimNode): NimNode =
  ## rewrite a try/except/finally into try/try-except/finally
  # create a copy of the try statement minus finally
  let newTry = copyNimNode(n).add n[0 .. ^2]

  # wrap the try-finally outside of `nc`
  result = copyNimNode n
  result.add newStmtList(newTry)
  result.add final

proc isVoodooCall*(n: NimNode): bool =
  ## true if this is a call to a voodoo procedure
  if not n.isNil and n.len > 0:
    if n.kind in nnkCallKinds:
      let callee = n[0]
      if not callee.isNil and callee.kind == nnkSym:
        result = callee.getImpl.hasPragma "cpsVoodooCall"

proc trampoline*[T: Continuation](c: T): T =
  ## This is the basic trampoline: it will run the continuation
  ## until the continuation is no longer in the `Running` state.
  var c: Continuation = c
  while not c.isNil and not c.fn.isNil:
    c = c.fn(c)
  result = T c

proc isCpsCall*(n: NimNode): bool =
  ## true if this node holds a call to a cps procedure
  if n.len > 0:
    if n.kind in CallNodes:
      let callee = n[0]
      if not callee.isNil and callee.kind == nnkSym:
        # what we're looking for here is a jumper; it could
        # be a magic or it could be another continuation leg
        # or it could be a completely new continuation
        result = callee.getImpl.hasPragma("cpsMustJump")

proc isCpsBlock*(n: NimNode): bool =
  ## `true` if the block `n` contains a cps call anywhere at all;
  ## this is used to figure out if a block needs tailcall handling...
  case n.kind
  of nnkForStmt, nnkBlockStmt, nnkBlockExpr, nnkElse, nnkElseExpr,
     nnkOfBranch, nnkExceptBranch, nnkFinally, ConvNodes, nnkExprColonExpr,
     nnkPragmaBlock, nnkIdentDefs, nnkVarSection, nnkLetSection:
    return n.last.isCpsBlock
  of nnkStmtList, nnkStmtListExpr, nnkIfStmt, nnkIfExpr, nnkCaseStmt,
     nnkWhileStmt, nnkElifBranch, nnkElifExpr, nnkTryStmt, nnkBracket,
     nnkTupleConstr, nnkObjConstr, nnkAsgn, nnkVarTuple:
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

proc pragmaArgument*(n: NimNode; s: string): NimNode =
  ## from foo() or proc foo() {.some: Pragma.}, retrieve Pragma
  case n.kind
  of nnkProcDef:
    for n in n.pragma.items:
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
    result = pragmaArgument(getImpl n[0], s)
  else:
    result = n.errorAst "unsupported pragmaArgument target: " & $n.kind

proc bootstrapSymbol*(n: NimNode): NimNode =
  ## find the return type of the bootstrap
  case n.kind
  of {nnkProcDef} + nnkCallKinds:
    pragmaArgument(n, "cpsBootstrap")
  else:
    newCall(ident"typeOf", n)

proc enbasen*(n: NimNode): NimNode =
  ## find the parent type of the given symbol/type
  case n.kind
  of nnkOfInherit:
    n[0]
  of nnkObjectTy:
    enbasen: n[1]
  of nnkRefTy:
    enbasen: n[0]
  of nnkTypeDef:
    enbasen: n.last
  of nnkSym:
    enbasen: getImpl n
  else:
    n

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
  ## to which control-flow should return; this is _usually_ the same value
  ## passed into the procedure, but this is not required nor is it checked!
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
