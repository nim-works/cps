##[

An attempt to collect all the callback code in one place.

NOTE: currently, cps/rewrites defines `isCallback(NimNode): bool`

]##
import std/macros

import cps/[spec, rewrites, environment]
import cps/normalizedast except newTree, newStmtList

template cpsCallback*() {.pragma.}          ## this is a callback typedef
template cpsCallbackShim*(whelp: typed) {.pragma.}  ##
## the symbol for creating a continuation which returns a continuation base
template cpsCallbackRecovery*(base: typed) {.pragma.}  ##
## a block that can be abbreviated to a child continuation call inside cps

type
  Callback*[C; R; P] = object
    fn*: P                            ##
    ## the bootstrap for continuation C
    rs*: proc (c: var C): R {.nimcall.}   ##
    ## the result fetcher for continuation C

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

proc createCallbackShim*(env: Env; whelp: ProcDef): ProcDef =
  ## this is a version of whelp that returns the base continuation type
  result = clone(whelp, newStmtList())
  result.returnParam = env.inherits
  result.name = genProcName(procedure env, "callback", info=whelp)
  # whelp_234(a, b, c)
  result.body = newCall whelp.name
  for defs in result.callingParams:
    result.body.add defs.name
  # C: whelp_234(a, b, c)
  result.body = newCall(result.returnParam, result.body)

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

proc createCastCallback*(whelp, callback, sym: NimNode): NimNode =
  ## Given a `callback` typedesc and a CPS continuation procedure,
  ## apply a (proc ()) type specifier to help disambiguate overloads.
  let tipe = getImpl(callback)[2]  # recover bootstrap proc type
  when not defined(isNimSkull):
    # erase the pragma so it doesn't blow old nim's mind
    tipe[1] = nnkPragma.newTree()
  result = newCall(whelp, newCall(tipe, sym))

proc recover*[C, R, P](callback: Callback[C, R, P]; continuation: var C): R =
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

when cpsCallOperatorSupported and not defined cpsNoCallOperator:
  {.push experimental: "callOperator".}
  macro `()`*[C; R; P](callback: Callback[C, R, P]; arguments: varargs[typed]): R =
    ## Allows for natural use of call syntax to invoke a callback and
    ## recover its result in a single expression.
    let call = newCall(bindSym"call", callback)
    for argument in arguments.items:
      call.add argument
    let mutable = genSymVar("callback_continuation", callback.NormNode).NimNode
    result = newStmtList()
    result.add:
      newTree nnkVarSection:
        newTree(nnkIdentDefs, mutable, newEmptyNode(), call)
    result.add:
      newCall(bindSym"recover", callback, mutable)
    var cbr = newCall(bindSym"cpsCallbackRecovery", getTypeInst C)
    cbr = nnkPragma.newTree(cbr)
    result = nnkPragmaBlock.newTree(cbr, result)
  {.pop.}

proc isCallbackRecovery*(n: NimNode): bool =
  ## the node appears to be a {.cpsCallbackRecovery.} pragma block
  if n.isNil: return false
  case n.kind
  of nnkPragmaBlock:
    n.len > 0 and n[0].isCallbackRecovery
  of nnkPragma:
    n.len > 0 and n[0].isCallbackRecovery
  of nnkCall:
    n.len > 0 and n[0].isCallbackRecovery
  of nnkSym:
    n.strVal == "cpsCallbackRecovery"
  else:
    false

proc baseContinuationType*(n: NimNode): NimNode =
  ## given a callable symbol presumed to be a callback,
  ## recover the (base) continuation return type of the proc.
  case n.kind
  of nnkDotExpr:
    # continuationEnvironment.callbackLocal.fn(arguments...)
    if n.len > 0 and n[0].kind == nnkDotExpr:
      let fun = n.last.getTypeImpl   # proctype from first object record (fn)
      result = fun[0][0]             # recover proc return type
  elif not n.isCallback:
    raise Defect.newException "callable is not a cps callback"
  else:
    discard
  if result.isNil:
    raise Defect.newException "unable to recover base type from callback"

proc setupCallbackChild*(env: var Env; call: Call): (Name, TypeExpr) =
  ## create a new child continuation variable to receive the result of
  ## the callback and add it to the environment.  return the child's
  ## symbol along with the base continuation type of the child.
  let ctype = baseContinuationType(call[0].NimNode).TypeExpr
  let child = genSymVar("callbackChild", info = call)
  env.localSection newIdentDef(child, ctype)
  result = (child, ctype)

when false:
  macro naturalize(kind: static[NimNodeKind]; callback: typed;
                   args: varargs[untyped]): untyped =
    ## perform a conditional typed rewrite for natural callback syntax inside cps
    if callback.looksLikeCallback:
      # convert it to callback.call(...)
      result = macros.newTree(kind, newDotExpr(callback, bindSym"call"))
      for arg in args.items:
        result.add arg
      # wrap that in recover(callback, ...)
      result = newCall(bindSym"recover", callback, result)
    else:
      result = kind.newTree(desym callback)
      for arg in args.items:
        result.add arg

  proc unwrapAnyDotExpr(n: NimNode): seq[NimNode] =
    ## turn a caller like foo.inc into @[inc, foo] so that we can flatten/reorder
    ## arguments correctly
    case n.kind
    of nnkDotExpr:
      @[n[1], n[0]]
    else:
      @[n]

  proc rewriteCalls(n: NimNode): NimNode =
    ## rewriting `callback(x)` into `recover(callback, call(callback, x))` for use
    ## inside of an untyped pass; this should be applied only to Callback symbols...
    proc recall(n: NimNode): NimNode =
      case n.kind
      of CallNodes:
        result = newCall(bindSym"naturalize", newLit(n.kind))
        result.add unwrapAnyDotExpr(n[0])  # help foo.inc(...) into inc(foo, ...)
        result.add n[1..^1]
      else:
        discard
    result = filter(n, recall)

  proc performUntypedPass(tipe: NimNode; n: NimNode): NimNode =
    ## Perform any rewrites needed prior to a `.cps: T.` transformation.
    if n.kind != nnkProcDef: return n
    result = n
    result.body = rewriteCalls result.body
