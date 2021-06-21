import std/macros except newStmtList

import cps/[spec, hooks, normalizedast]

proc firstReturn*(p: NormalizedNimNode): NormalizedNimNode =
  ## Find the first control-flow return statement or cps
  ## control-flow within statement lists; else, nil.
  case p.kind
  of nnkReturnStmt, nnkRaiseStmt:
    result = p
  of nnkTryStmt, nnkStmtList, nnkStmtListExpr:
    for child in p.items:
      result = child.NormalizedNimNode.firstReturn
      if not result.isNil:
        break
  of nnkBlockStmt, nnkBlockExpr, nnkFinally, nnkPragmaBlock:
    result = p.last.firstReturn
  elif p.isScopeExit:
    result = p
  else:
    result = nil

proc makeReturn*(n: NormalizedNimNode): NormalizedNimNode =
  ## generate a `return` of the node if it doesn't already contain a return
  if n.firstReturn.isNil:
    let toAdd = 
      if n.kind in nnkCallKinds:
        n             # what we're saying here is, don't hook Coop on magics
      else:
        hook(Coop, n) # but we will hook Coop on child continuations
    nnkReturnStmt.newNimNode(n).add(toAdd).NormalizedNimNode
  else:
    n

proc makeReturn*(pre, n: NormalizedNimNode): NormalizedNimNode =
  ## if `pre` holds no `return`, produce a `return` of `n` after `pre`
  if not pre.firstReturn.isNil:
    result.add:
      n.errorAst "i want to know about this"
  result = newStmtList pre
  result.add:
    if pre.firstReturn.isNil:
      makeReturn n
    else:
      newEmptyNode().NormalizedNimNode
    #else:
    #  doc "omitted a return of " & repr(n)

template pass*(source: Continuation; destination: Continuation): Continuation {.used.} =
  ## This symbol may be reimplemented to introduce logic during
  ## the transfer of control between parent and child continuations.
  ## The return value specifies the destination continuation.
  Continuation destination

proc terminator*(c: Name; T: NimNode): NormalizedNimNode =
  ## produce the terminating return statement of the continuation;
  ## this should return control to the mom and dealloc the continuation,
  ## or simply set the fn to nil and return the continuation.
  let (dealloc, pass, coop) = (Dealloc.sym, Pass.sym, Coop.sym)
  NormalizedNimNode:
    quote:
      if `c`.isNil:
        result = `c`
      else:
        # we're converting to Cont here for sigmatch reasons despite the
        # fact that Continuation is probably the only rational type
        # pass(continuation, Cont(c.mom))
        result = (typeof `c`) `pass`(`c`, Continuation `c`.mom)
        if result != `c`:
          # perform a cooperative yield when we pass control to mom
          result = `coop` result
          # dealloc(env_234234, continuation)
          `dealloc`(`T`, `c`)
      # critically, terminate control-flow here!
      return

proc tailCall*(cont, to: Name; jump: NormalizedNimNode = nil): NormalizedNimNode =
  ## a tail call to `to` with `cont` as the continuation; if the `jump`
  ## is supplied, return that call instead of the continuation itself
  result = newStmtList:
    newAssignment(newDotExpr(cont, asName("fn")), to)

  # figure out what the return value will be...
  result = makeReturn result:
    if jump.isNil:
      cont.NormalizedNimNode  # just return our continuation
    else:
      jump                    # return the jump target as requested

proc jumperCall*(cont, to: Name; via: NormalizedNimNode): NormalizedNimNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ## The `via` argument is expected to be a cps jumper call.
  let jump = copyNimTree via
  # we may need to insert an argument if the call is magical
  if getImpl(jump[0]).hasPragma "cpsMagicCall":
    jump.insert(1, cont.NimNode)
  # we need to desym the jumper; it is currently sem-ed to the
  # variant that doesn't take a continuation.
  jump[0] = desym jump[0]
  result = tailCall(cont, to, jump)
