import std/macros except newStmtList, items

import cps/[spec, hooks, normalizedast]

proc firstReturn*(p: NormNode): NormNode =
  ## Find the first control-flow return statement or cps
  ## control-flow within statement lists; else, nil.
  case p.kind
  of nnkReturnStmt, nnkRaiseStmt:
    result = p
  of nnkTryStmt, nnkStmtList, nnkStmtListExpr:
    for child in p.items:
      result = child.firstReturn
      if not result.isNil:
        break
  of nnkBlockStmt, nnkBlockExpr, nnkFinally, nnkPragmaBlock:
    result = p.last.firstReturn
  elif p.isScopeExit:
    result = p
  else:
    result = NilNormNode

proc makeReturn*(contType: Name; n: NormNode): NormNode =
  ## generate a `return` of the node if it doesn't already contain a return
  if n.firstReturn.isNil:
    let toAdd =
      if n.kind in nnkCallKinds:
        n             # what we're saying here is, don't hook Coop on magics
      else:
        Coop.hook:
          newCall contType:
            n         # but we will hook Coop on child continuations
    nnkReturnStmt.newNimNode(n).add(toAdd)
  else:
    n

proc makeReturn*(contType: Name; pre, n: NormNode): NormNode =
  ## if `pre` holds no `return`, produce a `return` of `n` after `pre`
  if not pre.firstReturn.isNil:
    result.add:
      n.errorAst "i want to know about this"
  result = newStmtList pre
  result.add:
    if pre.firstReturn.isNil:
      makeReturn(contType, n)
    else:
      newEmptyNode().NormNode
    #else:
    #  doc "omitted a return of " & repr(n)

template pass*(source: Continuation; destination: Continuation): Continuation {.used.} =
  ## This symbol may be reimplemented to introduce logic during
  ## the transfer of control between parent and child continuations.
  ## The return value specifies the destination continuation.
  Continuation destination

proc dismiss*(continuaton: Continuation): Continuation {.cpsMagic, used.} =
  ## A convenience which simply discards the continuation.
  discard

proc terminator*(c: Name; contType: Name; tipe: NormNode): NormNode =
  ## produce the terminating return statement of the continuation;
  ## this should return control to the mom and dealloc the continuation,
  ## or simply set the fn to nil and return the continuation.
  let coop = NimNode hook(Coop, asName"result")
  let pass = NimNode hook(Pass, newCall(contType, c), c.dot "mom")
  let dealloc = NimNode hook(Dealloc, newCall(contType, c), tipe)
  let c = NimNode c
  NormNode:
    quote:
      if `c`.isNil:
        result = nil
      else:
        `c`.fn = nil
        if `c`.mom.isNil:
          result = `c`
        else:
          # pass(continuation, c.mom)
          #result = (typeof `c`) `pass` Error: expected type, but got: Continuation(continuation.mom)
          result = `pass`
          if result != `c`:
            `c`.mom = nil
            # perform a cooperative yield if pass() chose mom
            result = `coop`
            # dealloc(env_234234, continuation)
            discard `dealloc`
      # critically, terminate control-flow here!
      return

proc tailCall*(cont, contType, to: Name; jump: NormNode = NilNormNode): NormNode =
  ## a tail call to `to` with `cont` as the continuation; if the `jump`
  ## is supplied, return that call instead of the continuation itself
  result = newStmtList:
    newAssignment(newDotExpr(cont, "fn".asName), to)

  # figure out what the return value will be...
  result = makeReturn(contType, result):
    if jump.isNil:
      cont.NormNode        # just return our continuation
    else:
      jump                 # return the jump target as requested

proc jumperCall*(cont, contType, to: Name; via: NormNode): NormNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ## The `via` argument is expected to be a cps jumper call.
  let jump = asCall via.copyNimTree
  # we may need to insert an argument if the call is magical
  if jump.impl.hasPragma "cpsMagicCall":
    # https://github.com/nim-lang/Nim/issues/18365
    jump.prependArg newCall(contType, cont)
  # we need to desym the jumper; it is currently sem-ed to the
  # variant that doesn't take a continuation.
  desym jump
  result = tailCall(cont, contType, to, jump)
