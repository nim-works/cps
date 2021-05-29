import std/macros

import cps/[spec, hooks]

proc firstReturn*(p: NimNode): NimNode =
  ## find the first control-flow return statement or cps control-flow within
  ## statement lists, or nil
  case p.kind
  of nnkReturnStmt, nnkRaiseStmt:
    result = p
  of nnkTryStmt, nnkStmtList, nnkStmtListExpr:
    for child in p.items:
      result = child.firstReturn
      if not result.isNil:
        break
  of nnkBlockStmt, nnkBlockExpr, nnkFinally:
    result = p.last.firstReturn
  elif p.isCpsPending or p.isCpsBreak or p.isCpsContinue:
    result = p
  else:
    result = nil

proc isNillish*(n: NimNode): bool =
  ## unwrap statement lists and see if the end in a literal nil
  case n.kind
  of nnkStmtList:
    isNillish n.last
  of nnkNilLit:
    true
  else:
    false

proc maybeReturnParent*(c: NimNode): NimNode =
  ## the appropriate target of a `return` statement in a CPS procedure
  ## (that would otherwise return continuation `c`) first performs a
  ## runtime check to see if the parent should be returned instead.
  let mom = newDotExpr(c, ident"mom")
  result =                                  # return value is as follows:
    nnkIfExpr.newTree [
      nnkElifExpr.newTree [                 # if
        newCall(bindSym"isNil", mom),       # we have no parent,
        c                                   # the current continuation;
      ],
      nnkElseExpr.newTree [                 # else,
        mom                                 # our parent continuation.
      ],
    ]

proc makeReturn*(n: NimNode): NimNode =
  ## generate a `return` of the node if it doesn't already contain a return
  if n.firstReturn.isNil:
    nnkReturnStmt.newNimNode(n).add:
      if n.kind in nnkCallKinds:
        n           # what we're saying here is, don't hook Coop on magics
      else:
        hook Coop:
          n         # but we will hook Coop on child continuations
  else:
    n

proc makeReturn*(pre: NimNode; n: NimNode): NimNode =
  ## if `pre` holds no `return`, produce a `return` of `n` after `pre`
  if not pre.firstReturn.isNil:
    result.add:
      n.errorAst "i want to know about this"
  result = newStmtList pre
  result.add:
    if pre.firstReturn.isNil:
      makeReturn n
    else:
      doc "omitted a return of " & repr(n)

proc tailCall*(cont: NimNode; to: NimNode; jump: NimNode = nil): NimNode =
  ## a tail call to `to` with `cont` as the continuation; if the `jump`
  ## is supplied, return that call instead of the continuation itself
  if to.isNillish and not jump.isNil:
    return jump.errorAst "where do you think you're going?"
  result = newStmtList:
    newAssignment(newDotExpr(cont, ident"fn"), to)

  # figure out what the return value will be...
  result = makeReturn result:
    if jump.isNil:
      if to.isNillish:             # continuation.fn appears to be nil,
        maybeReturnParent cont     # we may be returning a parent here.
      else:
        cont                       # just return our continuation
    else:
      jump                         # return the jump target as requested

proc jumperCall*(cont: NimNode; to: NimNode; via: NimNode): NimNode =
  ## Produce a tail call to `to` with `cont` as the continuation
  ## The `via` argument is expected to be a cps jumper call.
  let jump = copyNimTree via
  # we may need to insert an argument if the call is magical
  if getImpl(jump[0]).hasPragma "cpsMagicCall":
    jump.insert(1, cont)
  # we need to desym the jumper; it is currently sem-ed to the
  # variant that doesn't take a continuation.
  jump[0] = desym jump[0]
  result = tailCall(cont, to, jump)
