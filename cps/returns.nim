import std/macros

import cps/[spec, hooks]

proc firstReturn*(p: NimNode): NimNode =
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
      newEmptyNode()
    #else:
    #  doc "omitted a return of " & repr(n)

proc maybeReturnParent*(T: NimNode; c: NimNode): NimNode =
  ## the appropriate target of a `return` statement in a CPS procedure
  ## (that would otherwise return continuation `c`) first performs a
  ## runtime check to see if the parent should be returned instead.
  let justmom = newDotExpr(c, ident"mom")
  let convmom = newCall(newCall(ident"typeof", c), justmom)
  makeReturn:                               # return value is as follows:
    nnkIfExpr.newTree [
      nnkElifExpr.newTree [                 # if
        newCall(bindSym"isNil", justmom),   # we have no parent,
        c                                   # the current continuation;
      ],
      nnkElseExpr.newTree [                 # else,
        Pass.hook(c, convmom)               # hook into pass() and yield
                                            # our parent continuation.
      ],
    ]

template pass*[T: Continuation](source, destination: T): T {.used.} =
  ## This symbol may be reimplemented to introduce logic during
  ## the transfer of control between parent and child continuations.
  ## The return value specifies the destination continuation.
  destination

proc terminator*(c: NimNode; T: NimNode): NimNode =
  ## produce the terminating return statement of the continuation;
  ## this should return control to the mom and dealloc the continuation,
  ## or simply set the fn to nil and return the continuation.
  let (dealloc, pass) = (Dealloc.sym, Pass.sym)
  quote:
    if `c`.isNil:
      result = `c`
    else:
      `c`.fn = nil
      if `c`.mom.isNil:
        result = `c`
      else:
        result = `pass`((typeof `c`)(`c`), (typeof `c`)(`c`.mom))
        if result != `c`:
          `dealloc`(`T`, `c`)

proc tailCall*(cont: NimNode; to: NimNode; jump: NimNode = nil): NimNode =
  ## a tail call to `to` with `cont` as the continuation; if the `jump`
  ## is supplied, return that call instead of the continuation itself
  result = newStmtList:
    newAssignment(newDotExpr(cont, ident"fn"), to)

  # figure out what the return value will be...
  result = makeReturn result:
    if jump.isNil:
      cont                         # just return our continuation
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
