import std/macros

type
  # futures are used to keep track of control-flow targets; calls, breaks,
  # continues, flow-through, that sort of thing
  Future* = object
    kind*: NimNodeKind        # the source node kind we're coming from
    node*: NimNode            # the identifier/proc we're going to
    label*: NimNode           # blocks populate this for named breaks
    name*: NimNode            # name we can use for identifying the proc
  Futures* = seq[Future]

proc newFuture*(): Future =
  ## sentinel value for searches, etc.
  result = Future(kind: nnkNilLit, node: newNilLit(),
                  name: newEmptyNode(), label: newEmptyNode())

proc next*(ns: Futures): Future =
  ## read the next call off the stack
  if len(ns) == 0:
    newFuture()
  else:
    ns[^1]

proc last*(ns: Futures): Future =
  ## query the last loop in the stack
  result = newFuture()
  for i in countDown(ns.high, ns.low):
    if ns[i].kind in {nnkWhileStmt, nnkForStmt}:
      result = ns[i]
      break

proc breakName*(n: NimNode): NimNode =
  result =
    if n.kind in {nnkBlockStmt} and len(n) > 1:
      n[0]
    else:
      newEmptyNode()

proc newFuture*(n: NimNode): Future =
  result = newFuture()
  result.node = n
  result.label = n.breakName

proc newFuture*(kind: NimNodeKind; n: NimNode): Future =
  result = newFuture(n)
  result.kind = kind

proc add*(ns: var Futures; k: NimNode; n: NimNode) =
  var future = newFuture(k.kind, n)
  case n.kind
  of nnkIdent, nnkSym:
    future.name = n
  else:
    assert false
  ns.add future

func isEmpty*(n: NimNode): bool =
  ## `true` if the node `n` is Empty
  result = not n.isNil and n.kind == nnkEmpty

proc returnTo*(future: Future): NimNode =
  ## given a future, find the ident/sym it's pointing to
  if future.name.isEmpty:
    case future.node.kind
    of nnkIdent, nnkSym, nnkNilLit:
      result = future.node
    of nnkProcDef:
      result = future.node.name
    of nnkCall, nnkObjConstr, nnkExprColonExpr, nnkCast:
      result = future.node[1]
    else:
      raise newException(Defect, "unable to compute goto")
  else:
    result = future.name
