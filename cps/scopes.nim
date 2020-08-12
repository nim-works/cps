##[

  Scopes are used to keep track of control-flow targets; calls, breaks,
  continues, returns, flow-through, that sort of thing.

]##
import std/strutils
import std/macros

const
  scopeful = {nnkTryStmt, nnkWhileStmt, nnkIfStmt, nnkBlockStmt, nnkForStmt}

type
  Scope* = ref object
    parent*: NimNode          # the source node we're coming from
    kind*: NimNodeKind        # the source node kind we're coming from
    node*: NimNode            # the identifier/proc we're going to
    label*: NimNode           # blocks populate this for named breaks
    name*: NimNode            # name we can use for identifying the proc
    goto*: Scope              # where do you go after?
    brake*: Scope             # where do you go in the event of a break?
    scope*: Scope             # where did you come from?
  Scopes* = seq[Scope]

func isEmpty*(n: NimNode): bool =
  ## `true` if the node `n` is Empty
  result = not n.isNil and n.kind == nnkEmpty

func isNil*(scope: Scope): bool =
  ## `true` if the scope `scope` is undefined
  result = result or system.isNil(scope)
  result = result or nnkNilLit in {scope.kind, scope.node.kind}

proc kind*(scope: Scope): NimNodeKind =
  ## what kind of ast created the scope?
  if not scope.parent.isNil and not scope.parent.isEmpty:
    result = scope.parent.kind
  else:
    result = scope.kind

func isEmpty*(scope: Scope): bool =
  ## `true` if the scope `scope` is Empty or Nil
  result = scope.isNil or scope.node.isEmpty or scope.kind == nnkEmpty

proc `$`*(scope: Scope): string =
  if scope.isNil:
    result = "🔭(nil)"
  elif scope.isEmpty:
    result = "🔭(empty)"
  else:
    result = "🔭(kind: $1, name: $2, label: $4, node: $3)" % [
      $scope.kind, repr(scope.name),
      lispRepr(scope.node), repr(scope.label) ]

proc newScope*(parent: Scope = nil): Scope =
  ## sentinel value for searches, etc.
  result = Scope(kind: nnkNilLit, parent: newNilLit(),
                 label: newEmptyNode(), node: newEmptyNode(),
                 name: newEmptyNode())
  if not parent.isNil:
    result.goto = parent.goto
    result.brake = parent.brake

proc next*(ns: Scopes): Scope =
  ## read the next call off the stack
  if len(ns) == 0:
    newScope()
  else:
    ns[^1]

proc last*(ns: Scopes): Scope =
  ## query the last loop in the stack
  result = newScope()
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

proc returnTo*(scope: Scope): NimNode =
  ## given a scope, find the ident|sym it's pointing to, or `nil`
  if scope.isNil:
    result = newNilLit()
  elif scope.name.isEmpty:
    case scope.node.kind
    of nnkIdent, nnkSym, nnkNilLit:
      result = scope.node
    of nnkEmpty:
      # an empty scope is essentially used for `return nil`
      result = newNilLit()
    of nnkProcDef:
      result = scope.node.name
      warning "missing name for scope " & $scope
    of nnkCall, nnkObjConstr, nnkExprColonExpr, nnkCast:
      result = scope.node[1]
      warning "missing name for scope " & $scope
    else:
      echo scope
      echo scope.kind, "  ", scope.node.kind, "  ", scope.node.repr
      raise newException(Defect, "unable to guess goto identifier")
  else:
    result = scope.name

proc newScope*(n: NimNode; parent: Scope = nil): Scope =
  result = newScope(parent)
  result.node = n
  # try to guess the name early
  result.name = returnTo(result)
  result.label = n.breakName

proc newScope*(parent: NimNode; name: NimNode; n: NimNode): Scope =
  ## avoid returnTo() and form our chosen name
  result = newScope(nil.Scope)
  result.kind = parent.kind
  result.name = name
  result.node = n
  result.parent = parent
  result.label = parent.breakName

proc add*(ns: var Scopes; k: NimNode; n: NimNode) =
  assert n.kind in {nnkIdent, nnkSym}
  var scope = newScope(k, n, n)
  ns.add scope

proc openScope*(s: Scope; n: NimNode): Scope =
  ## open a new scope for the given node; returns the current scope
  assert not n.isNil
  case n.kind
  of scopeful:
    result = newScope(n, parent = s)
  else:
    result = s

proc closeScope*(s: Scope; n: NimNode): Scope =
  ## close an open scope for the given node; returns the current scope
  assert not n.isNil
  case n.kind
  of scopeful:
    result = s.scope
  else:
    result = s

template withScope*(s: Scope; n: NimNode; body: untyped) =
  ## do some work on a particular node with a particular scope
  openScope(s, n)
  try:
    var scope {.inject} = s
    body
  finally:
    closeScope(s, n)
