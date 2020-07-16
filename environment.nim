import std/options
import std/sequtils
import std/hashes
import std/tables
import std/macros

#[

XXX: need to handle shadowing with a dedupe or something

the idea here is that we can make a type hierarchy like
  grandparent
     -> parent
         -> child

and then later
  -> parent becomes a grandparent either by conversion or birth
  -> child becomes a parent by birth
  -> parent can beget new sibling to child

we accumulate var/let statements
generate a type for passing around which holds the environment
the environment gets unpacked in procs; this could even be swapped

]#

import eventqueue

type
  Pair = tuple
    key: NimNode
    val: NimNode

  Flag = enum
    Mutable

  Env* = ref object
    id: NimNode
    via: NimNode
    parent: Env
    child: Table[NimNode, NimNode]
    flags: set[Flag]

proc hash*(n: NimNode): Hash =
  var h: Hash = 0
  h = h !& hash($n)
  result = !$h

proc len*(e: Env): int =
  if not e.isNil:
    result = len(e.child)
    result.inc len(e.parent)

proc isEmpty*(e: Env): bool =
  result = len(e) == 0

proc inherits*(e: Env): NimNode =
  assert not e.isNil
  assert not e.via.isNil
  assert e.via.kind != nnkEmpty
  result = e.via

proc newEnv*(via: NimNode): Env =
  assert not via.isNil
  assert via.kind != nnkEmpty
  result = Env(via: via, id: via)

proc children(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.child)
    result.add children(e.parent)

iterator pairs(e: Env): Pair =
  ## FIXME: don't emit shadowed nodes
  for key, val in pairs(e.child):
    yield (key: key, val: val)
  for pair in children(e.parent):
    yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in pairs(e):
    for value in items(section):
      if value[1].kind == nnkEmpty:
        error "give " & $name & " a type: " & repr(section)
      else:
        n.add newIdentDefs(name, value[1])

template cpsLift*() {.pragma.}

proc isDirty(e: Env): bool =
  result = e.id.isNil or e.id.kind == nnkEmpty

proc identity*(e: Env): NimNode =
  assert not e.isNil
  assert not e.isDirty
  result = e.id

proc setDirty(e: var Env) =
  e.id = newEmptyNode()
  assert e.isDirty

proc `[]=`*(e: var Env; key: NimNode; val: NimNode) =
  assert key.kind == nnkIdent
  assert val.kind in {nnkVarSection, nnkLetSection}
  e.child[key] = val
  setDirty e

proc add*(e: var Env; n: NimNode) =
  case n.kind
  of nnkVarSection, nnkLetSection:
    for defs in items(n):
      var s = newNimNode(n.kind)
      case len(defs)
      of 2:
        # ident = value; glwt
        s.add newIdentDefs(defs[0], defs[1], newEmptyNode())
      of 3:
        # ident = value, default
        s.add newIdentDefs(defs[0], defs[1], defs[2])
      else:
        assert false, "wut"
      e[defs[0]] = s
  else:
    assert false, "unrecognized input node " & repr(n)

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  var pragma = newNimNode(nnkPragma)
  pragma.add bindSym"cpsLift"
  var record = newNimNode(nnkRecList)
  populateType(e, record)
  var parent = nnkOfInherit.newNimNode
  if e.parent.isNil:
    parent.add ident"RootObj"
  else:
    parent.add e.parent.identity
  var obj = newNimNode(nnkObjectTy)
  obj.add pragma
  obj.add parent
  obj.add record
  result = nnkRefTy.newNimNode
  result.add obj

proc makeType*(e: var Env): Option[NimNode] =
  ## turn an env into a named object typedef `foo = object ...`
  if e.isDirty:
    var typedef = newNimNode(nnkTypeDef)
    let name = genSym(nskType, "env")
    typedef.add name
    typedef.add newEmptyNode()
    typedef.add e.objectType
    e.id = name
    result = some(typedef)
    assert not e.isDirty

proc storeTypeSection*(e: var Env; into: var NimNode) =
  ## turn an env into a complete typedef in a type section
  let made = e.makeType
  if made.isSome:
    var ts = newNimNode(nnkTypeSection)
    ts.add get(made)
    into.add ts

proc newEnv*(into: var NimNode; parent: var Env): Env =
  ## a new env from the given parent; add a typedef for the
  ## parent into `into` if necessary
  assert not into.isNil
  if into.kind == nnkStmtList:
    if parent.isDirty:
      parent.storeTypeSection(into)
    result = newEnv(parent.id)
    result.parent = parent
  else:
    warning "kind is " & $into.kind

iterator localAssignments*(e: Env; locals: NimNode): Pair =
  for name, section in pairs(e):
    yield (key: name, val: newAssignment(newDotExpr(locals, name), name))

iterator localRetrievals*(e: Env; locals: NimNode): Pair =
  for name, value in pairs(e):
    let section = newNimNode(value.kind)
    # value[0][1] is the (only) identdefs of the section; [1] is type
    section.add newIdentDefs(name, value[0][1], newDotExpr(locals, name))
    echo treeRepr(section)
    yield (key: name, val: section)

proc defineLocals*(into: var NimNode; e: Env): NimNode =
  assert not e.isDirty
  #if not e.parent.isDirty and eqIdent(e.identity, e.inherits):
  if false:
    result = e.parent.identity
  else:
    result = gensym(nskVar, "locals")
    var vs = nnkVarSection.newNimNode
    when true:
      vs.add newIdentDefs(result, e.identity)
    else:
      if e.parent.isDirty or len(e.parent) == 0:
        vs.add newIdentDefs(result, e.identity)
      else:
        vs.add newIdentDefs(result, e.identity, e.parent.identity)
    into.add vs
