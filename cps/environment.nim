import std/sets
import std/options
import std/sequtils
import std/hashes
import std/tables
import std/macros

#[

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
    goto: seq[NimNode]        # identifiers of future gotos
    breaks: seq[NimNode]      # identifiers of future breaks

func insideCps*(e: Env): bool = len(e.goto) > 0 or len(e.breaks) > 0

func next(ns: seq[NimNode]): NimNode =
  ## read the next call off the stack
  if len(ns) == 0:
    newNilLit()
  else:
    ns[^1]

template nextOf(x: untyped): typed =
  var e = e
  while not e.isNil:
    result = next(e.`x`)
    if result.kind == nnkNilLit:
      e = e.parent
    else:
      break

func nextGoto*(e: Env): NimNode = nextOf(goto)
func nextBreak*(e: Env): NimNode = nextOf(breaks)
proc addGoto*(e: var Env; n: NimNode) = e.goto.add n
proc addBreak*(e: var Env; n: NimNode) = e.breaks.add n
proc popGoto*(e: var Env): NimNode = pop e.goto
proc popBreak*(e: var Env): NimNode = pop e.breaks

func isEmpty*(n: NimNode): bool =
  ## `true` if the node `n` is Empty
  result = not n.isNil and n.kind == nnkEmpty

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
  assert not e.via.isEmpty
  result = e.via

proc isDirty*(e: Env): bool =
  assert not e.isNil
  result = e.id.isNil or e.id.isEmpty

proc identity*(e: Env): NimNode =
  assert not e.isNil
  assert not e.isDirty
  result = e.id

proc setDirty(e: var Env) =
  assert not e.isNil
  e.id = newEmptyNode()
  assert e.isDirty

proc root*(e: Env): NimNode =
  var r = e
  while not r.parent.isNil:
    r = r.parent
  result = r.inherits

proc newEnv*(via: NimNode): Env =
  assert not via.isNil
  assert not via.isEmpty
  result = Env(via: via, id: via)

proc children(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.child)
    result.add children(e.parent)

iterator pairs(e: Env): Pair =
  var seen = initHashSet[string](len(e))
  for key, val in pairs(e.child):
    seen.incl key.strVal
    yield (key: key, val: val)
  for pair in children(e.parent):
    if not seen.containsOrIncl(pair[0].strVal):
      yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in pairs(e):
    for value in items(section):
      if value[1].isEmpty:
        error "give " & $name & " a type: " & repr(section)
      else:
        # name is an ident or symbol
        n.add newIdentDefs(ident($name), value[1])

template cpsLift*() {.pragma.}

proc `[]=`*(e: var Env; key: NimNode; val: NimNode) =
  ## set [ident|sym] = let/var section
  assert key.kind in {nnkSym, nnkIdent}
  assert val.kind in {nnkVarSection, nnkLetSection}
  e.child[key] = val
  setDirty e

proc addSection(e: var Env; n: NimNode) =
  ## add a let/var section to the env
  assert n.kind in {nnkVarSection, nnkLetSection}
  assert len(n) == 1, "pass 1-item sections"
  var (n, ts) = (n[0], n)
  if len(n) == 2:
    # ident: type
    n.add newEmptyNode()
  # ident: type = default
  e[n[0]] = ts

proc letOrVar(n: NimNode): NimNode =
  ## used on params to turn them into let/var sections
  assert n.kind == nnkIdentDefs
  case n[1].kind
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection.newTree newIdentDefs(n[0], n[1][0])
  else:
    result = nnkLetSection.newTree newIdentDefs(n[0], n[1])
  if len(result.last) < len(n):
    result.last.add n.last

proc add*(e: var Env; n: NimNode) =
  ## add a let/var section or proc param to the env
  case n.kind
  of nnkVarSection, nnkLetSection:
    for defs in items(n):
      addSection(e, newTree(n.kind, defs))
  of nnkIdentDefs:
    e.add letOrVar(n)
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
    parent.add e.via
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
    e.id = genSym(nskType, "env")
    typedef.add e.id
    typedef.add newEmptyNode()
    typedef.add e.objectType
    result = some(typedef)
    assert not e.isDirty

proc storeTypeSection*(e: var Env; into: var NimNode) =
  ## turn an env into a complete typedef in a type section
  let made = e.makeType
  if made.isSome:
    into.add newCommentStmtNode"stored the env into typesection here"
    var ts = newNimNode(nnkTypeSection)
    ts.add get(made)
    into.add ts
  assert not e.isDirty

proc newEnv*(into: var NimNode; parent: var Env): Env =
  ## a new env from the given parent; add a typedef for the
  ## parent into `into` if necessary
  assert not into.isNil
  assert not parent.isNil
  if into.kind == nnkStmtList:
    if parent.isDirty:
      parent.storeTypeSection(into)
    result = newEnv(parent.id)
    result.parent = parent
  else:
    # just pass the parent when we aren't prepared to record env changes
    result = parent

iterator localAssignments*(e: Env; locals: NimNode): Pair {.deprecated.} =
  for name, section in pairs(e):
    yield (key: name, val: newAssignment(newDotExpr(locals, name), name))

iterator localRetrievals*(e: Env; locals: NimNode): Pair =
  let locals = newCall(e.identity, locals)
  for name, value in pairs(e):
    let section = newNimNode(value.kind)
    # value[0][1] is the (only) identdefs of the section; [1] is type
    section.add newIdentDefs(name, value[0][1], newDotExpr(locals, name))
    yield (key: name, val: section)

proc defineLocals*(into: var NimNode; e: var Env; goto: NimNode): NimNode =
  e.storeTypeSection(into)
  assert not e.isDirty
  var obj = nnkObjConstr.newNimNode
  obj.add e.identity
  obj.add newColonExpr(ident"fn", goto)
  for name, section in pairs(e):
    obj.add newColonExpr(name, name)
  result = obj
  when false:
    result = gensym(nskLet, "locals")
    var vs = nnkLetSection.newNimNode
    vs.add newIdentDefs(result, newEmptyNode(), obj)
    into.add vs

template withGoto*(n: NimNode; body: untyped): untyped {.dirty.} =
  ## run a body with a longer goto stack
  if len(stripComments n) > 0:
    env.goto.add n
    try:
      body
    finally:
      result.add env.goto.pop
  else:
    body
