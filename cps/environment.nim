import std/sets
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
    store: NimNode            # where to put typedefs

func insideCps*(e: Env): bool = len(e.goto) > 0 or len(e.breaks) > 0

func next(ns: seq[NimNode]): NimNode =
  ## read the next call off the stack
  if len(ns) == 0:
    newNilLit()
  else:
    ns[^1]

template nextOf(x: untyped) =
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
  # a dirty parent yields a dirty child
  result = result or (not e.parent.isNil and e.parent.isDirty)

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

proc newEnv*(store: var NimNode; via: NimNode): Env =
  assert not via.isNil
  assert not via.isEmpty
  result = Env(store: store, via: via, id: via)

proc children(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.child)
    result.add children(e.parent)

proc seen(e: Env; size = 4): HashSet[string] =
  ## a hashset of identifiers defined in the env or its parent
  if e.isNil or e.parent.isNil:
    result = initHashSet[string](size)
  else:
    result = e.parent.seen(len(e))
  if not e.isNil:
    for key in keys(e.child):
      result.incl key.strVal

iterator pairs(e: Env): Pair =
  for key, val in pairs(e.child):
    yield (key: key, val: val)
  for pair in children(e.parent):
    yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in pairs(e.child):
    for defs in items(section):
      if defs[1].isEmpty:
        error "give " & $name & " a type: " & repr(section)
      else:
        # name is an ident or symbol
        n.add newIdentDefs(ident($name), defs[1])

template cpsLift*() {.pragma.}

proc contains*(e: Env; key: NimNode): bool =
  assert not key.isNil
  assert key.kind in {nnkSym, nnkIdent}
  result = key.strVal in e.seen

proc `[]=`*(e: var Env; key: NimNode; val: NimNode) =
  ## set [ident|sym] = let/var section
  assert key.kind in {nnkSym, nnkIdent}
  assert val.kind in {nnkVarSection, nnkLetSection}
  assert key notin e.parent, "Variable '" & val.repr & "' shadowed, not yet supported in CPS"
  assert key notin e.child
  e.child[key] = val
  setDirty e

proc addSection(e: var Env; n: NimNode) =
  ## add a let/var section to the env
  assert n.kind in {nnkVarSection, nnkLetSection}
  assert len(n) == 1, "pass 1-item sections"
  var (name, n, ts) = (n[0][0], n[0], n)
  if len(n) == 2:
    # ident: type
    n.add newEmptyNode()
  # ident: type = default
  e[name] = ts

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
      e.addSection newTree(n.kind, defs)
  of nnkIdentDefs:
    e.addSection letOrVar(n)
  else:
    assert false, "unrecognized input node " & repr(n)

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  var pragma = nnkPragma.newTree bindSym"cpsLift"
  var record = nnkRecList.newNimNode
  populateType(e, record)
  var parent = nnkOfInherit.newNimNode
  if e.parent.isNil:
    parent.add e.via
  else:
    parent.add e.parent.identity
  result = nnkRefTy.newTree nnkObjectTy.newTree(pragma, parent, record)

proc makeType*(e: var Env): NimNode =
  ## turn an env into a named object typedef `foo = object ...`
  e.id = genSym(nskType, "env")
  result = nnkTypeDef.newTree(e.id, newEmptyNode(), e.objectType)
  assert not e.isDirty

proc storeType*(e: var Env) =
  ## turn an env into a complete typedef in a type section
  assert not e.isNil
  if e.isDirty:
    if not e.parent.isNil:
      e.parent.storeType
    e.store.add nnkTypeSection.newTree e.makeType
  assert not e.isDirty

proc identity*(e: var Env): NimNode =
  assert not e.isNil
  e.storeType
  result = e.id

proc newEnv*(parent: var Env): Env =
  ## a new env from the given parent
  assert not parent.isNil
  # we'll want to optimize this block out, ultimately,
  # so that we don't need to dump types that aren't used
  block:
    parent.storeType
    result = newEnv(parent.store, parent.id)
  result.parent = parent
  result.store = parent.store

iterator localAssignments*(e: Env; locals: NimNode): Pair {.deprecated.} =
  for name, section in pairs(e):
    yield (key: name, val: newAssignment(newDotExpr(locals, name), name))

iterator localRetrievals*(e: Env; locals: NimNode): Pair =
  ## read locals out of an env
  let locals = newCall(e.identity, locals)
  for name, value in pairs(e):
    let section = newNimNode(value.kind)
    # value[0] is the (only) identdefs of the section; [0][1] is type
    section.add newIdentDefs(name, value[0][1], newDotExpr(locals, name))
    yield (key: name, val: section)

proc defineLocals*(e: var Env; goto: NimNode): NimNode =
  e.storeType
  result = nnkObjConstr.newTree(e.identity, newColonExpr(ident"fn", goto))
  for name, section in pairs(e):
    result.add newColonExpr(name, name)

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
