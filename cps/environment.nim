import std/sets
import std/sequtils
import std/hashes
import std/tables
import std/macros

{.experimental: "dynamicBindSym".}

const
  cpsCast {.booldefine.} = false
  cpsDebug {.booldefine.} = false
  cpsTrace {.booldefine.} = false
  comments* = cpsDebug  ## embed comments within the transformation

type
  Continuation* = concept c
    c.fn is ContinuationProc[Continuation]
    c is ref object
    c of RootObj

  ContinuationProc*[T] = proc(c: T): T {.nimcall.}

  Pair = tuple
    key: NimNode
    val: NimNode

  Future = tuple
    kind: NimNodeKind        # the source node kind we're coming from
    node: NimNode            # the identifier/proc we're going to
    name: NimNode            # named blocks populate this for named breaks
  Futures = seq[Future]

  Env* = ref object
    id: NimNode                     # the identifier of our continuation type
    via: NimNode                    # the type we inherit from
    parent: Env                     # the parent environment (scope)
    child: Table[NimNode, NimNode]  # locals and their typedefs
    goto: Futures                   # identifiers of future gotos
    breaks: Futures                 # identifiers of future breaks
    store: NimNode                  # where to put typedefs
    label: NimNode                  # the last tailcall (goto)

func doc*(s: string): NimNode =
  ## generate a doc statement for debugging
  when comments:
    newCommentStmtNode(s)
  else:
    newEmptyNode()

proc doc*(n: var NimNode; s: string) =
  ## add a doc statement to the ast for debugging
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

func insideCps*(e: Env): bool = len(e.goto) > 0 or len(e.breaks) > 0

proc next(ns: Futures): Future =
  ## read the next call off the stack
  if len(ns) == 0:
    (kind: nnkNilLit, node: newNilLit(), name: newEmptyNode())
  else:
    ns[^1]

proc last(ns: Futures): Future =
  ## query the last loop in the stack
  result = (kind: nnkNilLit, node: newNilLit(), name: newEmptyNode())
  for i in countDown(ns.high, ns.low):
    if ns[i].kind in {nnkWhileStmt, nnkForStmt}:
      result = ns[i]
      break

template searchScope(env: Env; x: untyped;
                     p: proc(ns: Futures): Future): Future =
  var e = env
  var r = (kind: nnkNilLit, node: newNilLit(), name: newEmptyNode())
  while not e.isNil:
    r = p(e.`x`)
    if r.kind == nnkNilLit:
      e = e.parent
    else:
      break
  r

func lastGotoLoop*(e: Env): Future = searchScope(e, goto, last)
func lastBreakLoop*(e: Env): Future = searchScope(e, breaks, last)

func nextGoto*(e: Env): NimNode = searchScope(e, goto, next).node
func nextBreak*(e: Env): NimNode = searchScope(e, breaks, next).node

proc breakName(n: NimNode): NimNode =
  result =
    if n.kind in {nnkBlockStmt} and len(n) > 1:
      n[0]
    else:
      newEmptyNode()

proc addGoto*(e: var Env; k: NimNode; n: NimNode) =
  e.goto.add (k.kind, n, newEmptyNode())

proc addBreak*(e: var Env; k: NimNode; n: NimNode) =
  e.breaks.add (k.kind, n, k.breakName)

proc popGoto*(e: var Env): NimNode = pop(e.goto).node
proc popBreak*(e: var Env): NimNode = pop(e.breaks).node

proc insideFor*(e: Env): bool = lastBreakLoop(e).kind == nnkForStmt
proc insideWhile*(e: Env): bool = lastBreakLoop(e).kind == nnkWhileStmt

proc topOfWhile*(e: Env): NimNode =
  ## fetch the goto target in order to `continue` inside `while:`
  assert e.insideWhile, "i thought i was in a while loop"
  let future = lastGotoLoop(e)
  assert future.kind == nnkWhileStmt, "goto doesn't match break"
  result = future.node

proc namedBreak*(e: Env; n: NimNode): NimNode =
  ## fetch the goto target in order to `break foo`
  assert n.kind == nnkBreakStmt
  if len(n) == 0:
    result = e.nextBreak
  else:
    proc match(ns: Futures): Future =
      ## find the loop matching the requested named break
      result = (kind: nnkNilLit, node: newNilLit(), name: newEmptyNode())
      for i in countDown(ns.high, ns.low):
        if ns[i].kind in {nnkBlockStmt} and eqIdent(ns[i].name, n[0]):
          result = ns[i]
          break
    result = searchScope(e, breaks, match).node

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

proc init[T](c: T; l: LineInfo): T =
  warning "provide an init proc for cpsTrace"

proc addTrace(e: Env; n: NimNode): NimNode =
  if n.isNil or n.kind == nnkNilLit: return
  # XXX: this doesn't work, sadly
  #discard bindSym("init" & $e.root, rule = brForceOpen)
  let info = lineInfoObj(n)
  var identity =
    if e.label.isNil or e.label.kind == nnkNilLit:
      "nil"
    else:
      repr(e.label)
  identity.add "(" & repr(e.identity) & ")"
  result = newCall(ident("init"), n, identity.newLit,
                   info.filename.newLit,
                   info.line.newLit,
                   info.column.newLit)

proc castToRoot(e: Env; n: NimNode): NimNode =
  when cpsCast:
    result = newTree(nnkCast, e.root, n)
  else:
    result = newTree(nnkCall, e.root, n)
  when cpsTrace:
    result = e.addTrace(result)

proc castToChild(e: Env; n: NimNode): NimNode =
  when cpsTrace:
    var n = e.addTrace(n)
  when cpsCast:
    result = newTree(nnkCast, e.identity, n)
  else:
    result = newTree(nnkCall, e.identity, n)

proc maybeConvertToRoot*(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    e.castToRoot(locals)
  else:
    locals

proc newEnv*(store: var NimNode; via: NimNode): Env =
  assert not via.isNil
  assert not via.isEmpty
  result = Env(store: store, via: via, id: via)

proc children(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.child)
    result.add children(e.parent)

proc seenHere(e: Env; size = 4): HashSet[string] =
  ## a hashset of identifiers defined in the env
  assert not e.isNil
  result = initHashSet[string](sets.rightSize(len(e.child)))
  for key in keys(e.child):
    result.incl key.strVal

proc seen(e: Env; size = 4): HashSet[string] =
  ## a hashset of identifiers defined in the env or its parent
  if e.isNil or e.parent.isNil:
    result = initHashSet[string](sets.rightSize(size))
  else:
    result = e.parent.seen(len(e))
  if not e.isNil:
    for key in keys(e.child):
      result.incl key.strVal

iterator pairs(e: Env): Pair =
  assert not e.isNil
  var seen = initHashSet[string](sets.rightSize(len(e.child)))
  var p = e
  while not p.isNil:
    for key, val in pairs(p.child):
      if not seen.containsOrIncl key.strVal:
        yield (key: key, val: val)
    p = p.parent

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
  assert key notin e.child
  e.child[key] = val
  setDirty e

proc addSection(e: var Env; n: NimNode) =
  ## add a let/var section to the env
  assert n.kind in {nnkVarSection, nnkLetSection}
  for i in 0 ..< len(n):
    var def = n[i]
    case def.kind
    of nnkIdentDefs:
      if len(def) == 2:
        # ident: type
        def.add newEmptyNode()
      for name in def[0 ..< len(def)-2]:  # ie. omit type and default
        e[name] = newTree(n.kind,
                          # ident: type = default
                          newIdentDefs(name, def[^2], def[^1]))
    #[
    of nnkVarTuple:
      assert def.last.kind == nnkPar, "expected parenthesis: " & repr(def)
      let par = def.last
      for i in 0 ..< len(par):
        let name = def[i]
        e[name] = newTree(n.kind,
                          newIdentDefs(name, getTypeInst(par[i]), par[i]))
    ]#
    else:
      error $def.kind & " is unsupported by cps: \n" & treeRepr(def)

proc letOrVar(n: NimNode): NimNode =
  ## used on params to turn them into let/var sections
  assert n.kind == nnkIdentDefs
  # FIXME: support `a, b, c: int = 5` syntax
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
  var record = nnkRecList.newNimNode(e.identity)
  populateType(e, record)
  var parent = nnkOfInherit.newNimNode(e.root).add e.inherits
  result = nnkRefTy.newTree nnkObjectTy.newTree(pragma, parent, record)

proc performReparent(e: var Env) =
  assert not e.isNil
  var here = seenHere(e)
  var p = e.parent
  while not p.isNil:
    var there = seen(p)
    if card(here * there) == 0:
      assert not p.isDirty
      e.via = p.id
      break
    elif p.parent.isNil:
      e.via = p.via
    p = p.parent

var c {.compiletime.}: int
proc makeType*(e: var Env): NimNode =
  ## turn an env into a named object typedef `foo = object ...`
  e.id = genSym(nskType, "env" & $c)
  inc c

  # determine if a symbol clash necessitates pointing to a new parent
  performReparent(e)

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
  let locals = e.castToChild(locals)
  for name, value in pairs(e):
    let tmp = nnkTemplateDef.newTree(name, newEmptyNode(), newEmptyNode(), newEmptyNode(), newEmptyNode(), newEmptyNode(), newDotExpr(locals, name))
    yield (key: name, val: tmp)

proc defineLocals*(e: var Env; goto: NimNode): NimNode =
  e.storeType
  result = nnkObjConstr.newTree(e.identity, newColonExpr(ident"fn", goto))
  for name, section in pairs(e):
    result.add newColonExpr(name, name)
  # setting the label here allows us to use it in trace composition
  e.label = goto

template withGoto*(f: NimNodeKind; n: NimNode; body: untyped): untyped {.dirty.} =
  ## run a body with a longer goto stack
  if len(stripComments n) > 0:
    env.goto.add (kind: f, node: n, name: newEmptyNode())
    try:
      body
    finally:
      result.add pop(env.goto).node
  else:
    body

