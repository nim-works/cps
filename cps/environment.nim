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

  LocalCache = OrderedTable[NimNode, NimNode] # symbols to types
  Env* = ref object
    id: NimNode                     # the identifier of our continuation type
    via: NimNode                    # the identifier of the type we inherit
    parent: Env                     # the parent environment (scope)
    locals: LocalCache              # locals and their typedefs|generics
    gotos: Futures                  # identifiers of future gotos
    breaks: Futures                 # identifiers of future breaks
    store: NimNode                  # where to put typedefs, a stmtlist
    label: NimNode                  # the last tailcall (goto)
    ideal: NimNode                  # the best parent for alloc purposes
    seen: HashSet[string]           # count/measure idents/syms by string

    # special symbols for cps machinery
    c: NimNode                      # the sym we use for the continuation
    fn: NimNode                     # the sym we use for the goto target
    ex: NimNode                     # the sym we use for stored exception
    rs: NimNode                     # the sym we use for "yielded" result

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

func insideCps*(e: Env): bool = len(e.gotos) > 0 or len(e.breaks) > 0

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

func lastGotoLoop*(e: Env): Future = searchScope(e, gotos, last)
func lastBreakLoop*(e: Env): Future = searchScope(e, breaks, last)

func nextGoto*(e: Env): NimNode = searchScope(e, gotos, next).node
func nextBreak*(e: Env): NimNode = searchScope(e, breaks, next).node

proc breakName(n: NimNode): NimNode =
  result =
    if n.kind in {nnkBlockStmt} and len(n) > 1:
      n[0]
    else:
      newEmptyNode()

proc addGoto*(e: var Env; k: NimNode; n: NimNode) =
  ## add to a stack of gotos, which are normal exits from control flow.
  ##
  ## think of things like `return` and `if: discard` and the end of a
  ## while loop
  e.gotos.add (k.kind, n, newEmptyNode())

proc addBreak*(e: var Env; k: NimNode; n: NimNode) =
  ## these are breaks, which are like gotos but also not.
  ##
  ## we need to keep track of them separately for hysterical reasons.
  e.breaks.add (k.kind, n, k.breakName)

proc popGoto*(e: var Env): NimNode =
  ## pop a goto proc off the stack; return its node
  pop(e.gotos).node

proc popBreak*(e: var Env): NimNode =
  ## same thing, but for breaks
  pop(e.breaks).node

proc insideFor*(e: Env): bool =
  ## does what it says on the tin, and does it well, i might add
  lastBreakLoop(e).kind == nnkForStmt

proc insideWhile*(e: Env): bool =
  ## actually, this is the better one.
  lastBreakLoop(e).kind == nnkWhileStmt

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
    result = len(e.locals)

proc isEmpty*(e: Env): bool =
  result = len(e) == 0

proc inherits*(e: Env): NimNode =
  assert not e.isNil
  assert not e.via.isNil
  assert not e.via.isEmpty
  result = if e.parent.isNil: e.via else: e.parent.id

proc isDirty*(e: Env): bool =
  ## the type hasn't been written since an add occurred
  assert not e.isNil
  result = if e.parent.isNil: len(e) > 0 else: e.id != e.parent.id
  # a dirty parent yields a dirty child
  result = result or (not e.parent.isNil and e.parent.isDirty)

proc identity*(e: Env): NimNode =
  assert not e.isNil
  #assert not e.isDirty
  result = e.id

proc setDirty(e: var Env) =
  when false:
    assert not e.isNil
    e.id = newEmptyNode()
    assert e.isDirty

proc root*(e: Env): NimNode =
  var r = e
  while not r.parent.isNil:
    r = r.parent
  result = r.inherits

proc init[T](c: T; l: LineInfo): T {.used.} =
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

var c {.compiletime.}: int
proc init(e: var Env) =
  if e.fn.isNil:
    e.fn = genSym(nskField, "fn" & $c)
  if e.ex.isNil:
    e.ex = genSym(nskField, "ex" & $c)
  if e.rs.isNil:
    e.rs = genSym(nskField, "rs" & $c)
  e.id = genSym(nskType, "env" & $c)
  inc c

proc allPairs(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.locals)
    result.add allPairs(e.parent)

iterator pairs(e: Env): Pair =
  assert not e.isNil
  var seen = initHashSet[string](len(e.locals))
  var p = e
  while not p.isNil:
    for key, val in pairs(p.locals):
      if not seen.containsOrIncl key.strVal:
        yield (key: key, val: val)
    p = p.parent

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in pairs(e.locals):
    for defs in items(section):
      if defs[1].isEmpty:
        error "give " & $name & " a type: " & repr(section)
      else:
        # name is an ident or symbol
        n.add newIdentDefs(name, defs[1])

template cpsLift*() {.pragma.}

proc contains*(e: Env; key: NimNode): bool =
  ## you're giving us a symbol|ident and we're telling you if we have it
  ## recorded with that name.
  assert not key.isNil
  assert key.kind in {nnkSym, nnkIdent}
  result = key.strVal in e.seen

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  when false:
    # i'm tried of looking at these gratuitous pragmas
    var pragma = nnkPragma.newTree bindSym"cpsLift"
  else:
    var pragma = newEmptyNode()
  var record = nnkRecList.newNimNode(e.identity)
  populateType(e, record)
  var parent = nnkOfInherit.newNimNode(e.root).add e.inherits
  result = nnkRefTy.newTree nnkObjectTy.newTree(pragma, parent, record)

proc `==`(a, b: Env): bool = a.seen == b.seen
proc `<`(a, b: Env): bool = a.seen < b.seen
proc `*`(a, b: Env): HashSet[string] = a.seen * b.seen

proc reparent(e: var Env; p: Env) =
  ## set all nodes to have a parent at least as large as p
  if not e.isNil:
    if e < p:
      # p is a superset of us; suggest it to our parents
      reparent(e.parent, p)
      if e.parent < p:
        # and then set it as our parent if it's better
        e.parent = p
    else:
      # offer ourselves to our parent instead
      reparent(e.parent, e)

proc makeType*(e: var Env): NimNode =
  ## turn an env into a named object typedef `foo = object ...`

  # determine if a symbol clash necessitates pointing to a new parent
  #performReparent(e)

  result = nnkTypeDef.newTree(e.id, newEmptyNode(), e.objectType)
  #assert not e.isDirty

proc first*(e: Env): NimNode = e.c
proc firstDef*(e: Env): NimNode = newIdentDefs(e.c, e.via, newNilLit())

proc newEnv*(parent: var Env; copy = off): Env =
  ## this is called as part of the recursion in the front-end,
  ## or on-demand in the back-end (with copy = on)
  assert not parent.isNil
  if copy:
    result = Env(store: parent.store,
                 via: parent.identity,
                 seen: parent.seen,
                 locals: parent.locals,
                 c: parent.c,
                 ex: parent.ex,
                 rs: parent.rs,
                 fn: parent.fn,
                 parent: parent)
    init result
  else:
    result = parent

proc storeType*(e: var Env; force = off): Env =
  ## turn an env into a complete typedef in a type section
  assert not e.isNil
  if force or e.isDirty:
    if not e.parent.isNil:
      assert not e.parent.isDirty
      # must store the parent for inheritance ordering reasons;
      # also, we don't want it to be changed under our feet.
      e.via = e.parent.id
      e.parent = e.parent.storeType
    e.store.add nnkTypeSection.newTree e.makeType
    # clearly, if we ever write again, we want it to be a new type
    result = newEnv(e)
    e = result
  else:
    result = e
    assert not e.isDirty

proc set(e: var Env; key: NimNode; val: NimNode): Env =
  ## set [ident|sym] = let/var section
  assert key.kind in {nnkSym, nnkIdent}
  assert val.kind in {nnkVarSection, nnkLetSection}
  if key in e.locals:
    result = e.storeType
  else:
    result = e
  e.locals[key] = val
  e.seen.incl repr(key) # repr the key to get the genSym'd string
  setDirty e

proc stripVar(n: NimNode): NimNode =
  ## pull the type out of a VarTy
  result = if n.kind == nnkVarTy: n[0] else: n

proc addIdentDef(e: var Env; kind: NimNodeKind; n: NimNode) =
  ## add `a, b, c: type = default` to the env
  case n.kind
  of nnkIdentDefs:
    if len(n) == 2:
      # ident: type; we'll add a default for numbering reasons
      n.add newEmptyNode()
    # iterate over the identifier names (a, b, c)
    for name in n[0 ..< len(n)-2]:  # ie. omit (:type) and (=default)
      # create a new identifier for the object field
      var field =
        # symbols (probably gensym'd?) flow through...  think ex, fn
        if name.kind == nnkSym:
          name
        else:
          genSym(nskField, name.strVal)
      e = e.set(field, newTree(kind,     # ident: <no var> type = default
                               newIdentDefs(name, stripVar(n[^2]), n[^1])))
  #[
  of nnkVarTuple:
    assert n.last.kind == nnkPar, "expected parenthesis: " & repr(n)
    let par = n.last
    for i in 0 ..< len(par):
      let name = n[i]
      e[name] = newTree(n.kind,
                        newIdentDefs(name, getTypeInst(par[i]), par[i]))
  ]#
  else:
    error $n.kind & " is unsupported by cps: \n" & treeRepr(n)

proc letOrVar(n: NimNode): NimNodeKind =
  ## choose between let or var for proc parameters
  assert n.kind == nnkIdentDefs
  if len(n) == 2:
    # ident: type; we'll add a default for numbering reasons
    n.add newEmptyNode()
  case n[^2].kind
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection
  else:
    result = nnkLetSection

proc add*(e: var Env; n: NimNode) =
  ## add a let/var section or proc param to the env
  try:
    case n.kind
    of nnkVarSection, nnkLetSection:
      for defs in items(n):
        e.addIdentDef(n.kind, defs)
    of nnkIdentDefs:
      e.addIdentDef(letOrVar(n), n)
    else:
      raise newException(Defect, "unrecognized input node " & repr(n))
  finally:
    e.setDirty

proc newEnv*(c: NimNode; store: var NimNode; via: NimNode): Env =
  ## the initial version of the environment
  assert not via.isNil
  assert not via.isEmpty
  result = Env(c: c, store: store, via: via, id: via)
  init result
  result.add newIdentDefs(result.fn,
                          newTree(nnkProcTy,
                                  newTree(nnkFormalParams,
                                    via,
                                    result.firstDef()),
                                  newEmptyNode()))
  result.add newIdentDefs(result.ex,
                          nnkRefTy.newTree(ident"CatchableError"))

proc identity*(e: var Env): NimNode =
  assert not e.isNil
  assert not e.id.isNil
  assert not e.id.isEmpty
  result = e.id

iterator localRetrievals*(e: Env; locals: NimNode): Pair =
  ## read locals out of an env
  let locals = e.castToChild(locals)
  for field, value in pairs(e):
    # we skip special fields here
    if repr(field) notin [repr(e.fn), repr(e.ex)]:
      # remake the section; we use locals here only for line info
      let section = newNimNode(value.kind, locals)
      # the name of this local is the first field of the only val
      assert len(value) == 1

      # recreate the name to cast a field symbol into a local value
      let name = ident(repr(value[0][0]))

      when true:
        let tmpl = nnkTemplateDef.newTree(name, newEmptyNode(),
                                          newEmptyNode(), newEmptyNode(),
                                          newEmptyNode(), newEmptyNode(),
                                          newDotExpr(locals, field))
        yield (key: name, val: tmpl)
      else:
        section.add newIdentDefs(name, value[0][1],
                                 # basically, `name: int = locals.field`
                                 newDotExpr(locals, field))
        yield (key: name, val: section)

proc defineLocals*(e: var Env; goto: NimNode): NimNode =
  result = nnkObjConstr.newTree(e.identity,
                                newColonExpr(e.fn, goto))
  for field, section in pairs(e):
    if repr(field) notin [repr(e.fn), repr(e.ex)]:
      # the name from identdefs is not gensym'd (usually!)
      let name = section.last[0]

      # specify the gensym'd field name and the local name
      result.add newColonExpr(field, name)

  # setting the label here allows us to use it in trace composition
  e.label = goto
  e = e.storeType

template withGoto*(f: NimNodeKind; n: NimNode; body: untyped): untyped {.dirty.} =
  ## run a body with a longer gotos stack
  if len(stripComments n) > 0:
    env.gotos.add (kind: f, node: n, name: newEmptyNode())
    try:
      body
    finally:
      result.add pop(env.gotos).node
  else:
    body

proc wrapProcBody*(e: var Env; locals: NimNode; n: NimNode): NimNode =
  # wrap gives us a scope above which to install our env; that lets
  # the scope inside the `try:` shadow existing locals.
  var wrap = nnkTryStmt.newNimNode(n)

  # add the body to the try/finally
  wrap.add n
  wrap.add newTree(nnkExceptBranch, ident"CatchableError")
      .add newTree(nnkStmtList,
                   doc"we probably want to do this in the finally below",
                   # stash the current exception
                   newAssignment(newDotExpr(ident"result", e.ex),
                                 newCall(ident"getCurrentException")),
                   # raise (re-raise) the exception
                   nnkRaiseStmt.newNimNode(n).add newEmptyNode())

  # add a finally clause that merely issues an empty discard
  # XXX: we'll hook cpsTrace here...
  wrap.add nnkFinally.newNimNode(n)
      .add nnkDiscardStmt.newNimNode(n)
      .add newEmptyNode()

  # we'll use a statement list as the body, around the try/finally
  result = newStmtList(wrap)
  # to that list, we will add the local variables in scope
  for name, asgn in localRetrievals(e, locals):
    result.insert(0, asgn)
  result.insert(0, doc "installing locals for " & $e.identity)
