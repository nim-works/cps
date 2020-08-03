import std/sets
import std/sequtils
import std/hashes
import std/tables
import std/macros
import std/algorithm

{.experimental: "dynamicBindSym".}

import cps/scopes

const
  cpsZevv {.booldefine.} = true   ## increment gensyms
  cpsCast {.booldefine.} = false
  cpsDebug {.booldefine.} = false
  cpsTrace {.booldefine.} = false
  cpsExcept {.booldefine.} = false
  cpsFn {.booldefine.} = false
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

  # the idents|symbols and the typedefs they refer to in order of discovery
  LocalCache = OrderedTable[NimNode, NimNode]

  Env* = ref object
    id: NimNode                     # the identifier of our continuation type
    via: NimNode                    # the identifier of the type we inherit
    parent: Env                     # the parent environment (scope)
    locals: LocalCache              # locals and their typedefs|generics
    gotos: Scopes                   # identifiers of scope gotos
    breaks: Scopes                  # identifiers of scope breaks
    store: NimNode                  # where to put typedefs, a stmtlist
    camefrom: Scope                 # the last tailcall (goto)
    seen: HashSet[string]           # count/measure idents/syms by string

    scope: Scope                    # current scope

    # special symbols for cps machinery
    c: NimNode                      # the sym we use for the continuation
    fn: NimNode                     # the sym we use for the goto target
    ex: NimNode                     # the sym we use for stored exception
    rs: NimNode                     # the sym we use for "yielded" result

when cpsDebug:
  template twice*(body: untyped): untyped =
    ## an exercise in futility, apparently
    block twice:
      once:
        break twice
      body

  proc `$`(p: Pair): string =
    result = p[0].repr & ": " & p[1].repr

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

template searchScope(env: Env; x: untyped;
                     p: proc(ns: Scopes): Scope): Scope =
  var e = env
  var r = newScope()
  while not e.isNil:
    r = p(e.`x`)
    if r.kind == nnkNilLit:
      e = e.parent
    else:
      break
  r

func lastGotoLoop*(e: Env): Scope = searchScope(e, gotos, last)
func lastBreakLoop*(e: Env): Scope = searchScope(e, breaks, last)

func nextGoto*(e: Env): Scope = searchScope(e, gotos, next)
func nextBreak*(e: Env): Scope = searchScope(e, breaks, next)

proc addGoto*(e: var Env; k: NimNode; n: NimNode) =
  ## add to a stack of gotos, which are normal exits from control flow.
  ##
  ## think of things like `return` and `if: discard` and the end of a
  ## while loop
  e.gotos.add(k, n)

proc addBreak*(e: var Env; scope: Scope) =
  ## it's nice when we can do this simply
  e.breaks.add scope

proc addBreak*(e: var Env; k: NimNode; n: NimNode) {.deprecated.} =
  ## these are breaks, which are like gotos but also not.
  ##
  ## we need to keep track of them separately for hysterical reasons.
  e.breaks.add(k, n)

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

proc topOfWhile*(e: Env): Scope =
  ## fetch the goto target in order to `continue` inside `while:`
  assert e.insideWhile, "i thought i was in a while loop"
  result = lastGotoLoop(e)
  assert result.kind == nnkWhileStmt, "goto doesn't match break"

proc namedBreak*(e: Env; n: NimNode): Scope =
  ## fetch the goto target in order to `break foo`
  assert n.kind == nnkBreakStmt
  if len(n) == 0:
    result = e.nextBreak
  else:
    proc match(ns: Scopes): Scope =
      ## find the loop matching the requested named break
      result = newScope()
      for i in countDown(ns.high, ns.low):
        if ns[i].kind in {nnkBlockStmt} and eqIdent(ns[i].label, n[0]):
          result = ns[i]
          break
    result = searchScope(e, breaks, match)

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

proc identity*(e: Env): NimNode =
  assert not e.isNil
  #assert not e.isDirty
  result = e.id

proc isWritten(e: Env): bool =
  ## why say in three lines what you can say in six?
  block found:
    for section in items(e.store):
      for def in items(section):
        result = repr(def[0]) == repr(e.identity)
        if result:
          break found

proc isDirty*(e: Env): bool =
  ## the type hasn't been written since an add occurred
  when false:
    assert not e.isNil
    result = if e.parent.isNil: len(e) > 0 else: e.id != e.parent.id
    # a dirty parent yields a dirty child
    result = result or (not e.parent.isNil and e.parent.isDirty)
  else:
    result = not e.isWritten

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
    if e.camefrom.isNil or e.camefrom.isEmpty:
      "nil"
    else:
      repr(e.camefrom.returnTo)
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
    when cpsFn:
      e.fn = genSym(nskField, "fn" & $c)
    else:
      e.fn = ident"fn"
  if e.ex.isNil:
    e.ex = genSym(nskField, "ex" & $c)
  if e.rs.isNil:
    e.rs = genSym(nskField, "rs" & $c)
  e.id = genSym(nskType, "env" & $c)
  inc c

proc allPairs(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq pairs(e.locals)
    # most-recently-defined comes first
    reverse result
    # add any inherited types from the parent
    result.add allPairs(e.parent)

proc definedName(n: NimNode): NimNode =
  ## create an identifier from an typesection/identDef as cached;
  ## this is a copy and it is repr'd to ensure gensym compat...
  assert n.kind in {nnkVarSection, nnkLetSection}, "use this on env[key]"
  result = ident(repr(n[0][0]))

iterator pairs(e: Env): Pair =
  assert not e.isNil
  var seen = initHashSet[string]()
  for pair in e.allPairs:
    # make sure we're actually measuring gensyms for collision
    if not seen.containsOrIncl definedName(pair.val).strVal:
      yield pair

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
proc firstDef*(e: Env): NimNode = newIdentDefs(e.first, e.via, newNilLit())

proc newEnv*(parent: var Env; copy = off): Env =
  ## this is called as part of the recursion in the front-end,
  ## or on-demand in the back-end (with copy = on)
  assert not parent.isNil
  if copy:
    var scope = newScope()
    scope.goto = parent.scope
    scope.brake = parent.scope
    result = Env(store: parent.store,
                 via: parent.identity,
                 seen: parent.seen,
                 locals: parent.locals,
                 c: parent.c,
                 ex: parent.ex,
                 rs: parent.rs,
                 fn: parent.fn,
                 scope: scope,
                 parent: parent)
    init result
  else:
    result = parent

proc makeFnType(e: Env): NimNode =
  # ensure that the continuation type is not nillable
  var continuation = e.firstDef
  continuation[^1] = newEmptyNode()
  result = newTree(nnkProcTy,
                   newTree(nnkFormalParams,
                           e.via, continuation), newEmptyNode())

proc makeFnGetter(e: Env): NimNode =
  result = newProc(ident"fn",
                   params = [e.makeFnType, newIdentDefs(ident"c",
                                                        e.identity)],
                   pragmas = nnkPragma.newTree bindSym"cpsLift",
                   body = newDotExpr(ident"c", e.fn))

proc storeType*(e: var Env; force = off): Env =
  ## turn an env into a complete typedef in a type section
  assert not e.isNil
  if e.isDirty:
    if not e.parent.isNil:
      assert not e.parent.isDirty
      # must store the parent for inheritance ordering reasons;
      # also, we don't want it to be changed under our feet.
      e.via = e.parent.id
      e.parent = e.parent.storeType
    e.store.add nnkTypeSection.newTree e.makeType
    when cpsFn:
      e.store.add e.makeFnGetter
    when cpsDebug:
      echo "storing type ", $e.identity
    # clearly, if we ever write again, we want it to be a new type
    result = newEnv(e)
    e = result
    when cpsDebug:
      echo "next type ", $e.identity
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
  result.locals[key] = val
  result.seen.incl key.strVal
  setDirty result

proc stripVar(n: NimNode): NimNode =
  ## pull the type out of a VarTy
  result = if n.kind == nnkVarTy: n[0] else: n

iterator addIdentDef(e: var Env; kind: NimNodeKind; n: NimNode): Pair =
  ## add `a, b, c: type = default` to the env;
  ## yields pairs of field, value as added
  case n.kind
  of nnkIdentDefs:
    if len(n) == 2:
      # ident: type; we'll add a default for numbering reasons
      n.add newEmptyNode()
    # iterate over the identifier names (a, b, c)
    for name in n[0 ..< len(n)-2]:  # ie. omit (:type) and (=default)
      # the workaround that zevv demanded repeatedly
      inc c
      # create a new identifier for the object field
      var field =
        # symbols (probably gensym'd?) flow through...  think ex, rs
        if name.kind == nnkSym:
          name
        else:
          when cpsZevv:
            genSym(nskField, name.strVal & $c)
          else:
            genSym(nskField, name.strVal)
      var value = newTree(kind,     # ident: <no var> type = default
                          newIdentDefs(name, stripVar(n[^2]), n[^1]))
      e = e.set(field, value)
      yield (key: field, val: value)
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

proc newEnv*(c: NimNode; store: var NimNode; via: NimNode): Env =
  ## the initial version of the environment
  assert not via.isNil
  assert not via.isEmpty
  var c = if c.isNil or c.isEmpty: ident"continuation" else: c
  result = Env(c: c, store: store, via: via, id: via)
  result.scope = newScope()
  init result
  when cpsFn:
    result.add newIdentDefs(result.fn, makeFnType(result))
  when cpsExcept:
    result.add newIdentDefs(result.ex,
                            nnkRefTy.newTree(ident"CatchableError"))

proc identity*(e: var Env): NimNode =
  assert not e.isNil
  assert not e.id.isNil
  assert not e.id.isEmpty
  result = e.id

proc rootTemplate*(e: Env): NimNode =
  ## the template used to rename `result` in .cps. procs
  result = nnkTemplateDef.newTree(e.first, newEmptyNode(),
                                  newEmptyNode(), newEmptyNode(),
                                  newEmptyNode(), newEmptyNode(),
                                  ident"result")

proc makeTemplate(e: Env; name: NimNode; field: NimNode): NimNode =
  let locals = e.castToChild(e.first)
  result = nnkTemplateDef.newTree(name, newEmptyNode(),
                                  newEmptyNode(), newEmptyNode(),
                                  newEmptyNode(), newEmptyNode(),
                                  newDotExpr(locals, field))

proc initialization(e: Env; kind: NimNodeKind;
                    field: NimNode; value: NimNode): NimNode =
  ## produce the `x = 34` appropriate given the field and identDefs
  assert kind in {nnkVarSection, nnkLetSection, nnkIdentDefs}

  # the defined name is composed from the let|var section
  let name = definedName(value)

  # start with the template, and then maybe add an initialization
  result = newStmtList(e.makeTemplate(name, field))

  # don't attempt to redefine proc params!
  if kind in {nnkVarSection, nnkLetSection}:
    # search first|only typedefs in a var/let section
    let defs = value[0]
    if len(defs) > 2 and not defs.last.isEmpty:
      result.add newAssignment(name, defs.last)

iterator addAssignment(e: var Env; kind: NimNodeKind;
                       defs: NimNode): Pair =
  ## compose template and assignment during addition of identDefs to env
  assert kind in {nnkVarSection, nnkLetSection, nnkIdentDefs}
  let section =
    if kind in {nnkVarSection, nnkLetSection}:
      kind
    else:
      letOrVar(defs)
  for field, value in e.addIdentDef(section, defs):
    let name = definedName(value)
    yield (key: name, val: e.initialization(kind, field, value))

iterator localSection*(e: var Env; n: NimNode): Pair =
  ## consume a var|let section and yield name, node pairs
  ## representing assignments to local scope; these are
  ## templates in the current incarnation...

  ## add a let/var section or proc param to the env
  try:
    when cpsDebug:
      echo "local section ", $n.kind, "\n", repr(n)
    case n.kind
    of nnkVarSection, nnkLetSection:
      for defs in items(n):
        for pair in e.addAssignment(n.kind, defs):
          yield pair
    of nnkIdentDefs:
      for pair in e.addAssignment(n.kind, n):
        yield pair
    else:
      raise newException(Defect, "unrecognized input node " & repr(n))
  finally:
    e.setDirty

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
      let name = definedName(value)

      when true:
        when defined(release):
          yield (key: name, val: e.makeTemplate(name, field))
        else:
          yield (key: name, val: nnkCall.newTree(ident"installLocal", name,
                                                 e.identity, field))
      else:
        section.add newIdentDefs(name, value[0][1],
                                 # basically, `name: int = locals.field`
                                 newDotExpr(locals, field))
        yield (key: name, val: section)

proc newContinuation*(e: Env; via: NimNode; goto: NimNode): NimNode =
  ## else, perform the following alloc...
  result = nnkObjConstr.newTree(e.identity, newColonExpr(e.fn, goto))
  for field, section in pairs(e):
    if repr(field) notin [repr(e.fn), repr(e.ex)]:
      # the name from identdefs is not gensym'd (usually!)
      let name = section.last[0]

      # specify the gensym'd field name and the local name
      result.add newColonExpr(field, name)

proc rootResult*(e: Env; name: NimNode): NimNode =
  result = newAssignment(name, e.newContinuation(e.first, newNilLit()))

proc defineLocals*(e: var Env; goto: NimNode): NimNode =
  # setup the continuation for a tail call to a possibly new environment
  var ctor = e.newContinuation(e.identity, goto)

  if e.first.isNil or e.first.isEmpty:
    # we don't have a local continuation; just use the ctor we built
    result = ctor
  else:
    # when we can reuse the continuation, we'll merely set the fn pointer
    var reuse = newStmtList(
      doc"re-use the local continuation by setting the fn",
      newAssignment(newDotExpr(e.first, e.fn), goto),
      e.first)
    when true:
      # FIXME: we currently cheat.
      result = reuse
    else:
      # this when statement returns an e.identity one way or another
      result = nnkWhenStmt.newNimNode
      result.add nnkElifBranch.newTree(
        newCall(ident"compiles", e.first),
        # compare e.first to e.identity; if the types are the same, then we
        # can reuse the existing type and not create a new object.
        newStmtList(
          newTree(nnkCommand, ident"echo",
            newTree(nnkCall, ident"typeof", ident"continuation")),
          newTree(nnkWhenStmt,
                  newTree(nnkElifBranch, infix(e.first, "is", e.identity), reuse),
                  newTree(nnkElse, ctor)),
        )
      )
      # else, use the ctor we just built
      result.add nnkElse.newTree(ctor)

  # record the last-rendered scope for use in trace composition
  e.camefrom = newScope(result, goto, result)

  # we store the type whenever we define locals, because the next code that
  # executes may need to cut a new type.  to put this another way, a later
  # scope may add a name that clashes with a scope ancestor of ours that is
  # only a peer of the later scope...
  e = e.storeType

template withGoto*(f: Scope; body: untyped): untyped {.dirty.} =
  ## run a body with a longer gotos stack
  if len(stripComments f.node) > 0:
    env.gotos.add(f)
    try:
      body
    finally:
      result.add pop(env.gotos).node
  else:
    body

template withBreak*(s: Scope; body: untyped): untyped {.dirty.} =
  ## run a body with a longer breaks stack
  if len(stripComments s.node) > 0:
    env.breaks.add(s)
    try:
      body
    finally:
      result.add pop(env.breaks).node
  else:
    body

proc wrapProcBody*(e: var Env; locals: NimNode; n: NimNode): NimNode =
  # return a proc body that defines the locals in a scope above the
  # original body; this lets the lower scope shadow existing locals or
  # proc parameters.
  #
  # except that we don't use multiple scopes anymore.  we just use templates.

  when cpsExcept:
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
  else:
    var wrap = n

  # we'll use a statement list as the body
  result = newStmtList(doc("done locals for " & $e.identity), wrap)
  # to that list, we will insert the local variables in scope
  for name, asgn in localRetrievals(e, locals):
    result.insert(0, asgn)
  result.insert(0, doc "installing locals for " & $e.identity)
