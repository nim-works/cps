##[

The Env(ironment) tracks continuation types and the variables of which
they are comprised.

]##
import std/sets
import std/sequtils
import std/hashes
import std/tables
import std/macros
import std/algorithm

{.experimental: "dynamicBindSym".}

import cps/spec
import cps/scopes

type
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

proc lastGotoLoop*(e: Env): Scope = searchScope(e, gotos, last)
proc lastBreakLoop*(e: Env): Scope = searchScope(e, breaks, last)

proc nextGoto*(e: Env): Scope = searchScope(e, gotos, next)
proc nextBreak*(e: Env): Scope = searchScope(e, breaks, next)

proc addGoto*(e: var Env; scope: Scope) =
  ## it's nice when we can do this simply
  e.gotos.add scope

proc addGoto*(e: var Env; k: NimNode; n: NimNode) =
  ## add to a stack of gotos, which are normal exits from control flow.
  ##
  ## think of things like `return` and `if: discard` and the end of a
  ## while loop
  e.gotos.add(k, n)

proc addBreak*(e: var Env; scope: Scope) =
  ## it's nice when we can do this simply
  e.breaks.add scope

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
  result = e.id

proc isWritten(e: Env): bool =
  ## why say in three lines what you can say in six?
  block found:
    for section in items(e.store):
      if section.kind == nnkTypeSection:
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
    result = not(e.isWritten) or (not(e.parent.isNil) and e.parent.isDirty)

proc root*(e: Env): NimNode =
  var r = e
  while not r.parent.isNil:
    r = r.parent
  result = r.inherits

when cpsTrace:
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
  result = newTree(nnkCall, e.root, n)
  when cpsTrace:
    result = e.addTrace(result)

proc castToChild(e: Env; n: NimNode): NimNode =
  when cpsTrace:
    var n = e.addTrace(n)
  result = newTree(nnkCall, e.identity, n)

proc maybeConvertToRoot*(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    e.castToRoot(locals)
  else:
    locals

proc init(e: var Env) =
  e.seen = initHashSet[string]()
  if e.fn.isNil:
    e.fn = ident"fn"
  if e.ex.isNil:
    e.ex = genField("ex")
  if e.rs.isNil:
    e.rs = genField("result")
  e.id = genSym(nskType, "env")

proc allPairs(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq e.locals.pairs
    # most-recently-defined comes first
    reverse result
    # add any inherited types from the parent
    result.add allPairs(e.parent)

iterator pairs(e: Env): Pair =
  assert not e.isNil
  var seen = initHashSet[string]()
  for pair in e.allPairs:
    # make sure we're actually measuring gensyms for collision
    if not seen.containsOrIncl definedName(pair.val).strVal:
      yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in e.locals.pairs:
    for defs in section.items:
      if defs[1].isEmpty:
        # get the type of the assignment
        n.add:
          newIdentDefs(name, getTypeImpl(defs.last), newEmptyNode())
      else:
        # name is an ident or symbol
        n.add:
          newIdentDefs(name, defs[1], newEmptyNode())

proc contains*(e: Env; key: NimNode): bool =
  ## you're giving us a symbol|ident and we're telling you if we have it
  ## recorded with that name.
  assert not key.isNil
  assert key.kind in {nnkSym, nnkIdent}
  result = key.strVal in e.seen

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  var pragma = newEmptyNode()
  var record = nnkRecList.newNimNode(e.identity)
  populateType(e, record)
  var parent = nnkOfInherit.newNimNode(e.root).add e.inherits
  result = nnkRefTy.newTree nnkObjectTy.newTree(pragma, parent, record)

proc `==`(a, b: Env): bool {.deprecated, used.} = a.seen == b.seen
proc `<`(a, b: Env): bool = a.seen < b.seen
proc `*`(a, b: Env): HashSet[string] {.deprecated, used.} = a.seen * b.seen

proc reparent(e: var Env; p: Env) =
  ## set all nodes to have a parent at least as large as p
  if not e.isNil:
    if e < p:
      # p is a superset of us; suggest it to our parents
      reparent(e.parent, p)
      if not e.parent.isNil:
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

proc first*(e: Env): NimNode = e.c

proc firstDef*(e: Env): NimNode =
  newIdentDefs(e.first, e.via, newEmptyNode())

proc get*(e: Env): NimNode =
  ## retrieve a continuation's result value from the env
  newDotExpr(e.castToChild(e.first), e.rs)

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
                 locals: initOrderedTable[NimNode, NimNode](),
                 c: parent.c,
                 ex: parent.ex,
                 rs: parent.rs,
                 fn: parent.fn,
                 gotos: parent.gotos,
                 breaks: parent.breaks,
                 scope: scope,
                 parent: parent)
    init result
  else:
    result = parent

when defined(whenyouwishuponastarmakesnodifferencewhoyouare):
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
    if force:
      reparent(e, e)
    if not e.parent.isNil:
      assert not e.parent.isDirty
      # must store the parent for inheritance ordering reasons;
      # also, we don't want it to be changed under our feet.
      assert e.via == e.parent.id
      e.parent = e.parent.storeType
    e.store.add nnkTypeSection.newTree e.makeType
    when cpsDebug:
      echo "storing type ", repr(e.identity)
    # clearly, if we ever write again, we want it to be a new type
    result = newEnv(e, copy = on)
    e = result
    when cpsDebug:
      echo "next type ", repr(e.identity)
  else:
    result = e
    assert not e.isDirty

proc set(e: var Env; key: NimNode; val: NimNode): Env =
  ## set [ident|sym] = let/var section
  assert key.kind in {nnkSym, nnkIdent}
  assert val.kind in {nnkVarSection, nnkLetSection}
  assert val.len == 1, "too large a section"
  assert val[0].len == 3, "too small an identdefs"
  if key in e.locals:
    result = e.storeType
  else:
    result = e
  result.locals[key] = val
  result.seen.incl key.strVal

iterator addIdentDef(e: var Env; kind: NimNodeKind; n: NimNode): Pair =
  ## add `a, b, c: type = default` to the env;
  ## yields pairs of field, value as added
  case n.kind
  of nnkIdentDefs:
    if len(n) == 2:
      # ident: type; we'll add a default for numbering reasons
      n.add newEmptyNode()
    if n[0].kind notin {nnkIdent, nnkSym}:
      error "bad rewrite presented\n" & $kind & ": " & repr(n), n
    else:
      # iterate over the identifier names (a, b, c)
      for name in n[0 ..< len(n)-2]:  # ie. omit (:type) and (=default)
        # create a new identifier for the object field
        let field = genField(name.strVal)
        let value = newTree(kind,     # ident: <no var> type = default
                            newIdentDefs(name, stripVar(n[^2]), n[^1]))
        e = e.set(field, value)
        yield (key: field, val: value)
  of nnkVarTuple:
    # transform tuple to section
    assert n.last.kind == nnkTupleConstr, "expected tuple: " & treeRepr(n)
    let tup = n.last
    for i in 0 ..< len(tup):
      let name = n[i]
      let field = genField(name.strVal)
      let value = newTree(kind,
                          newIdentDefs(name, getTypeInst(tup[i]), tup[i]))
      e = e.set(field, value)
      yield (key: field, val: value)
  else:
    error $n.kind & " is unsupported by cps: \n" & treeRepr(n)

proc newEnv*(c: NimNode; store: var NimNode; via: NimNode): Env =
  ## the initial version of the environment
  assert not via.isNil
  assert not via.isEmpty
  var c = if c.isNil or c.isEmpty: ident"continuation" else: c
  result = Env(c: c, store: store, via: via, id: via)
  result.seen = initHashSet[string]()
  result.scope = newScope()
  init result

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

proc initialization(e: Env; kind: NimNodeKind;
                    field: NimNode; value: NimNode): NimNode =
  ## produce the `x = 34` appropriate given the field and identDefs
  assert kind in {nnkVarSection, nnkLetSection, nnkIdentDefs}

  result = newStmtList()

  # this is our continuation type, fully cast
  let child = e.castToChild(e.first)

  # don't attempt to redefine proc params!
  if kind in {nnkVarSection, nnkLetSection}:
    # search first|only typedefs in a var/let section
    let defs = value[0]
    if len(defs) > 2 and not defs.last.isEmpty:
      # this is basically env2323(cont).foo34 = "some default"
      result.add newAssignment(newDotExpr(child, field), defs.last)

iterator addAssignment(e: var Env; kind: NimNodeKind; defs: NimNode): NimNode =
  ## compose an assignment during addition of identDefs to env
  assert kind in {nnkVarSection, nnkLetSection, nnkIdentDefs}
  let section =
    if kind in {nnkVarSection, nnkLetSection}:
      kind
    else:
      letOrVar(defs)
  for field, value in e.addIdentDef(section, defs):
    let name = definedName(value)
    when cpsDebug:
      echo $kind, "\t", repr(defs)
    yield e.initialization(kind, field, value)

proc getFieldViaLocal(e: Env; n: NimNode): NimNode =
  ## get a field from the env using a local symbol as input
  for field, sym in e.locals.pairs:
    if sym == n:
      result = field
      break
  if result.isNil:
    result = n.errorAst "unable to find field for symbol " & n.repr

proc findJustOneAssignmentName*(e: Env; n: NimNode): NimNode =
  case n.kind
  of nnkVarSection, nnkLetSection:
    if n.len != 1:
      n.errorAst "only one assignment per section is supported"
    else:
      e.findJustOneAssignmentName n[0]
  of nnkIdentDefs:
    if n[0].kind notin {nnkIdent, nnkSym}:
      n.errorAst "bad rewrite presented bogus input"
    elif n.len != 3:
      n.errorAst "only one identifier per assignment is supported"
    else:
      e.getFieldViaLocal n
  of nnkVarTuple:
    n.errorAst "tuples not supported yet"
  else:
    n.errorAst "unrecognized input"

proc localSection*(e: var Env; n: NimNode; into: NimNode = nil) =
  ## consume a var|let section and yield name, node pairs
  ## representing assignments to local scope
  template maybeAdd(x) =
    if not into.isNil:
      into.add x

  case n.kind
  of nnkVarSection, nnkLetSection:
    if n.len != 1:
      e.store.add:
        n.errorAst "expected only one var/let section member"
    else:
      for defs in n.items:
        if defs.kind == nnkVarTuple:
          # deconstruct the RHS types into multiple assignments
          # let (a, b, c) = foo() -> (env.a, env.b, env.c) = foo()
          let child = e.castToChild(e.first)
          let rhs = getTypeInst defs.last
          var tups = nnkTupleConstr.newTree
          for index, name in defs[0 ..< defs.len-2].pairs:
            let entry = newIdentDefs(name, rhs[index], newEmptyNode())
            # we need to insert the variable and then write a new
            # accessor that plucks the field from the env
            for field, value in e.addIdentDef(n.kind, entry):
              tups.add newDotExpr(child, field)
          maybeAdd newAssignment(tups, defs.last)
        else:
          # an iterator handles `var a, b, c = 3` appropriately
          for assignment in e.addAssignment(n.kind, defs):
            maybeAdd assignment
  of nnkIdentDefs:
    for assignment in e.addAssignment(n.kind, n):
      maybeAdd assignment
  else:
    e.store.add:
      n.errorAst "localSection input"

proc newContinuation*(e: Env; via: NimNode;
                      goto: NimNode; defaults = false): NimNode =
  ## else, perform the following alloc...
  result = nnkObjConstr.newTree(e.identity, newColonExpr(e.fn, goto))
  for field, section in pairs(e):
    # omit special fields in the env that we use for holding
    # custom functions, results, and exceptions, respectively
    if field notin [e.fn, e.rs, e.ex]:
      let defs = section.last
      if defaults:
        # initialize the field with any default supplied in its declaration
        if not defs.last.isEmpty:
          # only initialize a field that has a default
          # FIXME: this needs to only add requiresInit stuff
          result.add newColonExpr(field, defs.last)
      else:
        # the name from identdefs is not gensym'd (usually!)
        let name = defs[0]

        # specify the gensym'd field name and the local name
        assert name.kind == nnkSym, "expecting a symbol for " & repr(name)
        result.add newColonExpr(field, name)

proc defineLocals*(e: var Env; into: var NimNode; goto: NimNode): NimNode =
  # we store the type whenever we define locals, because the next code that
  # executes may need to cut a new type.  to put this another way, a later
  # scope may add a name that clashes with a scope ancestor of ours that is
  # only a peer of the later scope...

  # setup the continuation for a tail call to a possibly new environment;
  # this ctor variable is used in the bottom leg of the `when` below...
  var ctor = e.newContinuation(e.identity, goto)

  if e.first.isNil or e.first.isEmpty:
    # we don't have a local continuation; just use the ctor we built
    result = e.maybeConvertToRoot(ctor)
  else:
    # when we can reuse the continuation, we'll merely set the fn pointer
    assert into.kind == nnkStmtList
    into.doc "re-use the local continuation by setting the fn"
    into.add newAssignment(newDotExpr(e.first, e.fn), goto)
    result = e.first

  # record the last-rendered scope for use in trace composition
  e.camefrom = newScope(result, goto, result)


proc rewriteReturn*(e: var Env; n: NimNode): NimNode =
  ## Rewrite a return statement to use our result field.
  if n.len != 1:
    result = n.errorAst "return len != 1"
  else:
    case n[0].kind
    of nnkAsgn:
      # okay, it's a return: result = ...
      result = newStmtList()
      # ignore the result symbol and create a new assignment
      result.add newAssignment(e.rs, n.last.last)
      # and just issue an empty `return`
      result.add nnkReturnStmt.newNimNode(n).add newEmptyNode()
    of nnkEmpty, nnkIdent:
      # this is `return` or `return continuation`, so that's fine...
      result = n
    else:
      result = n.errorAst "malformed return"

proc rewriteSymbolsIntoEnvDotField*(e: var Env; n: NimNode): NimNode =
  ## swap symbols for those in the continuation
  result = n
  let child = e.castToChild(e.first)
  for field, section in pairs(e):
    result = result.resym(section[0][0], newDotExpr(child, field))

proc prepProcBody*(e: var Env; n: NimNode): NimNode =
  result = rewriteSymbolsIntoEnvDotField(e, n)

proc defineProc*(e: var Env; p: NimNode) =
  ## add a proc definition, eg. as part of makeTail()
  assert not p.isNil
  assert p.kind == nnkProcDef
  e.store.add p

