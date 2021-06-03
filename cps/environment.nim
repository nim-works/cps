##[

The Env(ironment) tracks continuation types and the variables of which
they are comprised.

]##

import std/[sets, sequtils, hashes, tables, macros, algorithm]
import cps/[spec, hooks, help, rewrites, normalizedast]

#{.experimental: "strictNotNil".}

const
  cpsReparent = false

type
  # the idents|symbols and the typedefs they refer to in order of discovery
  LocalCache = OrderedTable[NimNode, NimNode]

  Env* = ref object
    id: NimNode                     # the identifier of our continuation type
    via: NimNode                    # the identifier of the type we inherit
    parent: Env                     # the parent environment (scope)
    locals: LocalCache              # locals and their typedefs|generics
    store: NimNode                  # where to put typedefs, a stmtlist
    when cpsReparent:
      seen: HashSet[string]           # count/measure idents/syms by string

    # special symbols for cps machinery
    c: NimNode                      # the sym we use for the continuation
    fn: NimNode                     # the sym we use for the goto target
    rs: IdentDefs                   # the identdefs for the result
    ex: NimNode                     # the sym we use for current exception
    mom: NimNode                    # the sym we use for parent continuation

proc len*(e: Env): int = e.locals.len

proc isEmpty*(e: Env): bool = e.len == 0

proc inherits*(e: Env): NimNode =
  if e.parent.isNil:
    e.via
  else:
    e.parent.id

proc identity*(e: Env): NimNode = e.id

func isWritten(e: Env): bool =
  ## why say in three lines what you can say in six?
  block found:
    for section in e.store.items:
      if section.kind == nnkTypeSection:
        for def in section.items:
          result = repr(def[0]) == repr(e.identity)
          if result:
            break found

func isDirty*(e: Env): bool =
  ## the type hasn't been written since an add occurred
  (not e.isWritten) or (not e.parent.isNil and e.parent.isDirty)

proc root*(e: Env): NimNode =
  var r = e
  while not r.parent.isNil:
    r = r.parent
  result = r.inherits

proc castToRoot(e: Env; n: NimNode): NimNode =
  newTree(nnkCall, e.root, n)

proc castToChild(e: Env; n: NimNode): NimNode =
  newTree(nnkCall, e.identity, n)

proc maybeConvertToRoot*(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    e.castToRoot(locals)
  else:
    locals

proc set(e: var Env; key: NimNode; val: NimNode): Env
proc set(e: var Env; key: NimNode; val: VarSection): Env
# XXX: listen `key` param, you're going to get typed one of these days, mark my
#      words. Your `NimNode` days are numbered, now wtf do we call you?

proc init(e: var Env) =
  if e.fn.isNil:
    e.fn = ident"fn"
  if e.mom.isNil:
    e.mom = ident"mom" # FIXME: use a getter/setter?
  e.id = genSym(nskType, "env")
  if e.rs.hasType:
    e = e.set(e.rs.name, newVarSection e.rs)

proc definedName(n: NimNode): NimNode =
  ## create an identifier from an typesection/identDef as cached;
  ## this is a copy and it is repr'd to ensure gensym compat...
  assert n.kind in {nnkVarSection, nnkLetSection}, "use this on env[key]"
  result = ident(repr(n[0][0]))

proc allPairs(e: Env): seq[Pair] =
  if not e.isNil:
    result = toSeq e.locals.pairs
    # most-recently-defined comes first
    reverse result
    # add any inherited types from the parent
    result.add allPairs(e.parent)

iterator pairs(e: Env): Pair =
  var seen = initHashSet[string]()
  for pair in e.allPairs:
    # make sure we're actually measuring gensyms for collision
    if not seen.containsOrIncl definedName(pair.val).strVal:
      yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in e.locals.pairs:
    for defs in section.items:
      # we need either an initialization value or a type field
      if defs[1].isEmpty and defs[2].isEmpty:
        if name != e.rs.name:
          error "local " & repr(name) & " lacks type/initialization"
      else:
        n.add:
          if defs[1].isEmpty:         # get the type of the assignment
            newIdentDefs(name, getTypeImpl(defs.last), newEmptyNode())
          else:                       # name is an ident or symbol
            newIdentDefs(name, defs[1], newEmptyNode())

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  var pragma = newEmptyNode()
  var record = nnkRecList.newNimNode(e.identity)
  e.populateType record
  var parent = nnkOfInherit.newNimNode(e.root).add e.inherits
  result = nnkRefTy.newTree:
    nnkObjectTy.newTree(pragma, parent, record)

when cpsReparent:
  proc contains(e: Env; key: NimNode): bool {.deprecated: "unused".} =
    ## you're giving us a symbol|ident and we're telling you if we have it
    ## recorded with that name.
    assert not key.isNil
    assert key.kind in {nnkSym, nnkIdent}
    result = key.strVal in e.seen

  proc `==`(a, b: Env): bool {.deprecated, used.} = a.seen == b.seen
  proc `<`(a, b: Env): bool = a.seen < b.seen
  proc `*`(a, b: Env): HashSet[string] {.deprecated, used.} = a.seen * b.seen

  proc reparent(e: Env; p: Env) =
    ## set all nodes to have a parent at least as large as p
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

proc makeType*(e: Env): NimNode =
  ## turn an env into a named object typedef `foo = object ...`
  nnkTypeDef.newTree(e.id, newEmptyNode(), e.objectType)

proc first*(e: Env): NimNode = e.c

proc firstDef*(e: Env): NimNode =
  newIdentDefs(e.first, e.via, newEmptyNode())

proc get*(e: Env): NimNode =
  ## retrieve a continuation's result value from the env
  newDotExpr(e.castToChild(e.first), e.rs.name)

proc newEnv*(parent: Env; copy = off): Env =
  ## this is called as part of the recursion in the front-end,
  ## or on-demand in the back-end (with copy = on)
  if copy:
    result = Env(store: parent.store,
                 via: parent.identity,
                 locals: initOrderedTable[NimNode, NimNode](),
                 c: parent.c,
                 rs: parent.rs,
                 fn: parent.fn,
                 ex: parent.ex,
                 parent: parent)
    when cpsReparent:
      result.seen = parent.seen
    init result
  else:
    result = parent

proc storeType*(e: Env; force = off): Env =
  ## turn an env into a complete typedef in a type section
  if e.isDirty:
    when cpsReparent:
      if force:
        reparent(e, e)
    if not e.parent.isNil:
      if not e.parent.isDirty:
        # must store the parent for inheritance ordering reasons;
        # also, we don't want it to be changed under our feet.
        if e.via == e.parent.id:
          e.parent = storeType e.parent
    e.store.add:
      nnkTypeSection.newTree e.makeType
    when cpsDebug == "Env":
      echo "storing type ", repr(e.identity)
    # clearly, if we ever write again, we want it to be a new type
    result = newEnv(e, copy = on)
    when cpsDebug == "Env":
      echo "next type ", repr(result.identity)
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
    result = storeType e
  else:
    result = e
  result.locals[key] = val
  when cpsReparent:
    result.seen.incl key.strVal

proc set(e: var Env; key: NimNode; val: VarSection): Env =
  # XXX: not doing an explicit conversion will recursively call this proc over
  #      and over again and break everything. :(
  set(e, key, cVarSectionToNimNode(val))

iterator addIdentDef(e: var Env; kind: NimNodeKind; def: IdentDefs): Pair =
  ## add an IdentDef from a Var|Let Section to the env
  template stripVar(n: NimNode): NimNode =
    ## pull the type out of a VarTy
    if n.kind == nnkVarTy: n[0] else: n

  let
    field = genField def.name.strVal
    value = newTree(kind,     # ident: <no var> type = default
                    newIdentDefs(def.name, stripVar(def.typ), def.val))
  e = e.set(field, value)
  yield (key: field, val: value)

proc newEnv*(c: NimNode; store: var NimNode; via, rs: NimNode): Env=
  ## the initial version of the environment;
  ## `c` names the first parameter of continuations,
  ## `store` is where we add types and procedures,
  ## `via` is the type from which we inherit,
  ## `rs` is the return type (if not nnkEmpty) of the continuation.
  let via = if via.isNil: errorAst"need a type" else: via
  let rs = if rs.isNil: newEmptyNode() else: rs
  let c = if c.isNil or c.isEmpty: ident"continuation" else: c

  # add a check to make sure the supplied type will work for descendants
  let check = nnkWhenStmt.newNimNode via
  check.add:
    nnkElifBranch.newTree:
      [ infix(via, "isnot", bindSym"Continuation"),
        errorAst repr(via) & " does not match the Continuation concept" ]
  store.add check

  result = Env(c: c, store: store, via: via, id: via)
  result.rs = newIdentDefs("result", rs)
  when cpsReparent:
    result.seen = initHashSet[string]()
  init result

proc identity*(e: var Env): NimNode =
  assert not e.id.isNil
  assert not e.id.isEmpty
  result = e.id

proc initialization(e: Env; kind: NimNodeKind;
                    field: NimNode; value: NimNode): NimNode =
  ## produce the `x = 34` appropriate given the field and identDefs
  doAssert kind in {nnkVarSection, nnkLetSection, nnkIdentDefs}

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

proc letOrVar(n: IdentDefs): NimNodeKind =
  ## choose between let or var for proc parameters
  case n.typ.kind:
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection
  else:
    result = nnkLetSection

iterator addAssignment(e: var Env; kind: NimNodeKind; d: IdentDefs): NimNode =
  ## compose an assignment during addition of identDefs to env
  ## XXX: `kind` is being used to differentiate between IdentDef in a Var|Let
  ##       Section vs a parameter, used for assignment. Create a new type
  let section =
    if kind in {nnkVarSection, nnkLetSection}:
      kind
    else:
      letOrVar(d)
  for field, value in e.addIdentDef(section, d):
    #let name = definedName(value)
    when cpsDebug == "Env":
      echo $kind, "\t", repr(d)
    yield e.initialization(kind, field, value)

when false:
  proc getFieldViaLocal(e: Env; n: NimNode): NimNode =
    ## get a field from the env using a local symbol as input
    for field, defs in e.allPairs:
      if defs[0] == n:
        result = field
        break
    if result.isNil:
      result = n.errorAst "unable to find field for symbol " & n.repr

  proc findJustOneAssignmentName*(e: Env; n: NimNode): NimNode
    {.deprecated: "not used yet".} =
    case n.kind
    of nnkVarSection, nnkLetSection:
      if n.len != 1:
        n.errorAst "only one assignment per section is supported"
      else:
        e.findJustOneAssignmentName n[0]
    of nnkIdentDefs:
      if n.len != 3:
        n.errorAst "only one identifier per assignment is supported"
      elif n[0].kind notin {nnkIdent, nnkSym}:
        n.errorAst "bad rewrite presented bogus input"
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
            for field, value in e.addIdentDef(n.kind, expectIdentDefs(entry)):
              tups.add newDotExpr(child, field)
          maybeAdd newAssignment(tups, defs.last)
        else:
          # an iterator handles `var a, b, c = 3` appropriately
          for assignment in e.addAssignment(n.kind, expectIdentDefs(defs)):
            maybeAdd assignment
  of nnkIdentDefs:
    for assignment in e.addAssignment(n.kind, expectIdentDefs(n)):
      maybeAdd assignment
  else:
    e.store.add:
      n.errorAst "localSection input"

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
      result.add newAssignment(e.get, n.last.last)
      # and just issue an empty `return`
      result.add nnkReturnStmt.newNimNode(n).add newEmptyNode()
    of nnkEmpty, nnkIdent:
      # this is `return` or `return continuation`, so that's fine...
      result = n
    else:
      # okay, it's a return of some rando expr
      result = newStmtList()
      # ignore the result symbol and create a new assignment
      result.add newAssignment(e.get, n.last)
      # signify the end of the continuation
      result.add newAssignment(newDotExpr(e.first, e.fn), newNilLit())
      # and return the continuation
      result.add nnkReturnStmt.newNimNode(n).add(e.first)

proc rewriteSymbolsIntoEnvDotField*(e: var Env; n: NimNode): NimNode =
  ## swap symbols for those in the continuation
  result = n
  let child = e.castToChild e.first
  for field, section in e.pairs:
    # we don't resym identifiers, which we must have created;
    # these are a side-effect of an open nim bug
    let sym = section.last[0]
    if sym.kind == nnkSym:
      result = result.resym(sym, newDotExpr(child, field))
    else:
      {.warning: "pending https://github.com/nim-lang/Nim/issues/17851".}

proc createContinuation*(e: Env; name: NimNode; goto: NimNode): NimNode =
  ## allocate a continuation as `name` and maybe aim it at the leg `goto`
  proc resultdot(n: NimNode): NimNode =
    newDotExpr(e.castToChild(name), n)
  result = newStmtList:
    newAssignment name:
      hook Alloc: e.identity
  for field, section in e.pairs:
    # omit special fields in the env that we use for holding
    # custom functions, results, exceptions, and parent respectively
    if field notin [e.fn, e.rs.name, e.mom]:
      # the name from identdefs is not gensym'd (usually!)
      result.add:
        newAssignment(resultdot field, section.last[0])
  if not goto.isNil:
    result.add:
      newAssignment(resultdot e.fn, goto)

proc getException*(e: var Env): NimNode =
  ## get the current exception from the env, instantiating it if necessary
  if e.ex.isNil:
    e.ex = genField"ex"
    e = e.set e.ex:
      newVarSection(e.ex, nnkRefTy.newTree(bindSym"Exception"), newNilLit())
  result = newDotExpr(e.castToChild(e.first), e.ex)

proc createWhelp*(env: Env; n: ProcDef, goto: NimNode): ProcDef =
  ## the whelp needs to create a continuation
  result = clone(n, newStmtList())
  result.addPragma ident"used"  # avoid gratuitous warnings
  result.returnParam = env.root
  result.name = nskProc.genSym"whelp"
  result.introduce {Alloc, Boot}

  # create the continuation as the result and point it at the proc
  result.body.add:
    env.createContinuation(ident"result", desym goto)

  # hook the bootstrap
  result.body.add:
    newAssignment ident"result":
      Boot.hook ident"result"

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs[0])

proc createBootstrap*(env: Env; n: ProcDef, goto: NimNode): ProcDef =
  ## the bootstrap needs to create a continuation and trampoline it
  result = clone(n, newStmtList())
  result.addPragma ident"used"  # avoid gratuitous warnings
  result.introduce {Alloc, Boot}

  let c = nskVar.genSym"c"
  result.body.add:
    # declare `var c: Cont`
    # XXX: conversion must be forced otherwise we end up with an ambiguous call
    #      between the add a single NimNode and add many NimNode (varargs).
    cVarSectionToNimNode(newVarSection(c, env.root))

  # create the continuation using the new variable and point it at the proc
  result.body.add:
    env.createContinuation(c, desym goto)

  # hook the bootstrap
  result.body.add:
    newAssignment c:
      Boot.hook c

  # now the trampoline
  result.body.add:
    nnkWhileStmt.newTree: [
      newCall(ident"running", c),  # XXX: bindSym?  bleh.
      newAssignment(c, newDotExpr(c, env.fn).newCall(c))
    ]

  # do an easy static check, and then
  if env.rs.typ != result.returnParam:
    result.body.add:
      result.errorAst:
        "environment return-type doesn't match bootstrap return-type"
  # if the bootstrap has a return type,
  elif env.rs.hasType:
    result.body.add:
      # then at runtime, issue an if statement to
      nnkIfExpr.newTree:
        nnkElifExpr.newTree [
          # check if the continuation is not nil, and if so, to
          newCall(bindSym"not", newDotExpr(c, ident"dismissed")),
          # assign the result from the continuation's result field
          newAssignment(ident"result",
            newDotExpr(env.castToChild(c), env.rs.name))
        ]

proc rewriteVoodoo*(env: Env; n: NimNode): NimNode =
  ## Rewrite non-yielding cpsCall calls by inserting the continuation as
  ## the first argument
  proc voodoo(n: NimNode): NimNode =
    if n.isVoodooCall:
      result = n.copyNimTree
      result[0] = desym result[0]
      result.insert(1, env.first)
  result = filter(n, voodoo)
