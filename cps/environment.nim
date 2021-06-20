##[

The Env(ironment) tracks continuation types and the variables of which
they are comprised.

]##

import std/[sets, sequtils, hashes, tables, macros, algorithm, genasts]
import cps/[spec, hooks, help, rewrites, normalizedast]

#{.experimental: "strictNotNil".}

const
  cpsReparent = false

type
  # the idents|symbols and the typedefs they refer to in order of discovery
  LocalCache = OrderedTable[NimNode, IdentDefVarLet]

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
    mom: NimNode                    # the sym we use for parent continuation

  CachePair* = tuple
    key: NimNode
    val: IdentDefVarLet

proc `$`(p: CachePair): string {.used.} =
  p[0].repr & ": " & p[1].repr

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

proc set(e: var Env; key: NimNode; val: IdentDefVarLet): Env

proc init(e: var Env) =
  if e.fn.isNil:
    e.fn = ident"fn"
  if e.mom.isNil:
    e.mom = ident"mom" # FIXME: use a getter/setter?
  e.id = genSym(nskType, "env")
  if e.rs.hasType:
    e = e.set(e.rs.name, newIdentDefVar(e.rs))

proc allPairs(e: Env): seq[CachePair] =
  if not e.isNil:
    result = toSeq e.locals.pairs
    # most-recently-defined comes first
    reverse result
    # add any inherited types from the parent
    result.add allPairs(e.parent)

iterator pairs(e: Env): CachePair =
  var seen = initHashSet[string]()
  for pair in e.allPairs:
    # make sure we're actually measuring gensyms for collision
    let name = ident(repr(pair.val.name))
      ## create an identifier from an typesection/identDef as cached;
      ## this is a copy and it is repr'd to ensure gensym compat...
    if not seen.containsOrIncl name.strval:
      yield pair

proc populateType(e: Env; n: var NimNode) =
  ## add fields in the env into a record
  for name, section in e.locals.pairs:
    n.add:
      newIdentDefs(name, section.inferTypFromImpl, newEmptyNode())

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

proc rewriteResult(e: Env; n: NimNode): NimNode =
  ## replaces result symbols with the env's result; this should be
  ## safe to run on sem'd ast (for obvious reasons)
  proc rewriter(n: NimNode): NimNode =
    ## Rewrite any result symbols to use the result field from the Env.
    case n.kind
    of nnkSym:
      if n.symKind == nskResult:
        result = e.get
    else: discard
  result = filter(n, rewriter)

proc newEnv*(parent: Env; copy = off): Env =
  ## this is called as part of the recursion in the front-end,
  ## or on-demand in the back-end (with copy = on)
  if copy:
    result = Env(store: parent.store,
                 via: parent.identity,
                 locals: initOrderedTable[NimNode, IdentDefVarLet](),
                 c: parent.c,
                 rs: parent.rs,
                 fn: parent.fn,
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

proc set(e: var Env; key: NimNode; val: IdentDefVarLet): Env =
  ## set [ident|sym] = let/var section
  assert key.kind in {nnkSym, nnkIdent}
  if key in e.locals:
    result = storeType e
  else:
    result = e
  result.locals[key] = val
  when cpsReparent:
    result.seen.incl key.strVal

proc addIdentDef(e: var Env; kind: NimNodeKind; def: IdentDefs): CachePair =
  ## add an IdentDef from a Var|Let Section to the env
  template stripVar(n: NimNode): NimNode =
    ## pull the type out of a VarTy
    if n.kind == nnkVarTy: n[0] else: n

  let
    field = genField def.name.strVal
    value = newVarLetIdentDef( kind, def.name, stripVar(def.typ), def.val)
    # we stripVar to ident: <no var> type = default
  e = e.set(field, value)
  result = (key: field, val: value)

proc newEnv*(c: NimNode; store: var NimNode; via, rs: NimNode): Env=
  ## the initial version of the environment;
  ## `c` names the first parameter of continuations,
  ## `store` is where we add types and procedures,
  ## `via` is the type from which we inherit,
  ## `rs` is the return type (if not nnkEmpty) of the continuation.
  let via = if via.isNil: errorAst"need a type" else: via
  let rs = if rs.isNil: newEmptyNode() else: rs
  let c = if c.isNil or c.isEmpty: ident"continuation" else: c

  result = Env(c: c, store: store, via: via, id: via)
  result.rs = newIdentDefs("result", rs)
  when cpsReparent:
    result.seen = initHashSet[string]()
  init result

proc identity*(e: var Env): NimNode =
  assert not e.id.isNil
  assert not e.id.isEmpty
  result = e.id

proc initialization(e: Env; field: NimNode, section: IdentDefVarLet): NimNode =
  ## produce the `x = 34`
  result = newStmtList()
  # let/var sections basically become env2323(cont).foo34 = "some default"
  if section.hasValue:
    # this is our continuation type, fully cast
    let child = e.castToChild(e.first)
    result.add newAssignment(newDotExpr(child, field), section.val)

proc letOrVar(n: IdentDefs): NimNodeKind =
  ## choose between let or var for proc parameters
  case n.typ.kind:
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection
  else:
    result = nnkLetSection

proc addAssignment(e: var Env; d: IdentDefs): NimNode =
  ## compose an assignment during addition of identDefs to env for proc params
  let section = letOrVar(d)
  discard e.addIdentDef(section, d)
  when cpsDebug == "Env":
    echo $d.kind, "\t", repr(d)
  # don't attempt to redefine proc params!
  result = newStmtList()

proc addAssignment(e: var Env; section: IdentDefVarLet): NimNode =
  ## compose an assignment during addition of identDefs to env
  let (field, value) = e.addIdentDef(section.kind, section.identdef())
  when cpsDebug == "Env":
    echo $kind, "\t", repr(d)
  result = e.initialization(field, value)

when false:
  proc getFieldViaLocal(e: Env; n: NimNode): NimNode =
    ## get a field from the env using a local symbol as input
    for field, defs in e.allPairs:
      if defs[0] == n:
        result = field
        break
    if result.isNil:
      result = n.errorAst "unable to find field for symbol " & n.repr

proc localSection*(e: var Env; n: VarLet, into: NimNode = nil) =
  ## consume a var|let section and yield name, node pairs
  ## representing assignments to local scope
  template maybeAdd(x) =
    if not into.isNil:
      into.add x

  if n.isTuple:
    # deconstruct the RHS types into multiple assignments
    # let (a, b, c) = foo() -> (env.a, env.b, env.c) = foo()
    let
      defs = n.asVarLetTuple()
      child = e.castToChild(e.first)
      rhs = defs.typ
      tups = nnkTupleConstr.newTree
    for index, name in defs.indexNamePairs:
      let entry = newIdentDefs(name, rhs[index], newEmptyNode())
      # we need to insert the variable and then write a new
      # accessor that plucks the field from the env
      let (field, _) = e.addIdentDef(n.kind, expectIdentDefs(entry))
      tups.add newDotExpr(child, field)
    maybeAdd newAssignment(tups, defs.val)
  else:
    # an iterator handles `var a, b, c = 3` appropriately
    maybeAdd e.addAssignment(n.asVarLetIdentDef())

proc localSection*(e: var Env; n: NimNode; into: NimNode = nil) =
  ## consume ident defs and yield name, node pairs representing assignments to
  ## local scope.
  case n.kind
  of nnkVarSection, nnkLetSection:
    # XXX: this branch goes away once we type procParams, as that's the only
    #      other use for this proc based on the call sites.
    error "this is a deprecated path and should not be triggered"
  of nnkIdentDefs:
    let assignment = e.addAssignment(expectIdentDefs(n))
    if not into.isNil:
      into.add assignment
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
      # and add the termination annotation
      result.add newCpsTerminate()
    of nnkEmpty:
      # this is an empty return
      result = newCpsTerminate()
    else:
      # okay, it's a return of some rando expr
      result = newStmtList()
      # ignore the result symbol and create a new assignment
      result.add newAssignment(e.get, n.last)
      # and add the termination annotation
      result.add newCpsTerminate()

proc rewriteSymbolsIntoEnvDotField*(e: var Env; n: NimNode): NimNode =
  ## swap symbols for those in the continuation
  result = n
  let child = e.castToChild e.first
  for field, section in e.pairs:
    # we don't resym identifiers, which we must have created;
    # these are a side-effect of an open nim bug
    let sym = section.name
    if sym.kind == nnkSym:
      result = result.resym(sym, newDotExpr(child, field))
    else:
      {.warning: "pending https://github.com/nim-lang/Nim/issues/17851".}
  # make a special rewrite pass to replace the result symbols
  result = e.rewriteResult result

proc createContinuation*(e: Env; name: NimNode; goto: NimNode): NimNode =
  ## allocate a continuation as `name` and maybe aim it at the leg `goto`
  proc resultdot(n: NimNode): NimNode =
    newDotExpr(e.castToChild(name), n)
  result = newStmtList:
    newAssignment name:
      Alloc.hook(e.inherits, e.identity)
  for field, section in e.pairs:
    # omit special fields in the env that we use for holding
    # custom functions, results, exceptions, and parent respectively
    if field notin [e.fn, e.rs.name, e.mom]:
      # the name from identdefs is not gensym'd (usually!)
      result.add:
        newAssignment(resultdot field, section.name)
  if not goto.isNil:
    result.add:
      newAssignment(resultdot e.fn, goto)

proc genException*(e: var Env): NimNode =
  ## generates a new symbol of type ref Exception, then put it in the env.
  ##
  ## returns the access to the exception symbol from the env.
  let ex = genField("ex")
  e = e.set ex:
    # XXX: Should be IdentDefLet but saem haven't wrote it yet
    newIdentDefVar(ex, nnkRefTy.newTree(bindSym"Exception"), newNilLit())
  result = newDotExpr(e.castToChild(e.first), ex)

proc createResult*(env: Env): ProcDef =
  ## define a procedure for retrieving the result of a continuation
  let field =
    if env.rs.typ.isEmpty:
      newEmptyNode()           # the return value is Empty
    else:
      env.get                  # the return value is env.result

  ProcDef:
    genAst(field, c = env.first, cont = env.identity, tipe = env.rs.typ):
      proc `...`(c: cont): tipe =
        case c.state
        of Dismissed:
          raise Defect.newException:
            "dismissed continuations have no result"
        of Finished:
          field
        of Running:
          var c = trampoline c
          ... c

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
      Head.hook:
        Boot.hook c

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs[0])

  # now the trampoline
  let tramp = bindSym"trampoline"
  result.body.add:
    newAssignment(c, newCall(tramp, c))

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
