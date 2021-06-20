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
  LocalCache = OrderedTable[Name, IdentDefVarLet]

  Env* = ref object
    id: Name                        # the identifier of our continuation type
    via: Name                       # the identifier of the type we inherit
    parent: Env                     # the parent environment (scope)
    locals: LocalCache              # locals and their typedefs|generics
    store: NimNode                  # where to put typedefs, a stmtlist
    when cpsReparent:
      seen: HashSet[string]         # count/measure idents/syms by string

    # special symbols for cps machinery
    c: Name                         # the sym we use for the continuation
    fn: Name                        # the sym we use for the goto target
    rs: IdentDefs                   # the identdefs for the result
    mom: Name                       # the sym we use for parent continuation

  CachePair* = tuple
    key: Name
    val: IdentDefVarLet

proc `$`(p: CachePair): string {.used.} =
  p.val.repr & ": " & p.key.repr

proc len*(e: Env): int = e.locals.len

proc isEmpty*(e: Env): bool = e.len == 0

proc inherits*(e: Env): Name =
  if e.parent.isNil:
    e.via
  else:
    e.parent.id

proc identity*(e: Env): Name = e.id

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

proc root*(e: Env): Name =
  var r = e
  while not r.parent.isNil:
    r = r.parent
  result = r.inherits

proc castToRoot(e: Env; n: NimNode): NimNode =
  # XXX: remove NimNode
  newTree(nnkCall, e.root.NimNode, n)

proc castToChild(e: Env; n: NimNode): NimNode =
  # XXX: remove NimNode
  newTree(nnkCall, e.identity.NimNode, n)

proc castToChild(e: Env; n: Name): NimNode =
  # XXX: remove NimNode
  newTree(nnkCall, e.identity.NimNode, n.NimNode)

proc maybeConvertToRoot*(e: Env; locals: NimNode): NimNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    e.castToRoot(locals)
  else:
    locals

proc set(e: var Env; key: Name; val: IdentDefVarLet): Env

proc init(e: var Env) =
  if e.fn.isNil:
    e.fn = asName"fn"
  if e.mom.isNil:
    e.mom = asName"mom"
  e.id = newTypeName("cps environment")
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
  # XXX: remove NimNode
  for name, section in e.locals.pairs:
    n.add:
      newIdentDefs(name, section.inferTypFromImpl, newEmptyNode()).NimNode

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  # XXX: remove NimNode
  var pragma = newEmptyNode()
  var record = nnkRecList.newNimNode(e.identity.NimNode)
  e.populateType record
  var parent = nnkOfInherit.newNimNode(e.root.NimNode).add e.inherits.NimNode
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
  # XXX: remove NimNode
  nnkTypeDef.newTree(e.identity.NimNode, newEmptyNode(), e.objectType)

proc first*(e: Env): Name = e.c

proc firstDef*(e: Env): IdentDefs =
  # XXX: remove NimNode
  newIdentDefs(e.first, e.via, newEmptyNode())

proc get*(e: Env): NimNode =
  ## retrieve a continuation's result value from the env
  # XXX: remove NimNode
  newDotExpr(e.castToChild(e.first), e.rs.name.NimNode)

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
                 locals: initOrderedTable[Name, IdentDefVarLet](),
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
        if e.via == e.parent.identity:
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

proc set(e: var Env; key: Name; val: IdentDefVarLet): Env =
  ## set [ident|sym] = let/var section
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
    field = asName(genField def.name.strVal)
    value = newVarLetIdentDef(kind, def.name, stripVar(def.typ), def.val)
    # we stripVar to ident: <no var> type = default
  e = e.set(field, value)
  result = (key: field, val: value)

proc newEnv*(c: Name; store: var NimNode; via: Name, rs: NimNode): Env =
  ## the initial version of the environment;
  ## `c` names the first parameter of continuations,
  ## `store` is where we add types and procedures,
  ## `via` is the type from which we inherit,
  ## `rs` is the return type (if not nnkEmpty) of the continuation.
  # XXX: remove the Name hack
  let via = if via.isNil: errorAst"need a type".Name else: via

  result = Env(c: c, store: store, via: via, id: via)
  result.rs = newIdentDefs("result", asNameAllowEmpty(rs))
  when cpsReparent:
    result.seen = initHashSet[string]()
  init result

proc newEnv*(store: var NimNode; via: Name, rs: NimNode): Env=
  newEnv(asName("continuation"), store, via, rs)

proc identity*(e: var Env): Name =
  # XXX: replace asserts and remove NimNode
  assert not e.id.isNil
  assert not e.id.NimNode.isEmpty
  result = e.id

proc initialization(e: Env; field: Name, section: IdentDefVarLet): NimNode =
  ## produce the `x = 34`
  # XXX: remove NimNode
  result = newStmtList()
  # let/var sections basically become env2323(cont).foo34 = "some default"
  if section.hasValue:
    # this is our continuation type, fully cast
    let child = e.castToChild(e.first)
    result.add newAssignment(newDotExpr(child, field.NimNode), section.val)

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
  ## compose an assignment during addition of identdefs to env. For the
  ## purposes of CPS, even though let and var sections contain identdefs this
  ## proc should never handle those directly, see overloads.
  let section = letOrVar(d)
  discard e.addIdentDef(section, d)
  when cpsDebug == "Env":
    echo $d.kind, "\t", repr(d)
  # don't attempt to redefine proc params!
  result = newStmtList()

proc addAssignment(e: var Env; section: IdentDefVarLet): NimNode =
  ## compose an assignment during addition of var|let identDefs to env
  let (field, value) = e.addIdentDef(section.kind, section.identdef())
  when cpsDebug == "Env":
    echo $kind, "\t", repr(d)
  result = e.initialization(field, value)

when false:
  proc getFieldViaLocal*(e: Env; n: NimNode): NimNode =
    ## get a field from the env using a local symbol as input
    block found:
      for field, sym in e.locals.pairs:
        if sym.name == n:
          result = field
          break found
      result = n.errorAst:
        "unable to find field for symbol " & n.repr

proc localSection*(e: var Env; n: VarLet, into: NimNode = nil) =
  ## consume a var|let section and yield name, node pairs
  ## representing assignments to local scope
  # XXX: remove NimNode
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
      tups.add newDotExpr(child, field.NimNode)
    maybeAdd newAssignment(tups, defs.val)
  else:
    # an iterator handles `var a, b, c = 3` appropriately
    maybeAdd e.addAssignment(n.asVarLetIdentDef())

proc localSection*(e: var Env; n: IdentDefs; into: NimNode = nil) =
  ## consume nnkIdentDefs and populate `into` with assignments, even if `into`
  ## is nil, the `n` will be cached locally
  let assignment = e.addAssignment(n)
  if not into.isNil:
    into.add assignment

proc localSection*(e: var Env; n: RoutineParams; into: NimNode = nil) {.borrow.}
  ## consume proc definition params and yield name, node pairs representing
  ## assignments to local scope.

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
    if sym.isSymbol:
      result = result.resym(sym.NimNode, newDotExpr(child, field))
    else:
      {.warning: "pending https://github.com/nim-lang/Nim/issues/17851".}
  # make a special rewrite pass to replace the result symbols
  result = e.rewriteResult result

proc createContinuation*(e: Env; name: Name; goto: NimNode): NimNode =
  ## allocate a continuation as `name` and maybe aim it at the leg `goto`
  # XXX: remove NimNode
  proc resultdot(n: NimNode): NimNode =
    newDotExpr(e.castToChild(name), n)
  result = newStmtList:
    newAssignment name.NimNode:
      Alloc.hook(e.inherits.NimNode, e.identity.NimNode)
  for field, section in e.pairs:
    # omit special fields in the env that we use for holding
    # custom functions, results, exceptions, and parent respectively
    if field notin [e.fn, e.rs.name, e.mom]:
      # the name from identdefs is not gensym'd (usually!)
      result.add:
        newAssignment(resultdot field.NimNode, section.name.NimNode)
  if not goto.isNil:
    result.add:
      newAssignment(resultdot e.fn.NimNode, goto)

proc genException*(e: var Env): NimNode =
  ## generates a new symbol of type ref Exception, then put it in the env.
  ##
  ## returns the access to the exception symbol from the env.
  let ex = asName(genField("ex"))
  e = e.set ex:
    newIdentDefLet(ex, newRefType(bindName("Exception")), newNilLit())
  result = newDotExpr(e.castToChild(e.first), ex.NimNode)

proc createResult*(env: Env, exported = false): ProcDef =
  ## define a procedure for retrieving the result of a continuation
  ##
  ## `exported` determines whether this procedure will be exported
  let
    field =
      if env.rs.hasType:
        env.get                  # the return value is env.result
      else:
        nnkDiscardStmt.newTree:
          newEmptyNode()         # the return value is void

  # compose the (exported?) symbol
  var name = nnkAccQuoted.newTree ident"()"
  if exported:
    name = postfix(name, "*")

  result = ProcDef:
    genAst(name, field, c = env.first.NimNode, cont = env.identity.NimNode,
           tipe = env.rs.typ, dismissed=Dismissed, finished=Finished,
           running=Running):
      {.push experimental: "callOperator".}
      proc name(c: cont): tipe {.used.} =
        case c.state
        of dismissed:
          raise Defect.newException:
            "dismissed continuations have no result"
        of finished:
          field
        of running:
          `()`(trampoline c)
      {.pop.}

proc createWhelp*(env: Env; n: ProcDef, goto: NimNode): ProcDef =
  ## the whelp needs to create a continuation
  # XXX: remove NimNode
  result = clone(n, newStmtList())
  result.addPragma ident"used"  # avoid gratuitous warnings
  result.returnParam = env.identity
  result.name = nskProc.genSym"whelp"
  result.introduce {Alloc, Boot}

  # create the continuation as the result and point it at the proc
  result.body.add:
    env.createContinuation(asName"result", desym goto)

  # hook the bootstrap
  result.body.add:
    newAssignment ident"result":
      Boot.hook ident"result"

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs.name)

proc createBootstrap*(env: Env; n: ProcDef, goto: NimNode): ProcDef =
  ## the bootstrap needs to create a continuation and trampoline it
  # XXX: remove NimNode
  result = clone(n, newStmtList())
  result.addPragma "used"  # avoid gratuitous warnings
  result.introduce {Alloc, Boot}

  let c = newVarName("C")
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
    newAssignment c.NimNode:
      Head.hook:
        Boot.hook c.NimNode

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs.name)

  # now the trampoline
  let tramp = bindSym"trampoline"
  result.body.add:
    newAssignment(c.NimNode, newCall(tramp, c.NimNode))

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
      result.insert(1, env.first.NimNode)
  result = filter(n, voodoo)
