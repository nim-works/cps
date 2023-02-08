##[

The Env(ironment) tracks continuation types and the variables of which
they are comprised.

]##

import std/macros except newStmtList, newTree
import std/[sets, sequtils, hashes, tables, algorithm, genasts]
import cps/[spec, hooks, help, rewrites, normalizedast]

#{.experimental: "strictNotNil".}

const
  cpsReparent = false

type
  # the idents|symbols and the typedefs they refer to in order of discovery
  LocalCache = OrderedTable[Name, VarLetIdentDef]

  Env* = ref object
    id: Name                        # the identifier of our continuation type
    via: Name                       # the identifier of the type we inherit
    parent: Env                     # the parent environment (scope)
    locals: LocalCache              # locals and their typedefs|generics
    store: NimNode                  # where to put typedefs, a stmtlist
    procedure: string               # name of procedure we are transforming
    when cpsReparent:
      seen: HashSet[string]         # count/measure idents/syms by string

    # special symbols for cps machinery
    c: Name                         # the sym we use for the continuation
    fn: Name                        # the sym we use for the goto target
    rs: IdentDef                    # the identdefs for the result
    mom: Name                       # the sym we use for parent continuation

  CachePair* = tuple
    key: Name
    val: VarLetIdentDef

proc procedure*(e: Env): string =
  ## the name of the procedure undergoing transformation to cps
  e.procedure & "()"

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

proc castToRoot(e: Env; n: NormNode): Call =
  newCall(e.root, n)

proc castToChild(e: Env; n: Name): Call =
  newCall(e.identity, n)

proc maybeConvertToRoot*(e: Env; locals: NormNode): NormNode =
  ## add an Obj(foo: bar).Other conversion if necessary
  if not eqIdent(locals[0], e.root):
    # converters can't figure this out automatically, manually casting
    NormNode e.castToRoot(locals)
  else:
    locals

proc set(e: var Env; key: Name; val: VarLetIdentDef): Env

proc init(e: var Env) =
  if e.fn.isNil:
    e.fn = "fn".asName(info = e.store.NormNode)
  if e.mom.isNil:
    e.mom = "mom".asName(info = e.store.NormNode)
  e.id = genTypeName(procedure(e), "env", info = e.via)
  if e.rs.hasType:
    e = e.set(e.rs.name, newVarIdentDef(e.rs))

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
      newIdentDef(name, section.inferTypFromImpl, newEmptyNode()).NimNode

proc objectType(e: Env): NimNode =
  ## turn an env into an object type
  # XXX: remove NimNode
  var pragma = newEmptyNode()
  var record = nnkRecList.newNimNode(e.identity.NimNode)
  e.populateType record
  var parent = nnkOfInherit.newNimNode(e.root.NimNode).add e.inherits
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
  nnkTypeDef.newTree(e.identity, newEmptyNode(), e.objectType)

proc first*(e: Env): Name = e.c

proc firstDef*(e: Env): IdentDef =
  # https://github.com/nim-lang/Nim/issues/18365
  newIdentDef(e.first, sinkAnnotated asTypeExpr bindName"Continuation", newEmptyNode())

proc getResult*(e: Env): NormNode =
  ## retrieve a continuation's result value from the env
  newDotExpr(e.castToChild(e.first), e.rs.name)

proc rewriteResult*(e: Env; n: NormNode): NormNode =
  ## replaces result symbols with the env's result; this should be
  ## safe to run on sem'd ast (for obvious reasons)
  proc rewriter(n: NormNode): NormNode =
    ## Rewrite any result symbols to use the result field from the Env.
    case n.kind
    of nnkSym:
      if n.symKind == nskResult:
        result = e.getResult
    of nnkProcDef, nnkLambda:
      result = n  # don't rewrite `result` in nested procedures
    else: discard
  result = filter(n, rewriter)

proc newEnv*(parent: Env; copy = off): Env =
  ## this is called as part of the recursion in the front-end,
  ## or on-demand in the back-end (with copy = on)
  if copy:
    result = Env(store: parent.store,
                 procedure: parent.procedure,
                 via: parent.identity,
                 locals: initOrderedTable[Name, VarLetIdentDef](),
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

proc set(e: var Env; key: Name; val: VarLetIdentDef): Env =
  ## set [ident|sym] = let/var section
  if key in e.locals:
    result = storeType e
  else:
    result = e
  result.locals[key] = val
  when cpsReparent:
    result.seen.incl key.strVal

proc addIdentDef(e: var Env; kind: NimNodeKind; def: IdentDef): CachePair =
  ## add an IdentDef from a Var|Let Section to the env
  template stripVar(n: TypeExpr): TypeExpr =
    ## pull the type out of a VarTy
    if n.kind == nnkVarTy: n[0] else: n

  let
    field = genField def.name.strVal
    value = newVarLetIdentDef(kind, def.name, stripVar(def.typ), def.val)
    # we stripVar to ident: <no var> type = default
  e = e.set(field, value)
  result = (key: field, val: value)

proc newEnv*(c: Name; store: var NormNode; via: Name; rs: NormNode;
             procedure: string): Env =
  ## the initial version of the environment;
  ## `c` names the first parameter of continuations,
  ## `store` is where we add types and procedures,
  ## `via` is the type from which we inherit,
  ## `rs` is the return type (if not nnkEmpty) of the continuation.
  let via = if via.isNil: errorAst"need a type".Name else: via

  result = Env(c: c, store: store, via: via, id: via, procedure: procedure)
  result.rs = newIdentDef("result", asTypeExprAllowEmpty(rs))
  when cpsReparent:
    result.seen = initHashSet[string]()
  init result

proc newEnv*(store: var NormNode; via: Name; rs: NormNode;
             procedure: string): Env =
  newEnv("continuation".asName(info = store), store, via, rs, procedure)

proc identity*(e: var Env): Name =
  ## identifier of our continuation type
  result = e.id

proc initialization(e: Env; field: Name, section: VarLetIdentDef): NimNode =
  ## produce the `x = 34`
  result = newStmtList()
  # let/var sections basically become env2323(cont).foo34 = "some default"
  if section.hasValue:
    # this is our continuation type, fully cast
    let child = e.castToChild(e.first)
    result.add newAssignment(newDotExpr(child, field), section.val)

proc letOrVar(n: IdentDef): NimNodeKind =
  ## choose between let or var for proc parameters
  case n.typ.kind:
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection
  else:
    result = nnkLetSection

proc addAssignment(e: var Env; d: IdentDef): NimNode =
  ## compose an assignment during addition of identdefs to env. For the
  ## purposes of CPS, even though let and var sections contain identdefs this
  ## proc should never handle those directly, see overloads.
  let section = letOrVar(d)
  when cpsDebug == "Env":
    echo $section.kind, "\t", repr(section)
  discard e.addIdentDef(section, d)
  # don't attempt to redefine proc params!
  result = newStmtList()

proc addAssignment(e: var Env; section: VarLetIdentDef): NimNode =
  ## compose an assignment during addition of var|let identDefs to env
  when cpsDebug == "Env":
    echo $section.kind, "\t", repr(section)
  let (field, value) = e.addIdentDef(section.kind, section.identdef())
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
  template maybeAdd(x) =
    if not into.isNil:
      into.add x

  if n.isTuple:
    # deconstruct the RHS types into multiple assignments
    # let (a, b, c) = foo() -> (env.a, env.b, env.c) = foo()
    let
      defs = n.asVarLetTuple()
      child = e.castToChild(e.first)
      tups = nnkTupleConstr.newTree
    for index, name in defs.indexNamePairs:
      let name = asSym(name)
      # we need to insert the variable and then write a new
      # accessor that plucks the field from the env
      let (field, _) = e.addIdentDef n.kind:
        newIdentDef(name, name.typeInst, newEmptyNode())
      tups.add newDotExpr(child, field)
    maybeAdd newAssignment(tups, defs.val)
  else:
    # an iterator handles `var a, b, c = 3` appropriately,
    # so we can assume that only a single identdefs exists
    (NimNode n)[0] = addInitializationToDefault(n[0].NimNode)
    maybeAdd e.addAssignment(n.asVarLetIdentDef)

proc localSection*(e: var Env; n: IdentDef; into: NimNode = nil) =
  ## consume nnkIdentDefs and populate `into` with assignments, even if `into`
  ## is nil, the `n` will be cached locally
  let assignment = e.addAssignment(n)
  if not into.isNil:
    into.add assignment

proc localSection*(e: var Env; n: RoutineParam; into: NimNode = nil) {.borrow.}
  ## consume proc definition params and yield name, node pairs representing
  ## assignments to local scope.

proc rewriteReturn*(e: var Env; n: NormNode): NormNode =
  ## Rewrite a return statement to use our result field.
  if n.len != 1:
    result = n.errorAst "return len != 1"
  else:
    case n[0].kind
    of nnkAsgn:
      # okay, it's a return: result = ...
      result = newStmtList()
      # ignore the result symbol and create a new assignment
      result.add newAssignment(e.getResult, n.last.last)
      # and add the termination annotation
      result.add newCpsTerminate()
    of nnkEmpty:
      # this is an empty return
      result = newCpsTerminate()
    else:
      # okay, it's a return of some rando expr
      result = newStmtList()
      # ignore the result symbol and create a new assignment
      result.add newAssignment(e.getResult, n.last)
      # and add the termination annotation
      result.add newCpsTerminate()

proc rewriteSymbolsIntoEnvDotField*(e: var Env; n: NormNode): NormNode =
  ## swap symbols for those in the continuation
  result = n
  let child = e.castToChild e.first
  for field, section in e.pairs:
    # we don't resym identifiers, which we must have created;
    # these are a side-effect of an open nim bug
    let sym = section.name
    if sym.isSymbol:
      result = result.resym(sym, newDotExpr(child, field))
    else:
      {.warning: "pending https://github.com/nim-lang/Nim/issues/17851".}

proc createContinuation*(e: Env; name: Name; goto: NimNode): NimNode =
  ## allocate a continuation as `name` and maybe aim it at the leg `goto`
  proc resultdot(n: Name): NormNode =
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
    newLetIdentDef(ex, newRefType(bindName("Exception")), newNilLit())
  result = newDotExpr(e.castToChild(e.first), ex)

proc createRecover*(env: Env, exported = false): NimNode =
  ## define procedures for retrieving the result of a continuation
  ##
  ## `exported` determines whether these procedures will be exported

  # a body of a case clause which has the return type of the continuation
  let recoveredResult =
    NimNode:                   # for passing into genast()
      if env.rs.hasType:
        env.getResult          # get the result field from the environment
      else:
        nnkDiscardStmt.newTree:
          newEmptyNode()       # the return value is void; simply discard

  # the result fetcher used in the Callback shim has a gensym'd proc name
  let fetch =
    genProcName(procedure env, "recover", info=env.store.NormNode).NimNode

  # the recover procedure name
  let recover = NimNode: ident"recover"

  let tipe =
    case env.rs.typ.typeKind
    of ntyNone, ntyEmpty, ntyVoid:
      newEmptyNode()           # downcast to void return type
    else:
      NimNode:                 # for passing into genast()
        env.rs.typ             # user supplied a return type

  proc star(name: NimNode): NimNode =
    ## add the "*" postfix operator to the input ident/symbol if appropriate
    result = copyNimTree name
    if exported:
      result = postfix(result, "*")

  result =
    genAstOpt({}, tipe, recoveredResult, fetch, fetchStar = fetch.star,
              recoverStar = recover.star, c = env.first.NimNode,
              cont = env.identity.NimNode, contBase = env.inherits.NimNode,
              dismissed=Dismissed, finished=Finished, running=Running):

      proc fetchStar(c: var contBase): tipe {.used, nimcall.} =
        case c.state
        of dismissed:
          raise Defect.newException:
            "dismissed continuations have no result"
        of finished:
          recoveredResult
        of running:
          var base: contBase = move c  # mutable object conversion
          defer: c = cont(move base)   # due to void return types
          base = trampoline(base)
          fetch(base)

      proc recoverStar(c: var cont): tipe {.used, nimcall.} =
        var base: contBase = move c    # mutable object conversion
        defer: c = cont(move base)     # due to void return types
        fetch(base)

  when cpsCallOperatorSupported and not defined cpsNoCallOperator:
    let callOperator = nnkAccQuoted.newTree ident"()"
    result.add:
      genAstOpt({}, tipe, recover, callOperator = callOperator.star,
                cont = env.identity.NimNode):
        {.push experimental: "callOperator".}
        proc callOperatorStar(c: var cont): tipe {.used, nimcall.} =
          recover(c)
        {.pop.}

proc createWhelp*(env: Env; n: ProcDef; goto: NormNode): ProcDef =
  ## the whelp needs to create a continuation
  let resultName = "result".asName(n)

  result = clone(n, newStmtList())
  result.addPragma "used"  # avoid gratuitous warnings
  result.addPragma "nimcall"
  result.returnParam = env.identity
  result.name = genProcName(procedure env, "whelp", info=n)
  result.introduce {Alloc, Boot, Stack}

  # create the continuation as the result and point it at the proc
  result.body.add:
    env.createContinuation(resultName, desym goto)

  # hook the bootstrap
  result.body.add:
    newAssignment resultName:
      Stack.hook initFrame(Stack, $goto, goto.lineInfoObj).NormNode:
        newCall env.identity:
          Boot.hook newCall(env.root, resultName)

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs.name)

proc createCallbackShim*(env: Env; whelp: ProcDef): ProcDef =
  ## this is a version of whelp that returns the base continuation type
  result = clone(whelp, newStmtList())
  result.returnParam = env.inherits
  result.name = genProcName(procedure env, "callback", info=whelp)
  # whelp_234(a, b, c)
  result.body = newCall whelp.name
  for defs in result.callingParams:
    result.body.add defs.name
  # C: whelp_234(a, b, c)
  result.body = newCall(result.returnParam, result.body)

proc createBootstrap*(env: Env; n: ProcDef, goto: NormNode): ProcDef =
  ## the bootstrap needs to create a continuation and trampoline it
  result = clone(n, newStmtList())
  result.addPragma "used"  # avoid gratuitous warnings
  result.addPragma "nimcall"
  result.introduce {Alloc, Boot, Stack}

  let c = genSymVar("c", info = n)
  result.body.add:
    # declare `var c: Cont`
    newVarSection(c, env.root)

  # create the continuation using the new variable and point it at the proc
  result.body.add:
    env.createContinuation(c, desym goto)

  # hook the bootstrap
  result.body.add:
    newAssignment c:
      Head.hook:
        Stack.hook initFrame(Stack, $goto, goto.lineInfoObj).NormNode:
          Boot.hook c

  # rewrite the symbols used in the arguments to identifiers
  for defs in result.callingParams:
    result = desym(result, defs.name)

  # now the trampoline
  result.body.add:
    newAssignment(c, newCall(bindName"trampoline", c))

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
        nnkElifExpr.newTree(
          # check if the continuation is not nil, and if so, to
          newCall(bindSym"not", newDotExpr(c, asName"dismissed")),
          # assign the result from the continuation's result field
          newAssignment(asName"result",
            newDotExpr(env.castToChild(c), env.rs.name))
        )

proc rewriteVoodoo*(env: Env; n: NormNode): NormNode =
  ## Rewrite non-yielding cpsCall calls by inserting the continuation as
  ## the first argument
  proc voodoo(n: NormNode): NormNode =
    if n.isVoodooCall:
      let it = asCall n.copyNimTree
      # https://github.com/nim-lang/Nim/issues/18365
      it.prependArg newCall(env.root, env.first)
      desym it
      result = env.rewriteVoodoo it
  result = filter(n, voodoo)
