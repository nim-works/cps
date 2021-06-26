import std/macros
from std/hashes import Hash, hash
from std/sequtils import anyIt, toSeq
from std/typetraits import distinctBase

from cps/rewrites import NormalizedNimNode, normalizingRewrites, replace,
                         desym, resym

export NormalizedNimNode

# Parts of this module:
# * distinct types representing normalized/transformed variants of distinct AST
# * procs use to query, transform, etc on the normalized AST as conveniences
#
# Why each part:
# * Normalized types - codify the pre/post conditions of CPS code
# * Normalized procs - ensure invariants by centralizing the AST operations

type
  Name* = distinct NormalizedNimNode
    ## either an Ident or Sym
  Ident* = distinct Name
    ## nnkIdent
  Sym* = distinct Name
    ## nnkSym
  TypeExpr* = distinct NormalizedNimNode
    ## the type part of a let or var definition or a proc param

  IdentDefs* = distinct NormalizedNimNode
    ## currently this is IdentDefs mostly in a var let section, in reality
    ## these are also for routine and generic param definitions

  ProcDef* = distinct NormalizedNimNode
    ## an nnkProcDef node which has been normalized

  RoutineParam* = distinct IdentDefs
    ## each calling params of proc definition is an IdentDefs

  VarLet* = distinct NormalizedNimNode
    ## a var or let section, with a single define
  VarLetDef = distinct NormalizedNimNode
    ## identdef or tuple defintion from a var or let section

  TupleVarLet* = distinct VarLet
    ## a var or let section, but with a tuple defintion within
  IdentDefVarLet* = distinct VarLet
    ## a var or let section, but with a single identdefs, eg: `var a: int = 10`

  LetSection* = distinct VarLet
    ## a let section, with a single identdefs or vartuple

  VarSection* = distinct VarLet
    ## a let or var section, with a single identdefs or vartuple
  IdentDefLet* = distinct IdentDefVarLet
    ## identdef defintion from a let section
  IdentDefVar* = distinct IdentDefVarLet
    ## identdef defintion from a var section

  IdentDefLike* = IdentDefs | RoutineParam
    ## single var, let, or a proc definition's calling param
  DefLike* = IdentDefLike | VarLetDef
    ## IdentDefs could be a single variable define or a proc def param, while
    ## a VarLetDef is an identdefs or vartuple from a var or let section

  LetSectionLike* = LetSection
    ## abstract over various forms of let sections, used to define operations
    ## over or allow abstracting over this type class further
  VarSectionLike* = VarSection | IdentDefVar
    ## abstract over various var sections types
  VarLetLike* = VarLet | TupleVarLet | IdentDefVarLet | LetSectionLike |
               VarSectionLike
    ## abstract over various let or var sections types
  VarLetIdentDefLike* = IdentDefVarLet | IdentDefVar
    ## abstract over identdefs from let or var sections types
  
  ExprLike* = Name | NormalizedNimNode
    ## abstract over any nim value expression
  TypeExprLike* = Name | TypeExpr

func errorGot(msg: string, n: NimNode, got: string = repr(n)) =
  ## useful for error messages
  error msg & ", got:\n" & repr(got), n

proc normalizeProcDef*(n: NimNode): ProcDef =
  ## ensure this is a normalized procd definition
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

proc add*(f, c: NormalizedNimNode): NormalizedNimNode {.discardable.} =
  ## hopefully this fixes ambiguous call issues
  f.NimNode.add(c.NimNode).NormalizedNimNode
proc add*(f: NimNode, c: NormalizedNimNode): NimNode {.borrow, discardable.}
  ## hopefully this fixes ambiguous call issues

# XXX: Temporary procs until we get more strongly typed versions

proc copy*(n: NormalizedNimNode): NormalizedNimNode {.borrow.}
proc copyNimNode*(n: NormalizedNimNode): NormalizedNimNode {.borrow.}
proc copyNimTree*(n: NormalizedNimNode): NormalizedNimNode {.borrow.}

# XXX: make a pragma type and wrap this up

const
  ConvNodes* = {nnkHiddenStdConv..nnkConv}
    ## Conversion nodes in typed AST

  AccessNodes* = AtomicNodes + {nnkDotExpr, nnkDerefExpr, nnkHiddenDeref,
                                nnkAddr, nnkHiddenAddr}
    ## AST nodes for operations accessing a resource

  ConstructNodes* = {nnkBracket, nnkObjConstr, nnkTupleConstr}
    ## AST nodes for construction operations

proc getPragmaName*(n: NimNode): NimNode =
  ## retrieve the symbol/identifier from the child node of a nnkPragma
  case n.kind
  of nnkCall, nnkExprColonExpr:
    n[0]
  else:
    n

func hasPragma*(n: NimNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  case n.kind
  of nnkPragma:
    for p in n.items:
      # just skip ColonExprs, etc.
      result = p.getPragmaName.eqIdent s
      if result:
        break
  of RoutineNodes:
    result = hasPragma(n.pragma, s)
  of nnkObjectTy:
    result = hasPragma(n[0], s)
  of nnkRefTy:
    result = hasPragma(n.last, s)
  of nnkTypeDef:
    result = hasPragma(n.last, s)
  of nnkTypeSection:
    result = anyIt(toSeq items(n), hasPragma(it, s))
  else:
    result = false

# Converters - because plastering `.NimNode` makes everyone sad

template defineToNimNodeConverter(t: typedesc) =
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode
  
# fn-NormalizedNimNode

defineToNimNodeConverter(NormalizedNimNode)
template hash*(n: NormalizedNimNode): Hash =
  hash(n.NimNode)
proc desym*(n: NormalizedNimNode, sym: NimNode): NormalizedNimNode =
  ## desym all occurences of a specific sym
  n.replace(proc(it: NimNode): bool = it == sym, desym sym).NormalizedNimNode
func last*(n: NormalizedNimNode): NormalizedNimNode {.borrow.}
proc onlyNormalizedNimNode*[T: distinct](n: T): NormalizedNimNode =
  ## used for conversion in an vararg scenarios primarily
  when distinctBase(T) is NimNode:
    n
  else:
    errorGot "invalid type, expected some NormalizedNimNode", n
func kind*(n: NormalizedNimNode): NimNodeKind {.borrow.}
func len*(n: NormalizedNimNode): int {.borrow.}

proc newStmtList*(stmts: varargs[NormalizedNimNode, onlyNormalizedNimNode]): NormalizedNimNode =
  ## create a new normalized statement
  result = macros.newStmtList().NormalizedNimNode
  for s in stmts:
    result.NimNode.add s.NimNode

# fn-Name

# defineToNimNodeConverter(Name)
template hash*(n: Name): Hash =
  hash(n.NimNode)
func isNil*(n: Name): bool {.borrow.}
func `==`*(a, b: Name): bool {.borrow.}
func `==`*(a: NimNode, b: Name): bool {.borrow.}
func `$`*(a: Name): string {.borrow.}
func strVal*(n: Name): string {.borrow.}
func isSymbol*(n: Name): bool =
  n.NimNode.kind == nnkSym
proc asName*(n: NimNode): Name =
  ## coerce to `Name`
  if n.kind notin {nnkIdent, nnkSym}:
    errorGot "not an ident or sym", n
  n.Name
proc asNameAllowEmpty*(n: NimNode): Name =
  ## coerce to `Name`, allow `nnkEmpty`, error out otherwise
  if n.kind notin {nnkIdent, nnkSym, nnkEmpty}:
    errorGot "not an ident, sym, or empty", n
  n.Name
proc asName*(n: string): Name =
  ## `nnkIdent` as `Name`
  (ident n).Name
proc newTypeName*(n: string): Name =
  ## `genSym` an `nskType`
  genSym(nskType, n).Name
proc newVarName*(n: string = ""): Name =
  ## `genSym` an `nskVar`
  genSym(nskVar, n).Name
proc newLetName*(n: string): Name =
  ## `genSym` an `nskLet`
  genSym(nskLet, n).Name
proc newProcName*(n: string): Name =
  ## `genSym` an `nskProc`
  genSym(nskProc, n).Name
proc newUnknownName*(n: string): Name =
  ## `genSym` an `nskUnknown`
  genSym(nskUnknown, n).Name
proc bindName*(n: static string): Name =
  ## `bindSym` the string as a `Name`
  let r = bindSym(n)
  r.Name
proc bindName*(n: static string, rule: static BindSymRule): Name =
  ## `bindSym` the string as a `Name` and specified bind sym `rule`
  let r = bindSym(n, rule)
  r.Name
proc desym*(n: Name): Name {.borrow.}
  ## ensures that `Name` is an `nnkIdent`
proc resym*(fragment: NormalizedNimNode, sym, replacement: Name): NormalizedNimNode {.borrow.}
  ## replace `sym` in the AST `fragment` with the `replacement`

func typeInst*(n: Name): NimNode =
  ## gets the type via `getTypeInst`
  ## XXX: this shouldn't be done on name, as these could be bare idents
  getTypeInst n.NimNode

func eqIdent*(a: Name|NimNode, b: Name|NimNode): bool =
  # XXX: either a converter or higher level refactoring will remove the need
  #      for this func
  eqIdent(
    when a isnot NimNode: a.NimNode else: a,
    when b isnot NimNode: b.NimNode else: b
  )

converter nameToNormalizedNimNode*(n: Name): NormalizedNimNode =
  ## downgrade a `Name` to a `NormalizedNimNode`
  n.NormalizedNimNode

# fn-ExprLike
proc newDotExpr*(l: ExprLike, r: distinct ExprLike): NormalizedNimNode =
  ## create a new dot expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example
  newDotExpr(
    when l isnot NimNode: l.NimNode else: l,
    when r isnot NimNode: r.NimNode else: r
  ).NormalizedNimNode
proc newColonExpr*(l: ExprLike, r: distinct ExprLike): NormalizedNimNode =
  ## create a new colon expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example
  newColonExpr(
    when l isnot NimNode: l.NimNode else: l,
    when r isnot NimNode: r.NimNode else: r
  ).NormalizedNimNode
proc newAssignment*(l: ExprLike, r: distinct ExprLike): NormalizedNimNode =
  ## create a new assignment, meant for executable code
  newAssignment(
    when l isnot NimNode: l.NimNode else: l,
    when r isnot NimNode: r.NimNode else: r
  ).NormalizedNimNode
proc newCall*(n: Name, arg: ExprLike): NormalizedNimNode =
  ## create a new call with a single arg
  newCall(
    when n isnot NimNode: n.NimNode else: n,
    when arg isnot NimNode: arg.NimNode else: arg
  ).NormalizedNimNode
proc newCall*(n: Name, args: varargs[NormalizedNimNode, onlyNormalizedNimNode]): NormalizedNimNode =
  ## create a new call, with `n` as name some args
  result = newCall(n.NimNode).NormalizedNimNode
  for a in args:
    result.add a
proc newCall*(n: string, args: varargs[NormalizedNimNode, onlyNormalizedNimNode]): NormalizedNimNode =
  ## create a new call, with `n` as and ident name, and a single arg
  result = newCall(asName(n))
  for a in args:
    result.add a
proc newCall*(n: string, arg: ExprLike): NormalizedNimNode =
  ## create a new call, with `n` as and ident name, and a single arg
  newCall(
    asName(n).NimNode,
    when arg isnot NimNode: arg.NimNode else: arg
  ).NormalizedNimNode
proc newCall*[A: ExprLike, B: ExprLike](n: string, a: A, b: B): NormalizedNimNode =
  ## create a new call, with `n` as and ident name, and two args
  newCall(
    asName(n).NimNode,
    when a isnot NimNode: a.NimNode else: a,
    when b isnot NimNode: b.NimNode else: b
  ).NormalizedNimNode
proc newCall*(n: NormalizedNimNode, args: varargs[NormalizedNimNode, onlyNormalizedNimNode]): NormalizedNimNode =
  ## create a new call, with `n` as name some args
  result = newCall(n.NimNode).NormalizedNimNode
  for a in args:
    result.add a

# fn-TypeExpr
proc asTypeExpr*(n: NimNode): TypeExpr =
  if n.kind notin {nnkIdent, nnkSym, nnkVarTy, nnkRefTy}:
    # XXX: incomplete list of type kinds
    errorGot "not a type expression", n
  n.TypeExpr
proc newRefType*(n: Name): TypeExpr =
  nnkRefTy.newTree(n.NimNode).TypeExpr

# fn-Ident

defineToNimNodeConverter(Ident)
proc asIdent*(n: NimNode): Ident =
  if n.kind != nnkIdent:
    errorGot "not an ident", n
  n.Ident
proc asIdent*(n: string): Ident =
  (ident n).Ident

# fn-DefLike

func typ*(n: DefLike): NimNode = n.NimNode[^2]
func val*(n: DefLike): NimNode = n.NimNode[^1]
func hasValue*(n: DefLike): bool =
  ## has a non-Empty initial value defined for the ident, sym or tuple
  ## Yes, proc, you ARE a good proc. You have value, hasValue, in fact.
  n.val.kind != nnkEmpty

func hasType*(n: DefLike): bool =
  ## has a non-Empty type (`typ`) defined
  n.typ.kind != nnkEmpty

func inferTypFromImpl*(n: DefLike): Name =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ.Name else: getTypeImpl(n.val).Name

# fn-IdentDefs

defineToNimNodeConverter(IdentDefs)

proc validateIdentDefs(n: NimNode) =
  ## validators only, afterwards it's safe to cast, allows re-use
  if n.kind != nnkIdentDefs:
    errorGot "not an IdentDefs", n, $n.kind
  elif n[0].kind notin {nnkIdent, nnkSym}:
    errorGot "bad rewrite presented", n
  elif n.len != 3:
    errorGot "bad rewrite, failed to set init", n
proc expectIdentDefs*(n: NimNode): IdentDefs =
  ## return an IdentDef or error out
  validateIdentDefs(n)
  return n.IdentDefs

proc newIdentDefs*(n: string, t: TypeExprLike, val = newEmptyNode()): IdentDefs =
  newIdentDefs(ident(n), t.NimNode, val).IdentDefs
proc newIdentDefs*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefs =
  newIdentDefs(n.NimNode, t.NimNode, val).IdentDefs
proc newIdentDefs*(n: Name, val: NormalizedNimNode): IdentDefs =
  newIdentDefs(n.NimNode, newEmptyNode(), val).IdentDefs

# fn-IdentDefLike

func name*(n: IdentDefLike): Name = n.NimNode[0].Name

# fn-VarLetLike

func def*(n: VarLetLike): VarLetDef = VarLetDef n.NimNode[0]
  ## an IdentDefs or VarTuple
func typ*(n: VarLetLike): NimNode = n.def.typ
  ## the type of this definition (IdentDef or VarTuple)
func val*(n: VarLetLike): NormalizedNimNode = n.def.val.NormalizedNimNode
  ## the ident or sym being defined, or tuple being defined
func kind*(n: VarLetLike): NimNodeKind = n.NimNode.kind
func hasValue*(n: VarLetLike): bool = n.def.hasValue
  ## whether an initial value has been specified
func hasType(n: VarLetLike): bool = n.def.typ.kind != nnkEmpty
  ## whether an explicit type is defined
func isTuple*(n: VarLetLike): bool = n.def.NimNode.kind == nnkVarTuple

func validateAndCoerce(n: NimNode, T: typedesc = type VarLet): T =
  const sectionName =
    when T is LetSectionLike: "let"
    elif T is VarSectionLike: "var"
    elif T is VarLetLike:     "let or var"
    else:                     {.error.}
  const sectionKinds =
    when T is LetSectionLike: {nnkLetSection}
    elif T is VarSectionLike: {nnkVarSection}
    elif T is VarLetLike:     {nnkLetSection, nnkVarSection}
    else:                     {.error.}
  const checkIdentDef =
    when T is VarLetIdentDefLike: true
    elif T is VarLetLike:         false
    else:                         {.error.}
  const checkTuple =
    when T is TupleVarLet: true
    elif T is VarLetLike:  false
    else:                  {.error.}
  
  if n.kind notin sectionKinds:
    errorGot "not a " & sectionName & " section", n
  elif n.len != 1:
    errorGot sectionName & " has " & $n.len & " defs, requires exactly 1", n
  elif checkIdentDef:
    if n[0].kind != nnkIdentDefs:
      errorGot sectionName & " section must contain an IdentDefs", n
    validateIdentDefs(n[0])
  elif checkTuple and n[0].kind != nnkVarTuple:
    errorGot sectionName & " section must be a tuple assignment", n
  return n.T

# fn-VarLetIdentDefLike

func identdef*(n: VarLetIdentDefLike): IdentDefs = n.NimNode[0].IdentDefs
  ## retrieve the innner IdentDef
func name*(n: VarLetIdentDefLike): Name = n.identdef.name
  ## Name (ident|sym) of the identifer, as we only have a single identdefs it
  ## will have the name
func inferTypFromImpl*(n: VarLetIdentDefLike): Name =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ.Name else: getTypeImpl(n.val).Name

# fn-VarLet

proc expectVarLet*(n: NimNode): VarLet =
  ## return a VarLet if this is a var or let section, otherwise error out
  validateAndCoerce(n)

func errorGot(msg: string, n: VarLetLike, got: string = repr(n)) =
  errorGot(msg, n.NimNode, got)

proc asVarLetTuple*(n: VarLet): TupleVarLet =
  ## return a TupleVarLet if the def is a VarTuple, otherwise error out
  validateAndCoerce(n.NimNode, TupleVarLet)
proc asVarLetIdentDef*(n: VarLet): IdentDefVarLet =
  ## return a IdentDefVarLet if the def is an IdentDef, otherwise error out
  validateAndCoerce(n.NimNode, IdentDefVarLet)

func clone*(n: VarLet, value: NimNode = nil): VarLet =
  ## clone a `VarLet` but with `value` changed
  let def = copyNimNode(n.def.NimNode)
  # copy all nodes in the original definition excluding the last node, which
  # is the value.
  for idx in 0 ..< n.def.NimNode.len - 1:
    def.add copy(n.def.NimNode[idx])

  # add the value replacement
  # if one is not given we copy the value too
  if value.isNil:
    def.add copy(n.def.val)
  else:
    def.add copy(value)

  # copy the varlet and add the new def
  result = expectVarLet:
    copyNimNode(n.NimNode).add def

converter varLetToNormalizedNimNode*(n: VarLet): NormalizedNimNode =
  ## downgrade a `VarLet` to a `NormalizedNimNode`
  n.NormalizedNimNode

# fn-TupleVarLet

proc typ*(n: TupleVarLet): NimNode =
  ## return the type based on `getTypeInst`
  getTypeInst n.val
iterator indexNamePairs*(n: TupleVarLet): (int, NimNode) =
  ## return the names of fields on the lhs of a var/let tuple assignment
  for index, name in n.def.NimNode[0 .. ^3].pairs:
    yield (index, name)

# fn-IdentDefVarLet

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDefs): IdentDefVarLet =
  ## create a new IdentDefVarLet
  doAssert kind in {nnkLetSection, nnkVarSection},
    "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  newTree(kind, i).IdentDefVarLet
proc newVarLetIdentDef*(kind: NimNodeKind,
                        name: Name, typ, val: NimNode): IdentDefVarLet =
  ## create a new IdentDefVarLet
  newVarLetIdentDef(kind, newIdentDefs(name.NimNode, typ, val).IdentDefs)

converter identDefVarLetToNormalizedNimNode*(n: IdentDefVarLet): NormalizedNimNode =
  # allow downgrading
  n.NormalizedNimNode

# fn-VarSection

defineToNimNodeConverter(VarSection)

proc newVarSection*(i: IdentDefs): VarSection =
  (nnkVarSection.newTree i).VarSection
proc newVarSection*(n: Name, t: TypeExprLike, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newVarSection(newIdentDefs(n, t, val))

converter varSectionToNormalizedNimNode*(n: VarSection): NormalizedNimNode =
  # allow downgrading
  n.NormalizedNimNode

# fn-IdentDefLet

proc newIdentDefLet*(i: IdentDefs): IdentDefLet =
  ## create a var section with an identdef
  (nnkLetSection.newTree i).IdentDefLet
proc newIdentDefLet*(n: Name, val: NormalizedNimNode): IdentDefLet =
  newIdentDefLet(newIdentDefs(n, val))
proc newIdentDefLet*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefLet =
  ## create a let section with an identdef, eg: `n`: `t` = `val`
  newIdentDefLet(newIdentDefs(n, t, val))

converter identDefLetToIdentDefVarLet*(n: IdentDefLet): IdentDefVarLet =
  # allow downgrading
  n.IdentDefVarLet

converter identDefLetToNormalizedNimNode*(n: IdentDefLet): NormalizedNimNode =
  # allow downgrading
  n.NormalizedNimNode

# fn-IdentDefVar

proc newIdentDefVar*(i: IdentDefs): IdentDefVar =
  ## create a var section with an identdef
  (nnkVarSection.newTree i).IdentDefVar
proc newIdentDefVar*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefVar =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newIdentDefVar(newIdentDefs(n, t, val))

converter identDefVarToIdentDefVarLet*(n: IdentDefVar): IdentDefVarLet =
  # allow downgrading
  n.IdentDefVarLet

# fn-ProcDefParams

converter procDefParamToIdentDefs*(n: RoutineParam): IdentDefs =
  # allow downgrading
  n.IdentDefs

# fn-ProcDef

defineToNimNodeConverter(ProcDef)

proc newProcDef*(name: Name, formalParams: openArray[NimNode]): ProcDef =
  ## create a new proc def with name and formal params (includes return type)
  ## and an empty body (`nnkStmtList`)
  newProc(name.Nimnode, formalParams, newStmtList()).ProcDef

func asProcDef*(n: NimNode): ProcDef =
  ## coerce into a `ProcDef` or error out
  if n.kind != nnkProcDef:
    errorGot "not a proc definition", n
  n.ProcDef

func name*(n: ProcDef): Name =
  ## get the name of this ProcDef
  n.NimNode.name.Name
proc `name=`*(n: ProcDef, name: Name) {.borrow.}

proc body*(n: ProcDef): NormalizedNimNode {.borrow.}
proc `body=`*(n: ProcDef, b: NormalizedNimNode) {.borrow.}

func len*(n: ProcDef): int {.borrow.}

func formalParams(n: ProcDef): NimNode =
  ## first one will be the return, meant to be used internally
  n.NimNode.params

proc firstCallParam*(n: ProcDef): RoutineParam =
  ## returns the first call param for this proc def, useful due to CPS
  ## proc often just have one continuation parameter that we access often.
  if n.formalParams.len < 2:
    errorGot "proc definition does not take calling params", n.NimNode
  n.formalParams[1].RoutineParam

proc desym*[T: ProcDef](n: T, sym: Name): T =
  ## desym the routine
  desym(n.NormalizedNimNode, sym.NimNode).T

proc addPragma*(n: ProcDef, prag: Name) {.borrow.}
  ## add a pragma (`prag`) to the definition: `{.prag.}`

proc addPragma*(n: ProcDef, prag: string) =
  ## add the pragma (`prag`) as an ident to this definition
  n.addPragma asName(prag)

proc addPragma*(n: ProcDef, prag: Name, pragArg: NimNode) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  n.addPragma:
    nnkExprColonExpr.newTree(prag.NimNode, pragArg)

proc addPragma*(n: ProcDef, prag: Name, pragArg: Name) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  addPragma(n, prag, pragArg.NimNode)

proc addPragma*(n: ProcDef, prag: Name, pragArgs: openArray[Name]) =
  ## add pragmas of the form `{.raise: [IOError, OSError].}`
  addPragma(n, prag, pragArgs)

func hasPragma*(n: ProcDef, s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  hasPragma(n.NimNode, s)

func returnParam*(n: ProcDef): NormalizedNimNode =
  ## the return param or empty if void
  ## XXX: should return TypeExpr or Name, need to check rewrite restrictions 
  n.NimNode.params[0].NormalizedNimNode

proc `returnParam=`*(n: ProcDef, ret: Name) =
  ## set the return param
  n.params[0] = ret.NimNode

iterator callingParams*(n: ProcDef): RoutineParam =
  ## iterate over formal parameters used to call this routine, excludes the
  ## return parameter.
  for a in n.formalParams[1..^1].items:
    yield a.RoutineParam

proc clone*(n: ProcDef, body: NimNode = nil): ProcDef =
  ## create a copy of a typed proc which satisfies the compiler
  result = nnkProcDef.newTree(
    ident(repr n.name),         # repr to handle gensymbols
    newEmptyNode(),
    newEmptyNode(),
    copy n.params,              # parameter normalization will mutate these
    newEmptyNode(),
    newEmptyNode(),
    if body == nil: macros.copy(n.body) else: body
  ).ProcDef
  result.copyLineInfo n

converter procDefToNormalizedNimNode*(n: ProcDef): NormalizedNimNode =
  # allow downgrading
  n.NormalizedNimNode
