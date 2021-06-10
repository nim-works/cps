import std/macros
from std/hashes import Hash, hash

from cps/rewrites import normalizingRewrites, replace, desym

# Parts of this module:
# * distinct types representing normalized/transformed variants of distinct AST
# * procs use to query, transform, etc on the normalized AST as conveniences
#
# Why each part:
# * Normalized types - codify the pre/post conditions of CPS code
# * Normalized procs - ensure invariants by centralizing the AST operations

type
  NormalizedNimNode* = distinct NimNode
    ## a normalized node, but this should not be useed directly, use a
    ## specialized type instead, see below.

  # Outside of NormalizedNimNode, nodes fall into a few cateogries:
  # * one to one to an AST node, but with some invariants (eg: `Ident`)
  # * over multiple node kinds, but cover common operations (eg: `VarLet`)
  # * to a normalized type node, but further specialization (eg: `TupleVarLet`)

  Ident* = distinct Name
    ## nnkIdent
  Sym* = distinct Name
    ## nnkSym
  Name* = distinct NormalizedNimNode
    ## either an Ident or Sym

  ProcDef* = distinct NormalizedNimNode
    ## an nnkProcDef node which has been normalized
  
  IdentDefs* = distinct NormalizedNimNode
    ## currently this is IdentDefs mostly in a var let section, in reality
    ## these are also for routine and generic param definitions

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
  IdentDefVar* = distinct IdentDefVarLet
    ## identdef defintion from a var section

  DefLike* = IdentDefs | VarLetDef
    ## IdentDefs could be a singla variable define or a proc def param, while
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

func errorGot(msg: string, n: NimNode, got: string = repr(n)) =
  ## useful for error messages
  error msg & ", got:\n" & repr(got), n

proc normalizeProcDef*(n: NimNode): ProcDef =
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

# Converters - because plastering `.NimNode` makes everyone sad

template defineToNimNodeConverter(t: typedesc) =
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode

# fn-NormalizedNimNode

defineToNimNodeConverter(NormalizedNimNode)
template hash*(n: NormalizedNimNode) =
  hash(n.NimNode)
proc desym*(n: NormalizedNimNode, sym: NimNode): NormalizedNimNode =
  ## desym all occurences of a specific sym
  n.replace(proc(it: NimNode): bool = it == sym, desym sym).NormalizedNimNode

# fn-Name

# defineToNimNodeConverter(Name)
template hash*(n: Name): Hash =
  hash(n.NimNode)
func isNil*(n: Name): bool {.borrow.}
func `==`*(a, b: Name): bool {.borrow.}
func strVal*(n: Name): string {.borrow.}
func isSymbol*(n: Name): bool =
  n.NimNode.kind == nnkSym
proc asName*(n: NimNode): Name =
  if n.kind notin {nnkIdent, nnkSym}:
    errorGot "not an ident or sym", n
  n.Name
proc asName*(n: string): Name =
  (ident n).Name
proc newTypeName*(n: string): Name =
  genSym(nskType, n).Name

func eqIdent*(a: Name|NimNode, b: Name|NimNode): bool =
  # XXX: either a converter or higher level refactoring will remove the need
  #      for this func
  eqIdent(
    when a isnot NimNode: a.NimNode else: a,
    when b isnot NimNode: b.NimNode else: b
  )

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

func inferTypFromImpl*(n: DefLike): NimNode =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: getTypeImpl(n.val)

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

proc newIdentDefs*(n: string, typ: NimNode, val = newEmptyNode()): IdentDefs =
  newIdentDefs(ident(n), typ, val).IdentDefs
proc newIdentDefs*(n: Name, typ: NimNode, val = newEmptyNode()): IdentDefs =
  newIdentDefs(n.NimNode, typ, val).IdentDefs

func name*(n: IdentDefs): Name = n[0].Name

# fn-VarLetLike

func def*(n: VarLetLike): VarLetDef = VarLetDef n.NimNode[0]
  ## an IdentDefs or VarTuple
func typ*(n: VarLetLike): NimNode = n.def.typ
  ## the type of this definition (IdentDef or VarTuple)
func val*(n: VarLetLike): NimNode = n.def.val
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
func inferTypFromImpl*(n: VarLetIdentDefLike): NimNode =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: getTypeImpl(n.val)

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

# fn-VarSection

defineToNimNodeConverter(VarSection)

proc newVarSection*(i: IdentDefs): VarSection =
  (nnkVarSection.newTree i).VarSection
proc newVarSection*(n, typ: NimNode, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `typ` = `val`
  newVarSection(newIdentDefs(n, typ, val).IdentDefs)

# fn-IdentDefVar

proc newIdentDefVar*(i: IdentDefs): IdentDefVar =
  ## create a var section with an identdef
  (nnkVarSection.newTree i).IdentDefVar
proc newIdentDefVar*(n: Name, t: NimNode, val = newEmptyNode()): IdentDefVar =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newIdentDefVar(newIdentDefs(n, t, val).IdentDefs)

converter identDefVarToIdentDefVarLet*(n: IdentDefVar): IdentDefVarLet =
  # allow downgrading
  n.IdentDefVarLet

# fn-ProcDef

defineToNimNodeConverter(ProcDef)

func returnParam*(n: ProcDef): NimNode =
  ## the return param or empty if void
  n.params[0]

func `returnParam=`*(n: ProcDef, ret: NimNode) =
  ## set the return param
  ## XXX: remove normalizingRewrites once this is typed
  n.params[0] = normalizingRewrites ret

func `name=`*(n: ProcDef, name: NimNode) {.borrow.}

proc clone*(n: ProcDef, body: NimNode = nil): ProcDef =
  ## create a copy of a typed proc which satisfies the compiler
  result = nnkProcDef.newTree(
    ident(repr n.name),         # repr to handle gensymbols
    newEmptyNode(),
    newEmptyNode(),
    copy n.params,              # parameter normalization will mutate these
    newEmptyNode(),
    newEmptyNode(),
    if body == nil: copy n.body else: body).ProcDef
  result.copyLineInfo n

iterator callingParams*(n: ProcDef): NimNode =
  for a in n.params[1..^1].items:
    yield a

proc desym*(n: ProcDef, sym: NimNode): ProcDef {.borrow.}

proc addPragma*(n: ProcDef, prag: NimNode) {.borrow.}

proc addPragma*(n: ProcDef, prag: NimNode, pragArg: NimNode) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  ##
  ## XXX: there is a bracket form for pragmas, discussion[1] suggests using an
  ##      openArray for that variant.
  ##      [1]: https://github.com/disruptek/cps/pull/144#discussion_r642208263
  n.addPragma:
    nnkExprColonExpr.newTree(prag, pragArg)
proc addPragma*(n: ProcDef, prag: NimNode, pragArg: Name) =
  addPragma(n, prag, pragArg.NimNode)