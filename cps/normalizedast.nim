import std/macros
from std/hashes import Hash, hash
from std/sequtils import anyIt, toSeq
from std/typetraits import distinctBase

from cps/rewrites import NormalizedNode, normalizingRewrites, replace,
                         desym, resym

export NormalizedNode

# # Structure of the Module
# * distinct types representing normalized/transformed variants of distinct AST
# * procs use to query, transform, etc on the normalized AST as conveniences
#
# ## Why each part:
# * Normalized types - codify the pre/post conditions of CPS code
# * Normalized procs - ensure invariants by centralizing the AST operations
#
# # Nim (macros) AST vs Normalized AST
# 
# Major Difference:
# 1. Nim's AST focuses on syntax and syntax grammar
# 2. Normalized AST maintains the invariants per the rewrites module
# 3. Normalized AST focuses on semantics and semantics grammar
# 4. Opaque Types & Type Classes are used to work around the lack of Sum Types
#
# ## Syntax vs Semantic Grammar
# More about the types, the AST in Nim as exposed by macros is a reflection of
# a syntax tree and grammar, for untyped ASTs especially, this is entirely
# appropriate. Normalized AST is a more of a semantic grammar and as such
# attempts to provide higher level types over top the typical AST nodes one
# might expect. The little insight to take away is that semantics of a language
# can have a grammar onto itself, which will be richer version of the syntactic
# grammar[1].
#
# ## Maintaining Normalization Invariants
# Various procs used to transform the AST work to maintain the invariants put
# in place by by the `rewrites` module. This is done with a few strategies:
# 1. `asXXX` procs which convert nodes or error out
# 2. dedicated constructors that return specialized types
# 3. transformation procs that limit options by type
#
# ## Semantics Grammar
# An example of syntax based grammar, `nnkIdentDefs` is used inside let and var
# sections, routine (proc, template, ...) parameter definitions, generic param
# definitions. Treating all thsoe the same very quick runs into issues as the
# way in those circumstances need to be handled differ darmatically. Here
# distinct types on the parent of an individual instance as a way of pattern
# matching and then controling dispatch is used to ensure the handling is very
# linear. For parts of the code that share significant commonalities, lowering
# the type to a common type can be used -- an opaque type representing the sum.
#
# # Naming Conventions:
# 
# To be Completed
#
# # Longer Term ToDos:
# * converters are likely a bad thing
# * this should break-up into a a few files
#
# # References:
# [1]: Semantic Grammar Insight from Redex: https://docs.racket-lang.org/redex/

type
  Name* = distinct NormalizedNode
    ## opaque sum: `Ident` | `Sym`
    ## This lets you query and use name like things, which is often required as
    ## transformations need to resym, desym, bind, all over the place

  Ident* = distinct Name
    ## nnkIdent
  Sym* = distinct Name
    ## nnkSym

  TypeExpr* = distinct NormalizedNode
    ## opaque sum: see `TypeExprKinds`
    ## the type part of a let or var definition or routine param

  IdentDef* = distinct NormalizedNode
    ## currently this is nnkIdentDefs mostly in a var let section, in reality
    ## these are also for routine and generic param definitions. A normalized
    ## IdentDef should only have one identifier.

  ProcDef* = distinct NormalizedNode
    ## an nnkProcDef node which has been normalized

  RoutineParam* = distinct IdentDef
    ## each calling params of proc/func/etc definition is an nnkIdentDefs

  # Naming Convention Notes for Var and Let:
  # - first part of name is the Node, while the later parts add/narrow context
  # - `_` denotes if we're talking about it in a prefix or suffix context
  #
  # Precise rules in "precendence" order:
  # - Let_: LetSection, suffix part narrow the type of content
  # - Var_: VarSection, suffix part narrow the type of content
  # - _Section: used because Var and Let alone collide too easily
  # - VarLet_: is either a Var or Let
  # - IdentDef_: an IdentDef, from some context `_`
  # - TupleDef_: an VarTuple, from some context `_`
  # - Def_: Inner part of a Var or Let
  VarLet* = distinct NormalizedNode
    ## opqaue sum: a var or let section, with a single define ident or tuple
  DefVarLet = distinct NormalizedNode
    ## opaque sum: nnkIdentDefs|nnkVarTuple, from a var or let section

  VarLetTuple* = distinct VarLet
    ## opaque sum: a var or let section, but with a tuple defintion within
  VarLetIdentDef* = distinct VarLet
    ## a var or let section, but with a single identdefs, eg: `var a: int = 10`

  LetSection* = distinct VarLet
    ## a let section, with a single identdefs or vartuple
  VarSection* = distinct VarLet
    ## a let or var section, with a single identdefs or vartuple
  
  IdentDefLet* = distinct VarLetIdentDef
    ## identdef defintion from a let section
  IdentDefVar* = distinct VarLetIdentDef
    ## identdef defintion from a var section

  # unlike the other `XyzLike` types, this is sprinkled all over the place
  TypeExprLike* = Name | TypeExpr
    ## Any expression that could go into the type part of a routine parameter,
    ## the identdef of a var or let section, etc.

const TypeExprKinds = {
    nnkIdent, nnkSym,   # the simple atoms
    nnkVarTy, nnkRefTy, # the weirder ones
    nnkTupleConstr      # still have tuples in a few areas
    # nnkEmpty          # excluded as it's only allowed in some cases
  }
  ## list of type NimNodeKind
  ## XXX: this is an incomplete list

func errorGot(msg: string, n: NimNode, got: string = repr(n)) =
  ## useful for error messages
  error msg & ", got:\n" & repr(got), n

proc normalizeProcDef*(n: NimNode): ProcDef =
  ## ensure this is a normalized procd definition
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

# XXX: Temporary procs until we get more strongly typed versions

proc copy*(n: NormalizedNode): NormalizedNode {.borrow.}
proc copyNimNode*(n: NormalizedNode): NormalizedNode {.borrow.}
proc copyNimTree*(n: NormalizedNode): NormalizedNode {.borrow.}

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

# Converters - to reduce the conversion spam

template defineToNimNodeConverter(t: typedesc) =
  ## because plastering `.NimNode` makes everyone sad
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode

template allowAutoDowngradeNormalizedNode(t: typedesc) =
  ## because plastering `.NormalizedNode` makes everyone sad
  converter `c t ToNormalizedNode`*(n: `t`): NormalizedNode = n.NormalizedNode

template allowAutoDowngrade(t: typedesc, r: distinct typedesc) =
  ## defined a converter allowing easy downgrading of types, eg:
  ## * RoutineParam -> IdentDef
  ## XXX: check if downgrades are valid
  converter `c t To r`*(n: `t`): `r` = n.`r`

# fn-NormalizedNode

defineToNimNodeConverter(NormalizedNode)

proc newEmptyNormalizedNode*(): NormalizedNode =
  ## create a new empty node (`nnkEmpty`)
  newEmptyNode().NormalizedNode

proc onlyNormalizedNode*[T: distinct](n: T): NormalizedNode =
  ## used for conversion in an vararg scenarios primarily
  when distinctBase(T) is NimNode:
    n
  else:
    errorGot "invalid type, expected some NormalizedNode", n

type
  NormalizedVarargs = varargs[NormalizedNode, onlyNormalizedNode]

template hash*(n: NormalizedNode): Hash =
  ## hash `NormalizedNode`, necessary for what we do in `environment`
  hash(n.NimNode)

proc desym*(n: NormalizedNode, sym: NimNode): NormalizedNode =
  ## desym all occurences of a specific sym
  n.replace(proc(it: NimNode): bool = it == sym, desym sym).NormalizedNode

func len*(n: NormalizedNode): int {.borrow.}
  ## number of children

func last*(n: NormalizedNode): NormalizedNode {.borrow.}
  ## last child

func kind*(n: NormalizedNode): NimNodeKind {.borrow.}
  ## the kind (`NimNodeKind`) of the underlying `NimNode`

proc add*(f, c: NormalizedNode): NormalizedNode {.discardable.} =
  ## hopefully this fixes ambiguous call issues
  f.NimNode.add(c.NimNode).NormalizedNode
proc add*(f: NormalizedNode, cs: NormalizedVarargs): NormalizedNode {.discardable.} =
  ## hopefully this fixes ambiguous call issues
  for c in cs:
    f.add(c)
  f
proc add*(f: NimNode, c: NormalizedNode): NimNode {.borrow, discardable.}
  ## hopefully this fixes ambiguous call issues

proc newStmtList*(stmts: NormalizedVarargs): NormalizedNode =
  ## create a new normalized statement
  result = macros.newStmtList().NormalizedNode
  for s in stmts:
    result.NimNode.add s.NimNode

# TODO - restrict these to only valid types
proc `[]`*(n: NormalizedNode; i: int): NormalizedNode {.borrow.}
  ## grab the `i`'th child of a normalized node should be normalized itself
proc `[]`*(n: NormalizedNode; i: BackwardsIndex): NormalizedNode {.borrow.}
  ## grab the `i`'th child of a normalized node should be normalized itself
proc `[]`*[T, U](n: NormalizedNode, x: HSlice[T, U]): seq[NormalizedNode] =
  ## grab an inclusive `n` slice of normalized children
  seq[NormalizedNode] n.NimNode[x]
proc `[]=`*(n: NormalizedNode; i: int; child: NormalizedNode) {.borrow.}
  ## set the `i`'th child of a normalized node to a normalized child
proc `[]=`*(n: NormalizedNode; i: BackwardsIndex; child: NormalizedNode) {.borrow.}
  ## set the `i`'th child of a normalized node to a normalized child
converter seqNormalizedToSeqNimNode*(n: seq[NormalizedNode]): seq[NimNode] =
  ## convert a `seq[NormalizedNode]` to `seq[NimNode]` for ease of use
  seq[NimNode] n

# fn-Name

# defineToNimNodeConverter(Name)
template hash*(n: Name): Hash =
  ## hash `Name` nodes, necessary for what we do in `environment`
  hash(n.NimNode)
func isNil*(n: Name): bool {.borrow.}
func `==`*(a, b: Name): bool {.borrow.}
func `==`*(a: NimNode, b: Name): bool {.borrow.}
func `$`*(a: Name): string {.borrow.}
func strVal*(n: Name): string {.borrow.}

func isSymbol*(n: Name): bool =
  n.NimNode.kind == nnkSym

# fn-Name - coerce and validation
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

# fn-Name - construct various Name in various ways (ident/sym)
proc asName*(n: string): Name =
  ## `nnkIdent` as `Name`
  (ident n).Name

proc genSymType*(n: string): Name =
  ## `genSym` an `nskType`
  genSym(nskType, n).Name
proc genSymVar*(n: string = ""): Name =
  ## `genSym` an `nskVar`
  genSym(nskVar, n).Name
proc genSymLet*(n: string): Name =
  ## `genSym` an `nskLet`
  genSym(nskLet, n).Name
proc genSymProc*(n: string): Name =
  ## `genSym` an `nskProc`
  genSym(nskProc, n).Name
proc genSymUnknown*(n: string): Name =
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
proc resym*(fragment: NormalizedNode, sym, replacement: Name): NormalizedNode {.borrow.}
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

allowAutoDowngradeNormalizedNode(Name)

# fn-ExprLike
type
  ExprLike* = Name | NormalizedNode
    ## abstract over any nim value expression

template binaryExprOrStmt(name: untyped, comments: untyped) =
  ## create a binary expression or statement proc, eg: dot, colon, assignment
  proc `name`*(l: ExprLike, r: distinct ExprLike): NormalizedNode =
    comments
    `name`(
      when l isnot NimNode: l.NimNode else: l,
      when r isnot NimNode: r.NimNode else: r
    ).NormalizedNode  

binaryExprOrStmt newDotExpr:
  ## create a new dot expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example  
  
binaryExprOrStmt newColonExpr:
  ## create a new colon expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example
  
binaryExprOrStmt newAssignment:
  ## create a new assignment, meant for executable code

proc newCall*(n: Name, arg: ExprLike): NormalizedNode =
  ## create a new call with a single arg
  newCall(
    when n isnot NimNode: n.NimNode else: n,
    when arg isnot NimNode: arg.NimNode else: arg
  ).NormalizedNode
proc newCall*(n: NormalizedNode, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as name some args
  result = newCall(n.NimNode).NormalizedNode
  for a in args:
    result.add a
proc newCall*(n: Name, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as name some args
  result = newCall(NormalizedNode n, args)
proc newCall*(n: string, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as and ident name, and a single arg
  result = newCall(asName(n), args)

# fn-TypeExpr
proc asTypeExpr*(n: NimNode): TypeExpr =
  if n.kind notin TypeExprKinds:
    # XXX: incomplete list of type kinds
    errorGot "not a type expression", n
  n.TypeExpr
proc asTypeExprAllowEmpty*(n: NimNode): TypeExpr =
  ## coerce to `TypeExpr`, allow `nnkEmpty`, error out otherwise
  if n.kind notin {nnkEmpty} + TypeExprKinds:
    errorGot "not a type expression or empty", n
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

# fn-IdentDefLike
type
  IdentDefLike* = IdentDef | RoutineParam
    ## abstract over a single var or let sections IdentDef, or a routine param
    ## definition

func name*(n: IdentDefLike): Name = n.NimNode[0].Name

# fn-DefLike

type
  DefLike* = IdentDefLike | DefVarLet
    ## abstract over any IdentDef or VarTuple from a VarLet or a RoutineParam

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

# fn-IdentDef

defineToNimNodeConverter(IdentDef)

proc validateIdentDefs(n: NimNode) =
  ## validators only, afterwards it's safe to cast, allows re-use
  if n.kind != nnkIdentDefs:
    errorGot "not an IdentDef", n, $n.kind
  elif n[0].kind notin {nnkIdent, nnkSym}:
    errorGot "bad rewrite presented", n
  elif n.len != 3:
    errorGot "bad rewrite, failed to set init", n
proc asIdentDefs*(n: NimNode): IdentDef =
  ## return an IdentDef or error out
  validateIdentDefs(n)
  return n.IdentDef

proc newIdentDefs*(n: string, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  newIdentDefs(ident(n), t.NimNode, val).IdentDef
proc newIdentDefs*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  newIdentDefs(n.NimNode, t.NimNode, val).IdentDef
proc newIdentDefs*(n: Name, val: NormalizedNode): IdentDef =
  newIdentDefs(n.NimNode, newEmptyNode(), val).IdentDef

#########################################
# Start All the Let and Var Section Stuff
#########################################

type
  LetSectionLike* = LetSection | IdentDefLet
    ## abstract over various forms of let sections, used to define operations
    ## over or allow abstracting over this type class further
  VarSectionLike* = VarSection | IdentDefVar
    ## abstract over various var sections types

# fn-VarLetLike

type
  VarLetLike* = VarLet | VarLetTuple | VarLetIdentDef | LetSectionLike |
               VarSectionLike
    ## abstract over various let or var sections types

func def*(n: VarLetLike): DefVarLet = DefVarLet n.NimNode[0]
  ## an IdentDef or VarTuple
func typ*(n: VarLetLike): NimNode = n.def.typ
  ## the type of this definition (IdentDef or VarTuple)
func val*(n: VarLetLike): NormalizedNode = n.def.val.NormalizedNode
  ## the ident or sym being defined, or tuple being defined
func kind*(n: VarLetLike): NimNodeKind = n.NimNode.kind
func hasValue*(n: VarLetLike): bool = n.def.hasValue
  ## whether an initial value has been specified
func hasType(n: VarLetLike): bool = n.def.typ.kind != nnkEmpty
  ## whether an explicit type is defined
func isTuple*(n: VarLetLike): bool = n.def.NimNode.kind == nnkVarTuple

# fn-VarLetIdentDefLike

type
  VarLetIdentDefLike* = VarLetIdentDef | IdentDefVar
    ## abstract over identdefs from let or var sections types

func identdef*(n: VarLetIdentDefLike): IdentDef = n.NimNode[0].IdentDef
  ## retrieve the innner IdentDef
func name*(n: VarLetIdentDefLike): Name = n.identdef.name
  ## Name (ident|sym) of the identifer, as we only have a single identdefs it
  ## will have the name
func inferTypFromImpl*(n: VarLetIdentDefLike): Name =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ.Name else: getTypeImpl(n.val).Name

# fn-VarLet

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
    when T is VarLetTuple: true
    elif T is VarLetLike:  false
    else:                  {.error.}
  
  if n.kind notin sectionKinds:
    errorGot "not a " & sectionName & " section", n
  elif n.len != 1:
    errorGot sectionName & " has " & $n.len & " defs, requires exactly 1", n
  elif checkIdentDef:
    if n[0].kind != nnkIdentDefs:
      errorGot sectionName & " section must contain an IdentDef", n
    validateIdentDefs(n[0])
  elif checkTuple and n[0].kind != nnkVarTuple:
    errorGot sectionName & " section must be a tuple assignment", n
  return n.T

proc asVarLet*(n: NimNode): VarLet =
  ## return a VarLet if this is a var or let section, otherwise error out
  validateAndCoerce(n)

func errorGot(msg: string, n: VarLetLike, got: string = repr(n)) =
  errorGot(msg, n.NimNode, got)

proc asVarLetTuple*(n: VarLet): VarLetTuple =
  ## return a VarLetTuple if the def is a VarTuple, otherwise error out
  validateAndCoerce(n.NimNode, VarLetTuple)
proc asVarLetIdentDef*(n: VarLet): VarLetIdentDef =
  ## return a VarLetIdentDef if the def is an IdentDef, otherwise error out
  validateAndCoerce(n.NimNode, VarLetIdentDef)

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
  result = asVarLet:
    copyNimNode(n.NimNode).add def

allowAutoDowngradeNormalizedNode(VarLet)

# fn-VarLetTuple

proc typ*(n: VarLetTuple): NimNode =
  ## return the type based on `getTypeInst`
  getTypeInst n.val
iterator indexNamePairs*(n: VarLetTuple): (int, NimNode) =
  ## return the names of fields on the lhs of a var/let tuple assignment
  for index, name in n.def.NimNode[0 .. ^3].pairs:
    yield (index, name)

# fn-VarLetIdentDef

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDef): VarLetIdentDef =
  ## create a new VarLetIdentDef
  doAssert kind in {nnkLetSection, nnkVarSection},
    "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  newTree(kind, i).VarLetIdentDef
proc newVarLetIdentDef*(kind: NimNodeKind,
                        name: Name, typ, val: NimNode): VarLetIdentDef =
  ## create a new VarLetIdentDef
  newVarLetIdentDef(kind, newIdentDefs(name.NimNode, typ, val).IdentDef)

allowAutoDowngradeNormalizedNode(VarLetIdentDef)

# fn-VarSection

defineToNimNodeConverter(VarSection)

proc newVarSection*(i: IdentDef): VarSection =
  (nnkVarSection.newTree i).VarSection
proc newVarSection*(n: Name, t: TypeExprLike, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newVarSection(newIdentDefs(n, t, val))

allowAutoDowngradeNormalizedNode(VarSection)

# fn-IdentDefLet

proc newIdentDefLet*(i: IdentDef): IdentDefLet =
  ## create a var section with an identdef
  (nnkLetSection.newTree i).IdentDefLet
proc newIdentDefLet*(n: Name, val: NormalizedNode): IdentDefLet =
  newIdentDefLet(newIdentDefs(n, val))
proc newIdentDefLet*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefLet =
  ## create a let section with an identdef, eg: `n`: `t` = `val`
  newIdentDefLet(newIdentDefs(n, t, val))

allowAutoDowngrade(IdentDefLet, VarLetIdentDef)
allowAutoDowngradeNormalizedNode(IdentDefLet)

# fn-IdentDefVar

proc newIdentDefVar*(i: IdentDef): IdentDefVar =
  ## create a var section with an identdef
  (nnkVarSection.newTree i).IdentDefVar
proc newIdentDefVar*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefVar =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newIdentDefVar(newIdentDefs(n, t, val))

allowAutoDowngrade(IdentDefVar, VarLetIdentDef)

# fn-ProcDefParams

allowAutoDowngrade(RoutineParam, IdentDef)

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

proc body*(n: ProcDef): NormalizedNode {.borrow.}
proc `body=`*(n: ProcDef, b: NormalizedNode) {.borrow.}

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
  desym(n.NormalizedNode, sym.NimNode).T

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

func returnParam*(n: ProcDef): NormalizedNode =
  ## the return param or empty if void
  ## XXX: should return TypeExpr or Name, need to check rewrite restrictions 
  n.NimNode.params[0].NormalizedNode

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

allowAutoDowngradeNormalizedNode(ProcDef)
