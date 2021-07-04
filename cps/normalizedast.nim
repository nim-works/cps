import std/macros
from std/hashes import Hash, hash
from std/sequtils import anyIt, toSeq
from std/typetraits import distinctBase

from cps/rewrites import NormalizedNode, normalizingRewrites, replace,
                         desym, resym, resymCall

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
# a syntax tree and grammar, for untyped ASTs especially this is entirely
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

  DotExpr* = distinct NormalizedNode
    ## nnkDotExpr

  TypeExpr* = distinct NormalizedNode
    ## opaque sum: see `TypeExprKinds`
    ## the type part of a let or var definition, routine param, or type def or
    ## section.
  TypeExprObj* = distinct TypeExpr
    ## `nnkObjTy`
  TypeExprRef* = distinct TypeExpr
    ## `nnkRefTy`

  IdentDef* = distinct NormalizedNode
    ## currently this is nnkIdentDefs mostly in a var let section, in reality
    ## these are also for routine and generic param definitions. A normalized
    ## IdentDef should only have one identifier.

  RoutineDef* = distinct NormalizedNode
    ## any kind under `macros.RoutineNodes` which has been normalized
  ProcDef* = distinct RoutineDef
    ## an nnkProcDef node which has been normalized

  FormalParams* = distinct NormalizedNode
    ## formal params for a routine, includes return param, then calling params
  RoutineParam* = distinct IdentDef
    ## each calling params of proc/func/etc definition is an nnkIdentDefs

  Call* = distinct NormalizedNode
    ## opaque sum: call node of some variety, see `macros.CallNodes`

  Pragma* = distinct NormalizedNode
    ## opaque sum: `PragmaStmt`, `PragmaBlock`, and `PragmaExpr`
  PragmaStmt* = distinct Pragma
    ## an nnkPragma node, contains 0 or more child `PragmaAtom`
  PragmaBlock* = distinct Pragma
    ## an nnkPragmaBlock, like `{.cast(noSideEffect).}: ...`
  PragmaExpr* = distinct Pragma
    ## an nnkPragmaExpr, such as `foo{.praggy, maggy.}`

  PragmaAtom* = distinct NormalizedNode
    ## opaque sum: a single pragma expressions (eg: atom, colon, call, ...)

  TypeSection* = distinct NormalizedNode
    ## `nnkTypeSection`
  TypeDef* = distinct NormalizedNode
    ## `nnkTypeDef`

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

  VarLetTuple* = distinct VarLet
    ## opaque sum: a var or let section, but with a tuple defintion within
  VarLetIdentDef* = distinct VarLet
    ## opaque sum: a var or let section, but with a single identdefs, eg: `var a: int = 10`

  LetSection* = distinct VarLet
    ## a let section, with a single nnkIdentDefs or vartuple
  VarSection* = distinct VarLet
    ## a var section, with a single nnkIdentDefs or vartuple
  
  VarIdentDef* = distinct VarLetIdentDef
    ## a var section with a single nnkIdentDefs, never a vartuple
  LetIdentDef* = distinct VarLetIdentDef
    ## a let section with a single nnkIdentDefs, never a vartuple

  DefVarLet = distinct NormalizedNode
    ## opaque sum: nnkIdentDefs|nnkVarTuple, from a var or let section
  IdentDefVarLet* = distinct DefVarLet
    ## opaque sum: identdef defintion from a var or let section
  TupleDefVarLet* = distinct DefVarLet
    ## opaque sum: tuple defintion from a var or let section
  IdentDefLet* = distinct IdentDefVarLet
    ## opaque sum: identdef defintion from a let section
  IdentDefVar* = distinct IdentDefVarLet
    ## opaque sum: identdef defintion from a var section

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

func errorGot*(msg: string, n: NormalizedNode, got: string = repr(n)) =
  ## useful for error messages
  errorGot(msg, n, got)

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

# Converters - to reduce the conversion spam

template defineToNimNodeConverter(t: typedesc) =
  ## because plastering `.NimNode` makes everyone sad
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode

template allowAutoDowngrade(t: typedesc, r: distinct typedesc) =
  ## defined a converter allowing easy downgrading of types, eg:
  ## * RoutineParam -> IdentDef
  ## XXX: check if downgrades are valid
  converter `c t To r`*(n: `t`): `r` = n.`r`

template allowAutoDowngradeNormalizedNode(t: typedesc) =
  ## because plastering `.NormalizedNode` makes everyone sad
  allowAutoDowngrade(t, NormalizedNode)

# Define the various conversion relations in one place to show an overview
# XXX: use the power of macros/templates to make this map prettier

# all the types that can convert down to `NimNode`
defineToNimNodeConverter(NormalizedNode)
defineToNimNodeConverter(IdentDef)
defineToNimNodeConverter(Ident)
defineToNimNodeConverter(VarSection)
defineToNimNodeConverter(RoutineDef)

# all the types that can convert down to `NormalizedNode`
allowAutoDowngradeNormalizedNode(Name)
allowAutoDowngradeNormalizedNode(TypeExpr)

allowAutoDowngradeNormalizedNode(Call)

allowAutoDowngradeNormalizedNode(PragmaStmt)
allowAutoDowngradeNormalizedNode(PragmaAtom)

allowAutoDowngradeNormalizedNode(IdentDef)

allowAutoDowngradeNormalizedNode(RoutineDef)
allowAutoDowngradeNormalizedNode(ProcDef)

allowAutoDowngradeNormalizedNode(FormalParams)
allowAutoDowngradeNormalizedNode(RoutineParam)

allowAutoDowngradeNormalizedNode(VarSection)
allowAutoDowngradeNormalizedNode(LetSection)
allowAutoDowngradeNormalizedNode(VarLet)
allowAutoDowngradeNormalizedNode(VarLetIdentDef)
allowAutoDowngradeNormalizedNode(VarLetTuple)

allowAutoDowngradeNormalizedNode(DefVarLet)
allowAutoDowngradeNormalizedNode(IdentDefLet)

# types that go from a specific type to a less specific type, "downgrade"
allowAutoDowngrade(IdentDefLet, IdentDef)
allowAutoDowngrade(IdentDefVar, IdentDef)
allowAutoDowngrade(RoutineParam, IdentDef)
allowAutoDowngrade(LetIdentDef, VarLetIdentDef)
allowAutoDowngrade(VarIdentDef, VarLetIdentDef)
allowAutoDowngrade(ProcDef, RoutineDef)
allowAutoDowngrade(TypeExprRef, TypeExpr)

# fn-NormalizedNode

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

proc add*(f: NimNode|NormalizedNode, c: NormalizedNode): NormalizedNode {.discardable.} =
  ## hopefully this fixes ambiguous call issues
  NormalizedNode:
    f.NimNode.add(c.NimNode)
proc add*(f: NormalizedNode, cs: NormalizedVarargs): NormalizedNode {.discardable.} =
  ## hopefully this fixes ambiguous call issues
  for c in cs:
    f.add(c)
  f

proc getImpl*(n: NormalizedNode): NormalizedNode {.borrow.}
  ## the implementaiton of a normalized node should be

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

proc getPragmaName*(n: NimNode): NimNode {.deprecated: "Replace with Pragma version".} =
  ## retrieve the symbol/identifier from the child node of a nnkPragma
  case n.kind
  of nnkCall, nnkExprColonExpr:
    n[0]
  else:
    n

proc copyLineInfo*(arg, info: NormalizedNode) {.borrow.}
proc copyLineInfo*(arg: NormalizedNode, info: NimNode) {.borrow.}

iterator items*(n: NormalizedNode): NormalizedNode =
  ## iterate through the kiddos
  for c in n.NimNode.items:
    yield NormalizedNode c
iterator pairs*(n: NormalizedNode): (int, NormalizedNode) =
  ## iterate through the kiddos, but also get their index
  for i, c in n.NimNode.pairs:
    yield (i, NormalizedNode c)

proc newStmtList*(stmts: NormalizedVarargs): NormalizedNode =
  ## create a new normalized statement
  result = macros.newStmtList().NormalizedNode
  for s in stmts:
    result.NimNode.add s.NimNode

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
  not n.isNil and n.kind == nnkSym

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

proc genField*(ident = ""): Name
  {.deprecated: "pending https://github.com/nim-lang/Nim/issues/17851".} =
  ## generate a unique field to put inside an object definition
  asName:
    desym genSym(nskField, ident)

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

func typeInst*(n: Name): NimNode
  {.deprecated: "A Name can be a bare ident and entirely".} =
  ## gets the type via `getTypeInst`
  getTypeInst n.NimNode

func isExported*(n: Name): bool =
  ## true if this has been exported
  n.NimNode.isExported

func eqIdent*(a: Name|NimNode, b: string): bool =
  ## bridge eqIdent from macros
  macros.eqIdent(a.NimNode, b)
func eqIdent*(a: Name|NimNode, b: distinct Name|NimNode): bool =
  ## bridge eqIdent from macros
  macros.eqIdent(a.NimNode, b.NimNode)
func eqIdent*(a: NormalizedNode, b: Name): bool {.borrow.}

proc asName*(n: TypeExpr): Name =
  ## coerce to a `Name`, or error
  # if n.kind notin {nnkIdent, nnkSym}:
  #   errorGot "not an ident or sym", n
  n.Name

# fn-ExprLike
type
  ExprLike* = Name | NormalizedNode
    ## abstract over any nim value expression

template binaryExprOrStmt(name: untyped, comments: untyped) =
  ## declare a proc for a binary expression or statement, eg: dot, colon, asgn
  proc `name`*(l: ExprLike, r: distinct ExprLike): NormalizedNode =
    comments
    NormalizedNode:
      `name`(l.NimNode, r.NimNode)

binaryExprOrStmt newDotExpr:
  ## create a new dot expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example  
  
binaryExprOrStmt newColonExpr:
  ## create a new colon expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example
  
binaryExprOrStmt newAssignment:
  ## create a new assignment, meant for executable code

proc newCall*(n: NormalizedNode, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as name some args
  result = newCall(n.NimNode).NormalizedNode
  result.add args
proc newCall*(n: Name, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as name some args
  result = newCall(NormalizedNode n, args)
proc newCall*(n: string, args: NormalizedVarargs): NormalizedNode =
  ## create a new call, with `n` as and ident name, and a single arg
  result = newCall(asName(n), args)

# fn-TypeSection

proc asTypeSection*(n: NormalizedNode): TypeSection =
  ## coerce to `TypeSection`, error out otherwise
  if n.kind notin {nnkTypeSection}:
    errorGot "not a type section", n
  n.TypeSection

# fn-TypeDef

proc asTypeDef*(n: NormalizedNode): TypeDef =
  ## coerce to `TypeDef`, error out otherwise
  if n.kind notin {nnkTypeDef}:
    errorGot "not a type def", n
  n.TypeDef

# fn-TypeExpr

proc asTypeExpr*(n: NimNode): TypeExpr =
  ## coerce to `TypeExpr`, disallow `nnkEmpty`, error out otherwise
  if n.kind notin TypeExprKinds:
    errorGot "not a type expression", n
  n.TypeExpr
proc asTypeExprAllowEmpty*(n: NimNode): TypeExpr =
  ## coerce to `TypeExpr`, allow `nnkEmpty`, error out otherwise
  if n.kind notin {nnkEmpty} + TypeExprKinds:
    errorGot "not a type expression or empty", n
  n.TypeExpr

proc `[]`*(n: TypeExpr, i: int): TypeExpr = TypeExpr n.NimNode[i]
  ## allow iteration through a TypeExpr, in case it's a tuple type with kids

# fn-TypeExprObj

proc asTypeExprObj*(n: NormalizedNode): TypeExprObj =
  ## coerce to `TypeExprObj`, error out otherwise
  if n.kind notin {nnkObjectTy}:
    errorGot "not an object type expression", n
  n.TypeExprObj

# fn-TypeExprRef

proc asTypeExprRef*(n: NormalizedNode): TypeExprRef =
  ## coerce to `TypeExprRef`, error out otherwise
  if n.kind notin {nnkRefTy}:
    errorGot "not a ref type expression", n
  n.TypeExprRef

proc newRefType*(n: Name): TypeExprRef =
  ## create a new ref type from `n`
  nnkRefTy.newTree(n.NimNode).TypeExprRef

# fn-PragmaAtom

proc asPragmaAtom*(n: Name): PragmaAtom =
  ## convert a Name to a PragmaAtom
  PragmaAtom n

proc newPragmaColonExpr*(n: static[string], r: NormalizedNode): PragmaAtom =
  ## create a new `PragmaAtom` that's a colon expression
  PragmaAtom newColonExpr(bindName(n), r)

proc getPragmaName*(n: PragmaAtom): Name =
  ## retrieve the symbol/identifier from the child node of a PragmaAtom, or
  ## if it's an atom then return itself.
  Name:
    if n.kind in {nnkCall, nnkExprColonExpr}:
      n[0]
    else:
      n

# fn-IdentDefLike
type
  IdentDefLike* = IdentDef | RoutineParam | IdentDefVarLet | IdentDefLet | IdentDefVar
    ## abstract over a single var or let sections IdentDef, or a routine param
    ## definition

func name*(n: IdentDefLike): Name = n.NimNode[0].Name
  ## retrieve the name of this `IdentDefLike`

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

func inferTypFromImpl*(n: DefLike): TypeExpr =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  TypeExpr:
    if n.hasType: n.typ else: getTypeImpl(n.val)

# fn-IdentDef

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

proc newIdentDef*(n: string, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  newIdentDefs(ident(n), t.NimNode, val).IdentDef
proc newIdentDef*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  newIdentDefs(n.NimNode, t.NimNode, val).IdentDef
proc newIdentDef*(n: Name, val: NormalizedNode): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  newIdentDefs(n.NimNode, newEmptyNode(), val).IdentDef

#----------------------------------------
# Start All the Let and Var Section Stuff
#----------------------------------------

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

func def*(n: VarLetLike): DefVarLet = DefVarLet n[0]
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
  VarLetIdentDefLike* = VarLetIdentDef
    ## abstract over let or var sections with identdefs within

func identdef*(n: VarLetIdentDefLike): IdentDef = n.def.IdentDef
  ## retrieve the innner IdentDef
func name*(n: VarLetIdentDefLike): Name = n.identdef.name
  ## Name (ident|sym) of the identifer, as we only have a single identdefs it
  ## will have the name
func inferTypFromImpl*(n: VarLetIdentDefLike): TypeExpr =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  TypeExpr:
    if n.hasType: n.typ else: getTypeImpl(n.val)

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

# fn-VarLetTuple

proc def*(n: VarLetTuple): TupleDefVarLet = TupleDefVarLet n[0]
  ## the tuple definition (`nnkVarTuple`) from a var or let section
proc typ*(n: VarLetTuple): TypeExpr =
  ## return the type based on `getTypeInst`
  TypeExpr:
    getTypeInst n.val
iterator indexNamePairs*(n: VarLetTuple): (int, Name) =
  ## return the names of fields on the lhs of a var/let tuple assignment
  
  let fields = seq[Name] n.def.NormalizedNode[0 ..^ 3]
    ## get the names of the tuple fields from a TupleDefVarLet

  for index, name in fields.pairs:
    yield (index, name)

# fn-VarLetIdentDef

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDef): VarLetIdentDef =
  ## create a new VarLetIdentDef
  doAssert kind in {nnkLetSection, nnkVarSection},
    "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  newTree(kind, i).VarLetIdentDef
proc newVarLetIdentDef*(kind: NimNodeKind, name: Name,
                        typ: TypeExprLike, val: NimNode): VarLetIdentDef =
  ## create a new VarLetIdentDef
  newVarLetIdentDef(kind, newIdentDef(name, typ, val))

# fn-VarIdentDef

proc newVarIdentDef*(i: IdentDef): VarIdentDef =
  ## create a var section with an identdef
  ## ie: `newVarSection(i)`, where `i` is 'foo: int = 2` -> `var foo: int = 2`
  (nnkVarSection.newTree i).VarIdentDef

# fn-VarSection

proc newVarSection*(i: IdentDef): VarSection =
  ## create a var section with an identdef
  ## ie: `newVarSection(i)`, where `i` is 'foo: int = 2` -> `var foo: int = 2`
  VarSection (newVarIdentDef i)
proc newVarSection*(n: Name, t: TypeExprLike, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newVarSection(newIdentDef(n, t, val))

# fn-LetIdentDef

proc newLetIdentDef*(i: IdentDef): LetIdentDef =
  ## create a let section with an identdef
  ## ie: `newLetSection(i)`, where `i` is 'foo: int = 2` -> `let foo: int = 2`
  (nnkLetSection.newTree i).LetIdentDef
proc newLetIdentDef*(n: Name, val: NormalizedNode): LetIdentDef =
  newLetIdentDef(newIdentDef(n, val))
proc newLetIdentDef*(n: Name, t: TypeExprLike, val = newEmptyNode()): LetIdentDef =
  ## create a let section with an identdef, eg: `n`: `t` = `val`
  newLetIdentDef(newIdentDef(n, t, val))

# fn-IdentDefVar

proc newIdentDefVar*(i: IdentDef): IdentDefVar =
  ## create a var section with an identdef
  ## ie: `newVarSection(i)`, where `i` is 'foo: int = 2` -> `var foo: int = 2`
  (nnkVarSection.newTree i).IdentDefVar
proc newIdentDefVar*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDefVar =
  ## create a var section with an identdef, eg: `n`: `t` = `val`
  newIdentDefVar(newIdentDef(n, t, val))

# fn-PragmaLike

type
  PragmaLike* = PragmaBlock | PragmaExpr | PragmaStmt
    ## abstract over any pragma like thing and provide common operations

proc add(p: PragmaLike, e: PragmaAtom) =
  ## add a pragma atom of some variety to this PragmaLIke
  p.NimNode.add e
  copyLineInfo(p, e) # ensure the lineinfo is correct

iterator items*(n: PragmaLike): PragmaAtom =
  ## fetch the individual atoms
  let ps =
    when n is PragmaBlock:
      n.NimNode[0]
    else:
      n.NimNode

  for p in ps.items:
    yield p.PragmaAtom

func hasPragma*(n: PragmaLike, s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  for p in n.items:
    # just skip ColonExprs, etc.
    result = p.getPragmaName.eqIdent s
    if result:
      break

# fn-PragmaStmt

proc asPragmaStmt*(n: Name|NormalizedNode): PragmaStmt =
  ## validate and coerce into a PragmaStmt
  if n.kind notin {nnkPragma}:
    errorGot "not a pragmaStmt", n
  n.PragmaStmt

proc newPragmaStmt*(es: varargs[PragmaAtom]): PragmaStmt =
  ## create a new PragmaStmt node with `es` pragma exprs, but returns an empty
  ## node if none are provided.
  if es.len == 0:
    result = PragmaStmt newEmptyNormalizedNode()
  else:
    result = nnkPragma.newNimNode().PragmaStmt
  for p in es:
    result.add p

proc newPragmaStmt*(n: Name): PragmaStmt =
  ## create a new PragmaStmt node with `n` as a PragmaAtom
  result = nnkPragma.newNimNode().PragmaStmt
  result.add asPragmaAtom(n)

proc newPragmaStmtWithInfo*(inf: NormalizedNode, es: varargs[PragmaAtom]): PragmaStmt =
  ## create a new PragmaStmt node with `es` pragma exprs, but returns an empty
  ## node if none are provided, and sets the line info
  result = newPragmaStmt(es)
  copyLineInfo(result, inf)

# fn-PragmaBlock

proc asPragmaBlock*(n: NormalizedNode): PragmaBlock =
  ## validate and coerce into a PragmaBlock
  if n.kind notin {nnkPragmaBlock}:
    errorGot "not a pragmaStmt", n
  PragmaBlock n

# fn-PragmaHaver

type
  PragmaHaver* = RoutineDef | ProcDef | Call | TypeExprObj | TypeExprRef |
                 TypeDef
    ## abstract over things that have pragmas to provide a uniform interface

func pragma*(n: PragmaHaver): PragmaLike =
  ## fetch all pragma declared on this routine definition or a callee
  when n is RoutineDef | ProcDef:
    PragmaStmt n.NimNode.pragma
  elif n is Call:
    if n.canGetImpl:
      n.impl.pragma
    else:
      PragmaStmt newEmptyNormalizedNode()
  elif n is TypeExprObj:
    PragmaStmt n.NimNode[0]
  elif n is TypeExprRef | TypeDef:
    PragmaStmt n.NimNode.last
  else:
    {.error: "not all types have been defined".}

func hasPragma*(n: PragmaHaver, s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  n.pragma.hasPragma(s)

# fn-RoutineDefLike

type
  RoutineDefLike* = RoutineDef | ProcDef

proc addPragma*(n: RoutineDefLike, prag: Name) =
  ## add a pragma (`prag`) to the definition: `{.prag.}`
  n.NimNode.addPragma(NimNode prag)

proc addPragma*(n: RoutineDefLike, prag: string) =
  ## add the pragma (`prag`) as an ident to this definition
  n.addPragma asName(prag)

proc addPragma*(n: RoutineDefLike, prag: Name, pragArg: NimNode) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  n.NimNode.addPragma:
    nnkExprColonExpr.newTree(prag.NimNode, pragArg)

proc addPragma*(n: RoutineDefLike, prag: Name, pragArg: Name) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  addPragma(n, prag, pragArg.NimNode)

proc addPragma*(n: RoutineDefLike, prag: Name, pragArgs: openArray[Name]) =
  ## add pragmas of the form `{.raise: [IOError, OSError].}`
  addPragma(n, prag, pragArgs)

# fn-Call

proc asCallKind*(n: NormalizedNode): Call =
  ## validate and coerce into a `Call` if it's an `nnkCallKinds` or error
  if n.kind notin nnkCallKinds:
    errorGot "node is not a call kind", n
  Call n

proc asCall*(n: NormalizedNode): Call =
  ## validate and coerce into a `Call` if it's a CallNode or error
  if n.kind notin CallNodes:
    errorGot "node is not a call node", n
  Call n

func name*(n: Call): Name =
  ## callee name
  Name n[0]

proc `name=`(n: Call, newName: Name) =
  ## set the callee's `n`'s name to `newName
  n[0] = newName

func canGetImpl*(n: Call): bool =
  ## the callee's name is a symbol and so an impl can be retrieves
  n.name.isSymbol

func hasImpl*(n: Call): bool =
  ## the callee's name is a symbol and an impl is present
  n.name.isSymbol and n.name.NimNode.getImpl.kind != nnkNilLit

func impl*(n: Call): RoutineDef =
  ## return the `RoutineDef` associated to this `Call` `n`
  RoutineDef n.name.NimNode.getImpl

proc resymCall*(n: Call; sym, field: NormalizedNode): Call =
  ## this is used to rewrite continuation calls into their results
  Call:
    resymCall(NormalizedNode n, sym, field)
proc desym*(n: Call) =
  ## desyms the callee name
  n.name = desym n.name

# fn-FormalParams

proc newFormalParams*(ret: TypeExpr, ps: varargs[IdentDef]): FormalParams =
  ## create new formal params, with a return of `ret`, and calling params `ps`
  result = FormalParams nnkFormalParams.newTree(NimNode ret)
  for p in ps:
    result.add(NormalizedNode p)

# fn-RoutineDef

func asRoutineDef*(n: NormalizedNode): RoutineDef =
  ## coerce into a `RoutineDef` or error out
  if n.kind notin RoutineNodes:
    errorGot "not a routine (proc, template, macro, etc) definition", n
  n.RoutineDef

func name*(n: RoutineDef): Name =
  ## get the name of this RoutineDef
  Name n[0]
proc `name=`*(n: RoutineDef, name: Name) =
  ## set the name of this RoutineDef
  n.NimNode.name = name.NimNode

proc body*(n: RoutineDef): NormalizedNode =
  n.NimNode.body.NormalizedNode
proc `body=`*(n: RoutineDef, b: NormalizedNode) =
  n.NimNode.body = b

func len*(n: RoutineDef): int {.borrow.}

func formalParams*(n: RoutineDef): FormalParams =
  ## fetch the formal params, first one is the return param
  FormalParams n.NimNode.params

proc `formalParams=`*(n: RoutineDef, f: FormalParams) =
  ## set the formal params for this routine, first one is the return param
  n.NimNode.params = NimNode f

proc firstCallParam*(n: RoutineDef): RoutineParam =
  ## returns the first call param for this proc def, useful due to CPS
  ## proc often just have one continuation parameter that we access often.
  if n.formalParams.len < 2:
    errorGot "proc definition does not take calling params", n.NimNode
  n.formalParams[1].RoutineParam

proc `pragma=`*(n: RoutineDef, p: PragmaStmt) =
  ## set the pragma for this routine, replace the previous one entirely
  n.NimNode.pragma = p.NimNode

# fn-ProcDef

proc newProcDef*(name: Name, formalParams: openArray[NimNode]): ProcDef =
  ## create a new proc def with name and formal params (includes return type)
  ## and an empty body (`nnkStmtList`)
  newProc(name.Nimnode, formalParams, newStmtList()).ProcDef

func asProcDef*(n: NormalizedNode): ProcDef =
  ## coerce into a `ProcDef` or error out
  if n.kind != nnkProcDef:
    errorGot "not a proc definition", n
  n.ProcDef

func name*(n: ProcDef): Name {.borrow.}
  ## get the name of this ProcDef
proc `name=`*(n: ProcDef, name: Name) =
  ## set the name of this ProcDef
  n.RoutineDef.name = name

proc `pragma=`*(n: ProcDef, p: PragmaStmt) =
  ## set the pragma
  n.RoutineDef.pragma = p

func len*(n: ProcDef): int {.borrow.}

proc desym*[T: ProcDef](n: T, sym: Name): T =
  ## desym the routine
  desym(n.NormalizedNode, sym.NimNode).T

func returnParam*(n: ProcDef): TypeExpr =
  ## the return param or empty if void
  n.NimNode.params[0].TypeExpr

proc `returnParam=`*(n: ProcDef, ret: Name) =
  ## set the return param
  n.NimNode.params[0] = ret.NimNode

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
    copy n.NimNode.params,      # parameter normalization will mutate these
    newEmptyNode(),
    newEmptyNode(),
    if body == nil: macros.copy(n.body) else: body
  ).ProcDef
  result.copyLineInfo n

proc hasPragma*(n: NormalizedNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  case n.kind
  of nnkPragma:
    result = asPragmaStmt(n).hasPragma(s)
  of RoutineNodes:
    result = asRoutineDef(n).hasPragma(s)
  of nnkObjectTy:
    result = asTypeExprObj(n).hasPragma(s)
  of nnkRefTy:
    result = asTypeExprRef(n).hasPragma(s)
  of nnkTypeDef:
    result = asTypeDef(n).hasPragma(s)
  of nnkTypeSection:
    result = anyIt(toSeq items(n), hasPragma(it, s))
  else:
    result = false