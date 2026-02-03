import std/macros except newTree
import std/strformat
from std/hashes import Hash, hash
from std/sequtils import anyIt, toSeq
from std/typetraits import distinctBase

when defined(nimPreviewSlimSystem):
  import std/assertions

from cps/rewrites import NormNode, normalizingRewrites, replace,
                         desym, resym, childCallToRecoverResult,
                         NormalCallNodes

const
  NilNormNode* = nil.NormNode
  NilNimNode* = nil.NimNode

export NormNode, NormalCallNodes

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
  Name* = distinct NormNode
    ## opaque sum: `Ident` | `Sym`
    ## This lets you query and use name like things, which is often required as
    ## transformations need to resym, desym, bind, all over the place

  Ident* = distinct Name
    ## nnkIdent
  Sym* = distinct Name
    ## nnkSym

  DotExpr* = distinct NormNode
    ## nnkDotExpr

  TypeExpr* = distinct NormNode
    ## opaque sum: see `TypeExprKinds`
    ## the type part of a let or var definition, routine param, or type def or
    ## section.
  TypeExprObj* = distinct TypeExpr
    ## `nnkObjTy`
  TypeExprRef* = distinct TypeExpr
    ## `nnkRefTy`

  IdentDef* = distinct NormNode
    ## currently this is nnkIdentDefs mostly in a var let section, in reality
    ## these are also for routine and generic param definitions. A normalized
    ## IdentDef should only have one identifier.

  RoutineDef* = distinct NormNode
    ## any kind under `macros.RoutineNodes` which has been normalized
  ProcDef* = distinct RoutineDef
    ## an nnkProcDef node which has been normalized

  FormalParams* = distinct NormNode
    ## formal params for a routine, includes return param, then calling params
  RoutineParam* = distinct IdentDef
    ## each calling params of proc/func/etc definition is an nnkIdentDefs

  Call* = distinct NormNode
    ## opaque sum: call node of some variety, see `macros.CallNodes` and
    ## `macros.nnkCallKinds`
  CallKind* = Call
    ## opaque sum: call node of some variety, see `macros.nnkCallKinds`,
    ## this is an alias as it's not really useful to distinguish the two.

  Conv* = distinct NormNode
    ## an nnkConv node

  Pragma* = distinct NormNode
    ## opaque sum: `PragmaStmt`, `PragmaBlock`, and `PragmaExpr`
  PragmaStmt* = distinct Pragma
    ## an nnkPragma node, contains 0 or more child `PragmaAtom`
  PragmaBlock* = distinct Pragma
    ## an nnkPragmaBlock, like `{.cast(noSideEffect).}: ...`
  PragmaExpr* = distinct Pragma
    ## an nnkPragmaExpr, such as `foo{.praggy, maggy.}`

  PragmaAtom* = distinct NormNode
    ## opaque sum: a single pragma expressions (eg: atom, colon, call, ...)

  TypeSection* = distinct NormNode
    ## `nnkTypeSection`
  TypeDef* = distinct NormNode
    ## `nnkTypeDef`

  Statement* = distinct NormNode
    ## opaque sum: any statement node (var, let, assignment, if, while, etc.)
    ## represents a syntactic statement in the normalized AST
  Expression* = distinct NormNode
    ## opaque sum: any expression node (call, ident, literal, etc.)
    ## represents a syntactic expression in the normalized AST

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

  VarLet* = distinct NormNode
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

  DefVarLet = distinct NormNode
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
    nnkTupleConstr,     # still have tuples in a few areas
    nnkBracketExpr,     # seqs and the like
    nnkProcTy,          # proc type
    nnkTupleTy,         # full-fledged tuple declaration
    # nnkEmpty          # excluded as it's only allowed in some cases
  }
  ## list of type NimNodeKind
  ## XXX: this is an incomplete list

func errorGot(msg: string, n: NimNode, got = treeRepr(n)) =
  ## useful for error messages
  {.cast(noSideEffect).}:
    error msg & ", got:\n" & got, n

func errorGot*(msg: string, n: NormNode, got = treeRepr(n.NimNode)) =
  ## useful for error messages
  errorGot(msg, n.NimNode, got)

proc normalizeProcDef*(n: NimNode): ProcDef =
  ## ensure this is a normalized procd definition
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

proc normalizeCall*(n: NimNode): Call =
  ## ensure this is a normalized procd definition
  expectKind(n, CallNodes)
  result = (normalizingRewrites n).Call

# XXX: Temporary procs until we get more strongly typed versions

proc copy*(n: NormNode): NormNode {.borrow.}
proc copyNimNode*(n: NormNode): NormNode {.borrow.}
proc copyNimTree*(n: NormNode): NormNode {.borrow.}

const
  ConvNodes* = {nnkHiddenStdConv..nnkConv}
    ## Conversion nodes in typed AST

  AccessNodes* = AtomicNodes + {nnkBracketExpr, nnkDotExpr, nnkDerefExpr,
                                nnkHiddenDeref, nnkHiddenAddr}
    ## AST nodes for operations accessing a resource

  ConstructNodes* = {nnkBracket, nnkObjConstr, nnkTupleConstr}
    ## AST nodes for construction operations

  HiddenNodes* = {nnkHiddenCallConv, nnkHiddenStdConv, nnkHiddenSubConv,
                  nnkHiddenAddr, nnkHiddenDeref}
    ## "Hidden" AST nodes

# Converters - to reduce the conversion spam

macro defineToNimNodeConverter(ts: varargs[typed]) =
  ## because plastering `.NimNode` makes everyone sad
  result = newStmtList()
  for t in ts:
    let name = ident("c" & strVal(t) & "ToNimNode")
    result.add:
      quote:
        converter `name`*(n: `t`): NimNode =
          n.NimNode

template allowAutoDowngrade(t: typedesc, r: distinct typedesc) =
  ## defined a converter allowing easy downgrading of types, eg:
  ## * RoutineParam -> IdentDef
  ## XXX: check if downgrades are valid
  converter `c t To r`*(n: `t`): `r` = n.`r`

macro allowAutoDowngradeNormalizedNode(ts: varargs[typed]) =
  ## because plastering `.NormNode` makes everyone sad
  result = newStmtList()
  for t in ts:
    result.add:
      quote:
        allowAutoDowngrade(`t`, NormNode)

# Define the various conversion relations in one place to show an overview

# all the types that can convert down to `NimNode`
defineToNimNodeConverter(
    NormNode, IdentDef, Ident, VarSection, RoutineDef
  )

# all the types that can convert down to `NormNode`
allowAutoDowngradeNormalizedNode(
    Name, TypeExpr, Call, Conv, PragmaStmt, PragmaAtom, IdentDef, RoutineDef,
    ProcDef, FormalParams, RoutineParam, VarSection, LetSection, VarLet,
    VarLetIdentDef, VarLetTuple, DefVarLet, IdentDefLet, Sym
  )

# types that go from a specific type to a less specific type, "downgrade"
allowAutoDowngrade(IdentDefLet,  IdentDef)
allowAutoDowngrade(IdentDefVar,  IdentDef)
allowAutoDowngrade(RoutineParam, IdentDef)
allowAutoDowngrade(LetIdentDef,  VarLetIdentDef)
allowAutoDowngrade(VarIdentDef,  VarLetIdentDef)
allowAutoDowngrade(ProcDef,      RoutineDef)
allowAutoDowngrade(TypeExprRef,  TypeExpr)

template createAsTypeFunc(r: typedesc, kinds: set[NimNodeKind], msg: string) =
  ## creates a `func asX(n: NormNode): X`, or errors out
  func `as r`*(n: NormNode): `r` =
    ## coerces a node `n` or errors out
    if n.kind notin kinds:
      errorGot `msg`, n
    `r` n

template createAsTypeAllowEmptyFunc(r: typedesc, kinds: set[NimNodeKind],
                                    msg: string) =
  ## creates a `func asXAllowEmpty(n: NormNode): X`, the func coerces the
  ## type or returns an error node out
  func `as r AllowEmpty`*(n: NormNode): `r` =
    ## coerces a node `n` or errors out
    if n.kind notin {nnkEmpty} + kinds:
      errorGot `msg`, n
    `r` n

# fn-NormNode

proc newEmptyNormNode*(): NormNode =
  ## create a new empty node (`nnkEmpty`)
  newEmptyNode().NormNode

proc onlyNormalizedNode*[T: distinct](n: T): NormNode =
  ## used for conversion in an vararg scenarios primarily
  when distinctBase(T) is NimNode:
    n
  else:
    errorGot "invalid type, expected some NormNode", n

proc upgradeToNormalizedNode*[T](n: T): NormNode =
  ## used for conversion in varargs, will convert `NimNode` to `NormNode`
  when T is NormNode:
    n
  elif T is NimNode or T is distinct and distinctBase(T) is NimNode:
    NormNode n
  else:
    errorGot "invalid type, expected a NimNode or distinct of one", n

type
  NormalizedVarargs = varargs[NormNode, onlyNormalizedNode]
  AnyNodeVarargs = varargs[NormNode, upgradeToNormalizedNode]
  Desymable = NormNode | ProcDef

proc desym*[T: Desymable](n: T, sym: NimNode|Name): T =
  ## desym all occurences of a specific sym
  n.NimNode.replace(
    proc(it: NimNode): bool = it == sym.NimNode, desym sym.NimNode
  ).T # ensure the return type is the same as what we started with

template hash*(n: NormNode): Hash =
  ## hash `NormNode`, necessary for what we do in `environment`
  hash(n.NimNode)

proc `$`*(n: NormNode): string =
  ## to string
  $(n.NimNode) # it borrows the wrong one, looool

func len*(n: NormNode): int {.borrow.}
  ## number of children

func kind*(n: NormNode): NimNodeKind {.borrow.}
  ## the kind (`NimNodeKind`) of the underlying `NimNode`

proc add*(f: NormNode, c: NormNode): NormNode {.discardable.} =
  ## add a child node, and return the parent for further chaining.
  ## created in order to fix ambiguous call issues
  {.push hint[ConvFromXtoItselfNotNeeded]: off.}
  result = NormNode f.NimNode.add(c.NimNode)
  {.pop.}

proc add*(f: NimNode, c: NormNode): NormNode {.discardable.} =
  ## add a child node, and return the parent for further chaining.
  ## created in order to fix ambiguous call issues ... again.
  f.NormNode.add(c.NormNode)

proc add*(f: Statement, c: Statement): Statement {.discardable.} =
  ## typed variant: add a Statement child to a Statement parent
  f.NormNode.add(c.NormNode).Statement

proc add*(f: Expression, c: Expression): Expression {.discardable.} =
  ## typed variant: add an Expression child to an Expression parent
  f.NormNode.add(c.NormNode).Expression

proc copy*(n: Statement): Statement =
  ## typed variant: copy a Statement node
  n.NormNode.copy().Statement

proc copy*(n: Expression): Expression =
  ## typed variant: copy an Expression node
  n.NormNode.copy().Expression

proc copyNimNode*(n: Statement): Statement =
  ## typed variant: shallow copy a Statement node
  n.NormNode.copyNimNode().Statement

proc copyNimNode*(n: Expression): Expression =
  ## typed variant: shallow copy an Expression node
  n.NormNode.copyNimNode().Expression

proc copyNimTree*(n: Statement): Statement =
  ## typed variant: deep copy a Statement node
  n.NormNode.copyNimTree().Statement

proc copyNimTree*(n: Expression): Expression =
  ## typed variant: deep copy an Expression node
  n.NormNode.copyNimTree().Expression

template findChild*(n: NormNode; cond: untyped): NormNode =
  ## finds the first child node matching the condition or nil
  NormNode macros.findChild(n, cond)

proc findChildRecursive*(n: NormNode, cmp: proc(n: NormNode): bool): NormNode =
  ## finds the first child node where `cmp(node)` returns true, recursively
  ##
  ## returns nil if none found
  if cmp(n):
    result = n
  else:
    for child in n.items:
      result = findChildRecursive(NormNode(child), cmp)
      if not result.isNil:
        return

proc getImpl*(n: NormNode): NormNode {.borrow.}
  ## the implementaiton of a normalized node should be normalized itself

proc getTypeInst*(n: NormNode): TypeExpr {.borrow.}
  ## return the type instance, via `getTypeInst` of a NimNode

type
  RecursiveNode* = NormNode | TypeExpr

proc add*[T: RecursiveNode](f: T, cs: NormalizedVarargs): T {.discardable.} =
  ## add a child node, and return the parent for further chaining.
  ## created in order to fix ambiguous call issues
  T f.add(varargs[NimNode] cs)

# TODO - restrict these to only valid types
proc `[]`*[T: RecursiveNode](n: T; i: int): T =
  ## grab the `i`'th child of a normalized node should be normalized itself
  T n.NimNode[i]
proc `[]`*[T: RecursiveNode](n: T; i: BackwardsIndex): T =
  ## grab the `i`'th child of a normalized node should be normalized itself
  T n.NimNode[i]
proc `[]`*[R: RecursiveNode, T, U](n: R, x: HSlice[T, U]): seq[R] =
  ## grab an inclusive `n` slice of normalized children
  seq[R] n.NimNode[x]
proc `[]=`*(n: NormNode; i: int; child: NormNode) {.borrow.}
  ## set the `i`'th child of a normalized node to a normalized child
proc `[]=`*(n: NormNode; i: BackwardsIndex; child: NormNode) {.borrow.}
  ## set the `i`'th child of a normalized node to a normalized child

func last*[T: RecursiveNode](n: T): T =
  ## last child
  n.NimNode.last.T

proc getPragmaName*(n: NimNode): NimNode {.deprecated: "Replace with Pragma version".} =
  ## retrieve the symbol/identifier from the child node of a nnkPragma
  case n.kind
  of nnkCall, nnkExprColonExpr:
    n[0]
  else:
    n

proc copyLineInfo*(arg, info: NormNode) {.borrow.}
proc copyLineInfo*(arg: NormNode, info: NimNode) {.borrow.}

iterator items*(n: NormNode): NormNode =
  ## iterate through the kiddos
  for c in n.NimNode.items:
    yield NormNode c
iterator pairs*(n: NormNode): (int, NormNode) =
  ## iterate through the kiddos, but also get their index
  for i, c in n.NimNode.pairs:
    yield (i, NormNode c)

proc newStmtList*(stmts: AnyNodeVarargs): NormNode =
  ## create a new normalized statement
  result = NormNode macros.newStmtList(varargs[NimNode] stmts)
  if stmts.len > 0:
    result.copyLineInfo stmts[0]

proc newTree*(kind: NimNodeKind, n: AnyNodeVarargs): NormNode =
  ## creates a new tree (`newTree`) of `kind`, with child `n`
  NormNode macros.newTree(kind, varargs[NimNode] n)

template newNodeAndTransformIt*(n: NimNode, body: untyped): untyped =
  ## creates a node `it`, transformed by the `body`, and provides the result.
  ## Info and kind of node are derived from `n`. This proc is useful for when
  ## type information of `n` needs to be wiped, otherwise use
  ## `copyNodeAndTransformIt`. Assumes `n` and `it` are Normalized.
  block:
    var it {.inject.} = NormNode newNimNode(n.kind, n)
    body
    it

template copyNodeAndTransformIt*(n: NimNode, body: untyped): untyped =
  ## copies a node `n` into `it`, transforms `it` with `body`, and provides
  ## `it` as the expression result. Assumes `n` and `it` are Normalized.
  block:
    var it {.inject.} = NormNode copyNimNode(n)
    body
    it

proc wrap*(kind: NimNodeKind, n: NormNode): NormNode =
  ## wraps a node `n` within a new node of `kind`
  newTree(kind, n)

proc wrap*(kind: NimNodeKind, n: Statement): Statement =
  ## typed variant: wrap a Statement within a new node of `kind`
  wrap(kind, n.NormNode).Statement

proc wrap*(kind: NimNodeKind, n: Expression): Expression =
  ## typed variant: wrap an Expression within a new node of `kind`
  wrap(kind, n.NormNode).Expression

converter seqNormalizedToSeqNimNode*(n: seq[NormNode]): seq[NimNode] =
  ## convert a `seq[NormNode]` to `seq[NimNode]` for ease of use
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

const nameKinds = {nnkIdent, nnkSym}
createAsTypeFunc(Name, nameKinds, "not an ident or sym")
createAsTypeAllowEmptyFunc(Name, nameKinds, "not an ident or sym, or empty")

func asName*(n: NimNode): Name =
  ## coerce to `Name`
  asName(NormNode n)
func asNameAllowEmpty*(n: NimNode): Name =
  ## coerce to `Name`, allow `nnkEmpty`, error out otherwise
  asNameAllowEmpty(n)

template withLineInfoPlease(body: typed): untyped {.dirty.} =
  let sym = body
  if info.isNil:
    when false:
      raise Defect.newException "missing line info"
  else:
    copyLineInfo(sym, info)
  sym

# fn-Name - construct various Name in various ways (ident/sym)

proc asName*(n: string; info: NormNode = NilNormNode): Name =
  ## `nnkIdent` as `Name`
  withLineInfoPlease: (ident n).Name

proc genSymType*(n: string; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskType`
  withLineInfoPlease: genSym(nskType, n).Name
proc genSymVar*(n: string = ""; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskVar`
  withLineInfoPlease: genSym(nskVar, n).Name
proc genSymLet*(n: string = ""; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskLet`
  withLineInfoPlease: genSym(nskLet, n).Name
proc genSymProc*(n: string; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskProc`
  withLineInfoPlease: genSym(nskProc, n).Name
proc genSymField*(n: string; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskField`
  withLineInfoPlease: genSym(nskField, n).Name
proc genSymUnknown*(n: string; info: NormNode = NilNormNode): Name =
  ## `genSym` an `nskUnknown`
  withLineInfoPlease: genSym(nskUnknown, n).Name

proc desym*(n: Name): Name {.borrow.}
  ## ensures that `Name` is an `nnkIdent`
proc resym*(fragment: NormNode, sym, replacement: Name): NormNode {.borrow.}
  ## replace `sym` in the AST `fragment` with the `replacement`

proc genField*(ident = ""): Name
  {.deprecated: "pending https://github.com/nim-lang/Nim/issues/17851".} =
  ## generate a unique field to put inside an object definition
  desym genSym(nskField, ident).Name

proc bindName*(n: static string): Name =
  ## `bindSym` the string as a `Name`
  let r = bindSym(n)
  r.Name
proc bindName*(n: static string, rule: static BindSymRule): Name =
  ## `bindSym` the string as a `Name` and specified bind sym `rule`
  let r = bindSym(n, rule)
  r.Name

func typeInst*(n: Name): TypeExpr
  {.deprecated: "A Name can be a bare ident and entirely untyped".} =
  ## gets the type via `getTypeInst`
  getTypeInst n

func isExported*(n: Name): bool =
  ## true if this has been exported
  n.NimNode.isExported

func eqIdent*(a: Name|NimNode, b: string): bool =
  ## bridge eqIdent from macros
  macros.eqIdent(a.NimNode, b)
func eqIdent*(a: Name|NimNode, b: distinct Name|NimNode): bool =
  ## bridge eqIdent from macros
  macros.eqIdent(a.NimNode, b.NimNode)
func eqIdent*(a: NormNode, b: Name): bool {.borrow.}

proc asName*(n: TypeExpr): Name =
  ## coerce to a `Name`, or error
  n.Name

# fn-Sym

createAsTypeFunc(Sym, {nnkSym}, "not a sym")
allowAutoDowngrade(Sym, Name)

func typeInst*(n: Sym): TypeExpr =
  ## gets the type via `getTypeInst`
  getTypeInst n

# fn-ExprLike
type
  ExprLike* = Name | NormNode
    ## abstract over any nim value expression

template binaryExprOrStmt(name: untyped, comments: untyped) =
  ## declare a proc for a binary expression or statement, eg: dot, colon, asgn
  proc `name`*(l: ExprLike, r: distinct ExprLike): NormNode =
    comments
    NormNode:
      `name`(l.NimNode, r.NimNode)

binaryExprOrStmt newDotExpr:
  ## create a new dot expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example

template dot*(a, b: NormNode): NormNode =
  ## for constructing foo.bar
  let expression = newDotExpr(a, b)
  copyLineInfo(expression, a)
  expression

template dot*(a: NormNode; b: string): NormNode =
  ## for constructing `.`(foo, "bar")
  dot(a, asName(b))

binaryExprOrStmt newColonExpr:
  ## create a new colon expression, meant for executable code. In the future
  ## this is unlikely to work for type expressions for example

binaryExprOrStmt newAssignment:
  ## create a new assignment, meant for executable code

proc newCall*(n: NormNode, args: AnyNodeVarargs): Call =
  ## create a new call, with `n` as name some args
  result = Call macros.newCall(n, varargs[NimNode] args)
proc newCall*(n: Name, args: AnyNodeVarargs): Call =
  ## create a new call, with `n` as name some args
  result = newCall(NormNode n, args)
proc newCall*(n: string, args: AnyNodeVarargs): Call =
  ## create a new call, with `n` as and ident name, and a single arg
  result = newCall(asName(n), args)

# fn-TypeSection
createAsTypeFunc(TypeSection, {nnkTypeSection}, "not a type section")

# fn-TypeDef
createAsTypeFunc(TypeDef, {nnkTypeDef}, "not a type def")

# fn-TypeExpr

createAsTypeFunc(TypeExpr, TypeExprKinds, "not a type expression")
createAsTypeAllowEmptyFunc(TypeExpr, TypeExprKinds,
  "not a type expression or empty")

func isNil*(n: TypeExpr): bool {.borrow.}
  ## true if nil

proc `==`*(a, b: TypeExpr): bool {.borrow.}
  ## compare two `TypeEpxr`s and see if they're equal

proc typeKind*(n: TypeExpr): NimTypeKind {.borrow.}
  ## get the type kind of a type expr

proc sameType*(a, b: TypeExpr): bool {.borrow.}
  ## compare the type associated with the `TypeExpr`s and see if they're equal

# fn-TypeExprObj

createAsTypeFunc(TypeExprObj, {nnkObjectTy}, "not an object type expression")

# fn-TypeExprRef

createAsTypeFunc(TypeExprRef, {nnkRefTy}, "not a ref type expression")

proc newRefType*(n: Name): TypeExprRef =
  ## create a new ref type from `n`
  nnkRefTy.newTree(n.NimNode).TypeExprRef

proc sinkAnnotated*(n: NormNode): TypeExpr =
  ## create a sink annotated type expression from `n`
  nnkCommand.newTree(ident("sink"), n.NimNode).TypeExpr

# fn-PragmaAtom

proc asPragmaAtom*(n: Name): PragmaAtom =
  ## convert a Name to a PragmaAtom
  PragmaAtom n

proc newPragmaColonExpr*(n: static[string], r: NormNode): PragmaAtom =
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

# fn-IdentDefLike
type
  IdentDefLike* = IdentDef | RoutineParam | IdentDefVarLet | IdentDefLet | IdentDefVar
    ## abstract over a single var or let sections IdentDef, or a routine param
    ## definition

func name*(n: PragmaExpr): Name =
  for ps in n.items:
    return getPragmaName ps

func name*(n: IdentDefLike): Name =
  ## retrieve the name of this `IdentDefLike`
  case n[0].kind
  of nnkPragmaExpr:
    name n[0].PragmaExpr
  else:
    n[0].Name

# fn-DefLike

type
  DefLike* = IdentDefLike | DefVarLet | TupleDefVarLet
    ## abstract over any IdentDef or VarTuple from a VarLet or a RoutineParam

func typ*(n: DefLike): TypeExpr =
  ## get the type of this identdef or vartuple
  TypeExpr:
    n.NormNode[^2]
func val*(n: DefLike): NormNode =
  ## get the value of this identdef or vartuple
  n.NormNode[^1]
func hasValue*(n: DefLike): bool =
  ## has a non-Empty initial value defined for the ident, sym or tuple
  ## Yes, proc, you ARE a good proc. You have value, hasValue, in fact.
  n.val.kind != nnkEmpty

func hasType*(n: DefLike): bool =
  ## has a non-Empty type (`typ`) defined
  n.typ.kind != nnkEmpty

func inferTypFromImpl*(n: DefLike): TypeExpr =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: TypeExpr getTypeImpl(n.val)

# fn-IdentDef

proc validateIdentDefs(n: NimNode) =
  ## validators only, afterwards it's safe to cast, allows re-use
  if n.kind != nnkIdentDefs:
    errorGot "not an IdentDef", n, $n.kind
  elif n.len != 3:
    errorGot "bad rewrite, failed to set init", n
  elif n[0].kind notin {nnkIdent, nnkSym, nnkPragmaExpr}:
    errorGot "bad rewrite presented", n

proc asIdentDefs*(n: NimNode): IdentDef =
  ## return an IdentDef or error out
  validateIdentDefs(n)
  return n.IdentDef

proc newIdentDef*(n: Name, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  result = newIdentDefs(n.NimNode, t.NimNode, val).IdentDef
  copyLineInfo(result, n)
proc newIdentDef*(n: string, t: TypeExprLike, val = newEmptyNode()): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  newIdentDef(ident(n).Name, t, val)
proc newIdentDef*(n: Name, val: NormNode): IdentDef =
  ## create a single assignment nnkIdentDefs, we drop the plural to indicate
  ## the singleton nature
  newIdentDef(n, newEmptyNormNode().TypeExpr, val)

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

func def*(n: VarLetLike): DefVarLet|TupleDefVarLet =
  ## an IdentDef or VarTuple as a not very specific type `DefVarLet`
  ## for `VarLetTuple` a more specific type `TupleDefVarLet` is returned
  when n is VarLetTuple:
    # the tuple definition (`nnkVarTuple`) from a var or let section
    TupleDefVarLet n[0]
  else:
    DefVarLet n[0]
func val*(n: VarLetLike): NormNode =
  ## the ident or sym being defined, or tuple being defined
  n.def.val

func typ*(n: VarLetLike): TypeExpr =
  ## the type of this definition (IdentDef or VarTuple)
  when n is VarLetTuple:
    #return the type based on `getTypeInst`
    getTypeInst n.val
  else:
    n.def.typ
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
  ## retrieve the inner IdentDef
func name*(n: VarLetIdentDefLike): Name = n.identdef.name
  ## Name (ident|sym) of the identifer, as we only have a single identdefs it
  ## will have the name
func inferTypFromImpl*(n: VarLetIdentDefLike): TypeExpr =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: TypeExpr getTypeImpl(n.val)

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

func smartSniffer*(n: VarLetLike): TypeExpr =
  ## perform some type-fu for tuple type sniffing
  TypeExpr:
    if n.def.typ.kind in {nnkVarTuple, nnkTupleClassTy}:
      getTypeInst n.val
    else:
      inferTypFromImpl n.def

func clone*(n: VarLet; value: NimNode = NilNimNode): VarLet =
  ## clone a `VarLet` but with `value` changed
  let def = copyNimNode(n.def)
  # re-use the name(s)
  for i in 0 ..< n.def.len - 2:
    def.add:
      copy:
        n.def[i]
  # add our best guess as to the type
  def.add:
    copy:
      smartSniffer n
  # add the value
  def.add:
    copy:
      if value.isNil:
        n.def.val
      else:
        NormNode value

  # copy the varlet and add the new def
  result = asVarLet:
    copyNimNode(n).add def

# fn-VarLetTuple

iterator indexNamePairs*(n: VarLetTuple): (int, Name) =
  ## return the names of fields on the lhs of a var/let tuple assignment

  let fields = seq[Name] n.def.NormNode[0 ..^ 3]
    ## get the names of the tuple fields from a TupleDefVarLet

  for index, name in fields.pairs:
    yield (index, name)

# fn-VarLetIdentDef

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDef): VarLetIdentDef =
  ## create a new VarLetIdentDef
  if kind notin {nnkLetSection, nnkVarSection}:
    error "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  else:
    result = newTree(kind, i).VarLetIdentDef

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
proc newLetIdentDef*(n: Name, val: NormNode): LetIdentDef =
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

# fn-PragmaStmt

createAsTypeFunc(PragmaStmt, {nnkPragma}, "not a pragmaStmt")
proc asPragmaStmt*(n: Name): PragmaStmt =
  ## validate and coerce into a PragmaStmt
  asPragmaStmt(n.NormNode)

proc newPragmaStmt*(es: varargs[PragmaAtom]): PragmaStmt =
  ## create a new PragmaStmt node with `es` pragma exprs, but returns an empty
  ## node if none are provided.
  if es.len == 0:
    result = PragmaStmt newEmptyNormNode()
  else:
    result = nnkPragma.newNimNode().PragmaStmt
  for p in es:
    result.add p

proc newPragmaStmt*(n: Name): PragmaStmt =
  ## create a new PragmaStmt node with `n` as a PragmaAtom
  result = nnkPragma.newNimNode().PragmaStmt
  result.add asPragmaAtom(n)

proc newPragmaStmtWithInfo*(inf: NormNode, es: varargs[PragmaAtom]): PragmaStmt =
  ## create a new PragmaStmt node with `es` pragma exprs, but returns an empty
  ## node if none are provided, and sets the line info
  result = newPragmaStmt(es)
  copyLineInfo(result, inf)

# fn-PragmaBlock

createAsTypeFunc(PragmaBlock, {nnkPragmaBlock}, "not a pragmaBlock")

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
    if n.hasImpl:
      n.impl.pragma
    else:
      PragmaStmt newEmptyNormNode()
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
  let name = prag.asName(n)
  n.addPragma name

proc addPragma*(n: RoutineDefLike, prag: Name, pragArg: NimNode) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  n.NimNode.addPragma:
    nnkExprColonExpr.newTree(prag, pragArg)

proc addPragma*(n: RoutineDefLike, prag: Name, pragArg: Name) =
  ## adds a pragma as follows {.`prag`: `pragArg`.} in a colon expression
  addPragma(n, prag, pragArg.NimNode)

proc addPragma*(n: RoutineDefLike, prag: Name, pragArgs: openArray[Name]) =
  ## add pragmas of the form `{.raise: [IOError, OSError].}`
  addPragma(n, prag, pragArgs)

# fn-Call

createAsTypeFunc(CallKind, nnkCallKinds, "node is not a call kind")
createAsTypeFunc(Call, CallNodes, "node is not a call node")

template ifCallThenIt*(n: NormNode, body: untyped) =
  ## if `n` is a `CallNodes` then run the `body` with `it` as `Call`
  if not n.isNil and n.kind in CallNodes:
    let it {.inject.} = asCall(n)
    body

template ifCallKindThenIt*(n: NormNode, body: untyped) =
  ## if `n` is a `nnCallKinds` then run the `body` with `it` as `Call`
  if not n.isNil and n.kind in nnkCallKinds:
    let it {.inject.} = asCallKind(n)
    body

func name*(n: Call): Name =
  ## callee name
  Name n[0]

proc `name=`*(callee: Call, newName: Name) =
  ## set the `callee`'s name to `newName`
  callee[0] = newName

proc prependArg*(n: Call, arg: NormNode) =
  ## add an argument to the call in the first position of the call
  n.NimNode.insert(1, arg)

func hasImpl*(n: Call): bool =
  ## the callee's name is a symbol and a routine impl is present
  n.name.isSymbol and n.name.NimNode.getImpl.kind in RoutineNodes

func impl*(n: Call): RoutineDef =
  ## return the `RoutineDef` associated to this `Call` `n`
  if n.hasImpl:
    result = RoutineDef n.name.NimNode.getImpl
  else:
    error("call does not have an implementation", n.name.NimNode)

proc childCallToRecoverResult*(n: Call; sym, field: NormNode): Call =
  ## this is used to rewrite continuation calls into their results
  Call:
    rewrites.childCallToRecoverResult(n, sym, field)

proc desym*(n: Call) =
  ## desyms the callee name
  n.name = desym n.name

# fn-Conv

createAsTypeFunc(Conv, {nnkConv}, "node is not a conv node")

proc typ*(n: Conv): TypeExpr =
  ## the type being converted to
  n[0].asTypeExpr

proc expr*(n: Conv): NormNode =
  ## the expression being converted
  n[1]

# fn-FormalParams

proc newFormalParams*(ret: TypeExpr, ps: varargs[IdentDef]): FormalParams =
  ## create new formal params, with a return of `ret`, and calling params `ps`
  result = FormalParams nnkFormalParams.newTree(NimNode ret)
  for p in ps:
    result.add(NormNode p)

# fn-RoutineDef

createAsTypeFunc(RoutineDef, RoutineNodes):
  "not a routine (proc, template, macro, etc) definition"

func name*(n: RoutineDef): Name =
  ## get the name of this RoutineDef
  Name n.NimNode.name
proc `name=`*(n: RoutineDef, name: Name) =
  ## set the name of this RoutineDef
  n.NimNode.name = name.NimNode

proc body*(n: RoutineDef): NormNode =
  n.NimNode.body.NormNode
proc `body=`*(n: RoutineDef, b: NormNode) =
  n.NimNode.body = b

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

proc newProcDef*(name: Name, retType: TypeExpr,
                 callParams: varargs[IdentDef]): ProcDef =
  ## create a new proc def with name, returnt type, and calling params and an
  ## empty body (`nnkStmtList`)
  var formalParams = @[NimNode retType]
  formalParams.add seq[NimNode](@callParams)
  newProc(name.NimNode, formalParams, newStmtList()).ProcDef

createAsTypeFunc(ProcDef, {nnkProcDef}, "node is not a proc definition")

proc `pragma=`*(n: ProcDef, p: PragmaStmt) =
  ## set the pragma
  n.RoutineDef.pragma = p

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

proc clone*(n: ProcDef, body: NimNode|NormNode = NilNimNode): ProcDef =
  ## create a copy of a typed proc which satisfies the compiler
  let body = if body.isNil: NilNimNode else: body.NimNode
  result = nnkProcDef.newTree(
    ident(repr n.name),         # repr to handle gensymbols
    newEmptyNode(),
    newEmptyNode(),
    copy n.NimNode.params,      # parameter normalization will mutate these
    newEmptyNode(),
    newEmptyNode(),
    if body.isNil: macros.copy(n.body) else: body
  ).ProcDef
  result.copyLineInfo n

proc hasPragma*(n: NormNode; s: static[string]): bool =
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

func hasPragma*(n: Statement; s: static[string]): bool =
  ## Typed variant: check if a Statement has pragma `s`
  n.NormNode.hasPragma(s)

func hasPragma*(n: TypeExpr; s: static[string]): bool =
  ## Typed variant: check if a TypeExpr has pragma `s`
  n.NormNode.hasPragma(s)

proc genProcName*(a: string; info = NilNormNode): Name {.deprecated.} =
  genSymProc(fmt"cps:{a}", info=info)

proc genProcName*(a, b: string; info = NilNormNode): Name =
  genSymProc(fmt"cps:{a} {b}", info=info)

proc genTypeName*(a, b: string; info = NilNormNode): Name =
  genSymType(fmt"cps:{a} {b}", info=info)

proc postfix*(n: Name; op: string): Name =
  postfix(n.NimNode, op).Name
