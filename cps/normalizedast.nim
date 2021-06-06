import std/macros

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

  ProcDef* = distinct NormalizedNimNode
    ## an nnkProcDef node which has been normalized
  
  IdentDefs* = distinct NormalizedNimNode

  VarLet* = distinct NormalizedNimNode
  VarLetDef* = distinct NormalizedNimNode
  VarLetDefLhs* = distinct NormalizedNimNode
  VarLetDefType* = distinct NormalizedNimNode
  VarLetDefRhs* = distinct NormalizedNimNode

  VarLetTuple* = distinct VarLet
  VarLetIdentDef* = distinct VarLet

  LetSection* = distinct VarLet
  LetDef = distinct VarLetDef # IdentDef or VarTuple
  # LetVal = distinct NormalizedNimNode # val for an IdentDef or VarTuple
  LetIdentDef* = distinct LetSection
  LetTuple* = distinct VarLetTuple

  VarSection* = distinct VarLet
  VarIdentDef* = distinct VarSection
  VarTuple* = distinct VarLetTuple

proc normalizeProcDef*(n: NimNode): ProcDef =
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

# Converters - because plastering `.NimNode` makes everyone sad

template defineToNimNodeConverter(t: typedesc) =
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode

# fn-NormalizedNimNode

defineToNimNodeConverter(NormalizedNimNode)

proc desym*(n: NormalizedNimNode, sym: NimNode): NormalizedNimNode =
  ## desym all occurences of a specific sym
  n.replace(proc(it: NimNode): bool = it == sym, desym sym).NormalizedNimNode

# fn-IdentDefs

defineToNimNodeConverter(IdentDefs)

proc expectIdentDefs*(n: NimNode): IdentDefs =
  ## return an IdentDef or error out
  doAssert n.kind == nnkIdentDefs, "not an IdentDefs, got: " & $n.kind
  if n[0].kind notin {nnkIdent, nnkSym}:
    error "bad rewrite presented:\n" & repr(n), n
  elif n.len != 3:
    error "bad rewrite, failed to set init\n" & repr(n), n

  return n.IdentDefs

proc newIdentDefs*(n: string, typ: NimNode, val = newEmptyNode()): IdentDefs =
  newIdentDefs(ident(n), typ, val).IdentDefs

func name*(n: IdentDefs): NimNode = n[0]
func typ*(n: IdentDefs): NimNode = n[1]
func val*(n: IdentDefs): NimNode = n[2]

func hasValue*(n: IdentDefs): bool =
  ## has a non-Empty value defined
  ##
  ## Yes, proc, you ARE a good proc. You have value, hasValue, in fact.
  n.val.kind != nnkEmpty

func hasType*(n: IdentDefs): bool =
  ## has a non-Empty type (`typ`) defined
  n.typ.kind != nnkEmpty

func inferTypFromImpl*(n: IdentDefs): NimNode =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: getTypeImpl(n.val)

# fn-VarLetDef

proc typ*(n: VarLetDef): VarLetDefType = n.NimNode[^2].VarLetDefType
  ## the type of this definition (IdentDef or VarTuple)
proc rhs*(n: VarLetDef): VarLetDefRhs = n.NimNode[^1].VarLetDefRhs
  ## the ident or sym being defined, or tuples being defined
proc hasValue*(n: VarLetDef): bool = n.rhs.NimNode.kind != nnkEmpty
  ## has an initial value (nonEmpty) set for the ident or tuple

# fn-VarLet

proc expectVarLet*(n: NimNode): VarLet =
  ## return a VarLet if this is a var or let section, otherwise error out
  if n.kind notin {nnkLetSection, nnkVarSection}:
    error "not a var or let section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, var or let section has " & $n.len &
          " items, requires exactly 1:\n" & repr(n), n
  return n.VarLet

proc def(n: VarLet): VarLetDef = n.NimNode[0].VarLetDef
  ## an IdentDefs or VarTuple
proc typ*(n: VarLet): VarLetDefType = n.def.typ
  ## the type of this definition (IdentDef or VarTuple)
proc rhs*(n: VarLet): VarLetDefRhs = n.def.rhs
  ## the ident or sym being defined, or tuples being defined
proc hasValue*(n: VarLet): bool = n.def.hasValue
proc isTuple*(n: VarLet): bool = n.def.NimNode.kind == nnkVarTuple
proc asVarLetTuple*(n: VarLet): VarLetTuple =
  ## return a VarLetTuple if the def is a VarTuple, otherwise error out
  if n.def.NimNode.kind notin {nnkVarTuple}:
    error "must be a tuple assignment, got:\n" & repr(n.NimNode), n.NimNode
  return n.VarLetTuple
proc asVarLetIdentDef*(n: VarLet): VarLetIdentDef =
  ## return a VarLetIdentDef if the def is an IdentDef, otherwise error out
  if n.def.NimNode.kind notin {nnkIdentDefs}:
    error "must be an IdentDefs, got:\n" & repr(n.NimNode), n.NimNode
  return n.VarLetIdentDef

# fn-VarLetTuple

proc def(n: VarLetTuple): NimNode {.borrow.}
proc rhs(n: VarLetTuple): NimNode {.borrow.}
proc val*(n: VarLetTuple): NimNode = n.rhs
  ## alias for `rhs`
proc typ*(n: VarLetTuple): NimNode =
  ## return the type based on `getTypeInst`
  getTypeInst n.rhs
iterator indexNamePairs*(n: VarLetTuple): (int, NimNode) =
  ## return the names of fields on the lhs of a var/let tuple assignment
  for index, name in n.def[0 .. ^3].pairs:
    # echo $index, " ", repr(name)
    yield (index, name)

# fn-VarLetIdentDef

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDefs): VarLetIdentDef =
  ## create a new VarLetIdentDef
  doAssert kind in {nnkLetSection, nnkVarSection},
    "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  newTree(kind, i).VarLetIdentDef
proc newVarLetIdentDef*(kind: NimNodeKind,
                        name, typ, val: NimNode): VarLetIdentDef =
  ## create a new VarLetIdentDef
  newVarLetIdentDef(kind, newIdentDefs(name, typ, val).IdentDefs)
proc def(n: VarLetIdentDef): IdentDefs {.borrow.}
proc identdef*(n: VarLetIdentDef): IdentDefs =  n.def
  ## retrieve the innner IdentDef
  # XXX: might want to remove this proc
proc name*(n: VarLetIdentDef): NimNode = n.def.name
  ## Name (ident|sym) of the identifer
func typ(n: VarLetIdentDef): NimNode {.borrow.}
func rhs(n: VarLetIdentDef): NimNode {.borrow.}
func hasType(n: VarLetIdentDef): bool = n.def.typ.kind != nnkEmpty
func inferTypFromImpl*(n: VarLetIdentDef): NimNode =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: getTypeImpl(n.rhs)
proc expectVarLetIdentDef*(n: NimNode): VarLetIdentDef =
  ## return a VarLetIdentDef, otherwise error out
  expectVarLet(n).asVarLetIdentDef

# fn-LetSection

proc expectLetSection*(n: NimNode): LetSection =
  ## return an LetSection or error out
  if n.kind != nnkLetSection:
    error "not a let section, got:\n" & repr(n), n
  elif n.len > 1:
    error "bad rewrite, let section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  return n.LetSection

proc hasValue*(n: LetSection): bool {.borrow.}
proc rhs(n: LetSection): LetDef {.borrow.}
proc val*(n: LetSection): NimNode = n.rhs.NimNode
  ## the init value of the single identdefs within

# fn-VarSection

defineToNimNodeConverter(VarSection)

proc expectVarSection*(n: NimNode): VarSection =
  ## return an VarSection or error out
  if n.kind != nnkVarSection:
    error "not a var section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, var section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  return n.VarSection

proc newVarSection*(i: IdentDefs): VarSection =
  (nnkVarSection.newTree i).VarSection
proc newVarSection*(n, typ: NimNode, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `typ` = `val`
  newVarSection(newIdentDefs(n, typ, val).IdentDefs)

# fn-LetIdentDef

proc expectLetIdentDef*(n: NimNode): LetIdentDef =
  ## return an LetIdentDef or error out
  if n.kind != nnkLetSection:
    error "not a let section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, let section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  elif n[0].kind notin {nnkIdentDefs}:
    error "LetIdentDef requires a single IdentDefs child, got:\n" & repr(n), n
  return n.LetIdentDef

proc newLetIdentDef*(i: IdentDefs): LetIdentDef =
  (nnkLetSection.newTree i).LetIdentDef

proc newLetIdentDef*(n, typ: NimNode, val = newEmptyNode()): LetIdentDef =
  ## create a let section with an identdef, eg: `n`: `typ` = `val`
  newLetIdentDef(newIdentDefs(n, typ, val).IdentDefs)

proc identdef(n: LetIdentDef): IdentDefs =
  ## Internal accessor to get the single IdentDefs
  n.NimNode[0].IdentDefs

proc name*(n: LetIdentDef): NimNode =
  # we only have a single identdefs, and that'll have the name
  n.identdef.name

proc hasValue*(n: LetIdentDef): bool =
  ## has a non-Empty value defined
  n.identdef.hasValue

proc val*(n: LetIdentDef): NimNode =
  ## the init value of the single identdefs within
  n.identdef.val

# fn-VarIdentDef

proc expectVarIdentDef*(n: NimNode): VarIdentDef =
  ## return an VarIdentDef or error out
  if n.kind != nnkVarSection:
    error "not a var section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, var section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  elif n[0].kind notin {nnkIdentDefs}:
    error "VarIdentDef requires a single IdentDefs child, got:\n" & repr(n), n
  return n.VarIdentDef

proc newVarIdentDef*(i: IdentDefs): VarIdentDef =
  (nnkVarSection.newTree i).VarIdentDef

proc newVarIdentDef*(n, typ: NimNode, val = newEmptyNode()): VarIdentDef =
  ## create a var section with an identdef, eg: `n`: `typ` = `val`
  newVarIdentDef(newIdentDefs(n, typ, val).IdentDefs)

proc identdef(n: VarIdentDef): IdentDefs =
  ## Internal accessor to get the single IdentDefs
  n.NimNode[0].IdentDefs

proc name*(n: VarIdentDef): NimNode =
  # we only have a single identdefs, and that'll have the name
  n.identdef.name

proc hasValue*(n: VarIdentDef): bool =
  ## has a non-Empty value defined
  n.identdef.hasValue

proc val*(n: VarIdentDef): NimNode =
  ## the init value of the single identdefs within
  n.identdef.val

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
