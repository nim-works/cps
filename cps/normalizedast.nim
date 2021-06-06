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
  VarLetDef = distinct NormalizedNimNode

  TupleVarLet* = distinct VarLet
  IdentDefVarLet* = distinct VarLet

  LetSection* = distinct VarLet
  IdentDefLet* = distinct LetSection

  VarSection* = distinct VarLet
  IdentDefVar* = distinct IdentDefVarLet

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
  if n.kind != nnkIdentDefs:
    error "not an IdentDefs, got: " & $n.kind, n
  elif n[0].kind notin {nnkIdent, nnkSym}:
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

proc typ(n: VarLetDef): NimNode = n.NimNode[^2]
  ## the type of this definition (IdentDef or VarTuple)
proc val(n: VarLetDef): NimNode = n.NimNode[^1]
  ## the initial value of the ident, sym, or var tuple being defined
proc hasValue(n: VarLetDef): bool = n.val.kind != nnkEmpty
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
proc typ*(n: VarLet): NimNode = n.def.typ
  ## the type of this definition (IdentDef or VarTuple)
proc val*(n: VarLet): NimNode = n.def.val
  ## the ident or sym being defined, or tuple being defined
proc hasValue*(n: VarLet): bool = n.def.hasValue
  ## whether an initial value has been specified
proc isTuple*(n: VarLet): bool = n.def.NimNode.kind == nnkVarTuple

proc asVarLetTuple*(n: VarLet): TupleVarLet =
  ## return a TupleVarLet if the def is a VarTuple, otherwise error out
  if n.def.NimNode.kind notin {nnkVarTuple}:
    error "must be a tuple assignment, got:\n" & repr(n.NimNode), n.NimNode
  return n.TupleVarLet
proc asVarLetIdentDef*(n: VarLet): IdentDefVarLet =
  ## return a IdentDefVarLet if the def is an IdentDef, otherwise error out
  if n.def.NimNode.kind notin {nnkIdentDefs}:
    error "must be an IdentDefs, got:\n" & repr(n.NimNode), n.NimNode
  return n.IdentDefVarLet

# fn-TupleVarLet

proc def(n: TupleVarLet): NimNode {.borrow.}
proc val*(n: TupleVarLet): NimNode {.borrow.}
proc typ*(n: TupleVarLet): NimNode =
  ## return the type based on `getTypeInst`
  getTypeInst n.val
iterator indexNamePairs*(n: TupleVarLet): (int, NimNode) =
  ## return the names of fields on the lhs of a var/let tuple assignment
  for index, name in n.def[0 .. ^3].pairs:
    yield (index, name)

# fn-IdentDefVarLet

proc newVarLetIdentDef*(kind: NimNodeKind, i: IdentDefs): IdentDefVarLet =
  ## create a new IdentDefVarLet
  doAssert kind in {nnkLetSection, nnkVarSection},
    "kind must be nnkLetSection nnkVarSection, got: " & repr(kind)
  newTree(kind, i).IdentDefVarLet
proc newVarLetIdentDef*(kind: NimNodeKind,
                        name, typ, val: NimNode): IdentDefVarLet =
  ## create a new IdentDefVarLet
  newVarLetIdentDef(kind, newIdentDefs(name, typ, val).IdentDefs)
proc def(n: IdentDefVarLet): IdentDefs {.borrow.}
proc identdef*(n: IdentDefVarLet): IdentDefs =  n.def
  ## retrieve the innner IdentDef
  # XXX: might want to remove this proc
proc name*(n: IdentDefVarLet): NimNode = n.def.name
  ## Name (ident|sym) of the identifer
func typ(n: IdentDefVarLet): NimNode {.borrow.}
func val(n: IdentDefVarLet): NimNode {.borrow.}
func hasValue(n: IdentDefVarLet): bool {.borrow.}
func hasType(n: IdentDefVarLet): bool = n.def.typ.kind != nnkEmpty
func inferTypFromImpl*(n: IdentDefVarLet): NimNode =
  ## returns the typ if specified or uses `macro.getTypeImpl` to infer it
  if n.hasType: n.typ else: getTypeImpl(n.val)
proc expectVarLetIdentDef*(n: NimNode): IdentDefVarLet =
  ## return a IdentDefVarLet, otherwise error out
  expectVarLet(n).asVarLetIdentDef

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

# fn-IdentDefLet

proc expectIdentDefLet*(n: NimNode): IdentDefLet =
  ## return an IdentDefLet or error out
  if n.kind != nnkLetSection:
    error "not a let section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, let section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  elif n[0].kind notin {nnkIdentDefs}:
    error "IdentDefLet requires a single IdentDefs child, got:\n" & repr(n), n
  return n.IdentDefLet

proc newIdentDefLet*(i: IdentDefs): IdentDefLet =
  (nnkLetSection.newTree i).IdentDefLet

proc newIdentDefLet*(n, typ: NimNode, val = newEmptyNode()): IdentDefLet =
  ## create a let section with an identdef, eg: `n`: `typ` = `val`
  newIdentDefLet(newIdentDefs(n, typ, val).IdentDefs)

proc identdef(n: IdentDefLet): IdentDefs =
  ## Internal accessor to get the single IdentDefs
  n.NimNode[0].IdentDefs

proc name*(n: IdentDefLet): NimNode =
  # we only have a single identdefs, and that'll have the name
  n.identdef.name

proc hasValue*(n: IdentDefLet): bool =
  ## has a non-Empty value defined
  n.identdef.hasValue

proc val*(n: IdentDefLet): NimNode =
  ## the init value of the single identdefs within
  n.identdef.val

# fn-IdentDefVar

proc expectIdentDefVar*(n: NimNode): IdentDefVar =
  ## return an IdentDefVar or error out
  if n.kind != nnkVarSection:
    error "not a var section, got:\n" & repr(n), n
  elif n.len != 1:
    error "bad rewrite, var section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  elif n[0].kind notin {nnkIdentDefs}:
    error "IdentDefVar requires a single IdentDefs child, got:\n" & repr(n), n
  return n.IdentDefVar

proc newIdentDefVar*(i: IdentDefs): IdentDefVar =
  ## create a var section with an identdef
  (nnkVarSection.newTree i).IdentDefVar
proc newIdentDefVar*(n, typ: NimNode, val = newEmptyNode()): IdentDefVar =
  ## create a var section with an identdef, eg: `n`: `typ` = `val`
  newIdentDefVar(newIdentDefs(n, typ, val).IdentDefs)

proc identdef(n: IdentDefVar): IdentDefs = n.NimNode[0].IdentDefs
  ## Internal accessor to get the single IdentDefs
proc name*(n: IdentDefVar): NimNode = n.identdef.name
  # we only have a single identdefs, and that'll have the name
proc hasValue*(n: IdentDefVar): bool {.borrow.}
proc val*(n: IdentDefVar): NimNode = n.identdef.val
  ## the init value of the single identdefs within

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
