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

  LetSection* = distinct NormalizedNimNode
  LetDef = distinct NormalizedNimNode # IdentDef or VarTuple
  # LetVal = distinct NormalizedNimNode # val for an IdentDef or VarTuple
  LetIdentDef* = distinct LetSection
  LetTuple* = distinct LetSection

  VarSection* = distinct NormalizedNimNode
  VarDef = distinct NormalizedNimNode # IdentDef or VarTuple
  # VarVal = distinct NormalizedNimNode # val for an IdentDef or VarTuple
  VarIdentDef* = distinct VarSection
  VarTuple* = distinct VarSection

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

func name*(n: IdentDefs): NimNode =
  n[0]

func typ*(n: IdentDefs): NimNode =
  n[1]

func val*(n: IdentDefs): NimNode =
  # XXX: rename to `value` or 
  n[2]

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

# fn-LetDef

proc hasValue(n: LetDef): bool =
  ## has a non-Empty value defined
  n.NimNode[2].kind != nnkEmpty

proc val(n: LetDef): NimNode =
  n.NimNode[2]

# fn-LetSection

proc expectLetSection*(n: NimNode): LetSection =
  ## return an LetSection or error out
  if n.kind != nnkLetSection:
    error "not a let section, got:\n" & repr(n), n
  elif n.len > 1:
    error "bad rewrite, let section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  return n.LetSection

proc newLetSection*(i: IdentDefs): LetSection =
  (nnkLetSection.newTree i).LetSection

proc def(n: LetSection): LetDef =
  ## Internal accessor to get the single IdentDefs or tuple
  n.NimNode[0].LetDef

proc hasValue*(n: LetSection): bool =
  ## has a non-Empty value defined
  n.def.hasValue

proc val*(n: LetSection): NimNode =
  ## the init value of the single identdefs within
  n.def.val

# fn-VarDef

proc hasValue(n: VarDef): bool =
  ## has a non-Empty value defined
  n.NimNode[2].kind != nnkEmpty

proc val(n: VarDef): NimNode =
  n.NimNode[2]

# fn-VarSection

defineToNimNodeConverter(VarSection)

proc expectVarSection*(n: NimNode): VarSection =
  ## return an VarSection or error out
  if n.kind != nnkVarSection:
    error "not a var section, got:\n" & repr(n), n
  elif n.len > 1:
    error "bad rewrite, var section has " & $n.len &
          " defines, requires exactly 1:\n" & repr(n), n
  return n.VarSection

proc newVarSection*(i: IdentDefs): VarSection =
  (nnkVarSection.newTree i).VarSection

proc newVarSection*(n, typ: NimNode, val = newEmptyNode()): VarSection =
  ## create a var section with an identdef, eg: `n`: `typ` = `val`
  newVarSection(newIdentDefs(n, typ, val).IdentDefs)

proc def(n: VarSection): VarDef =
  ## Internal accessor to get the def part (IdentDef or VarTuple)
  n[0].VarDef

proc hasValue*(n: VarSection): bool =
  ## has a non-Empty value defined
  n.def.hasValue

proc val*(n: VarSection): NimNode =
  ## the init value of the single identdefs or tuple within
  n.def.val

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
