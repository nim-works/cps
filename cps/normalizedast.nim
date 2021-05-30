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

  VarSection* = distinct NormalizedNimNode

proc normalizeProcDef*(n: NimNode): ProcDef =
  expectKind(n, nnkProcDef)
  result = (normalizingRewrites n).ProcDef

# Converters - because plastering `.NimNode` makes everyone sad

template nimNodeConverter(t: typedesc) =
  converter `c t ToNimNode`*(n: `t`): NimNode = n.NimNode

# fn-NormalizedNimNode

nimNodeConverter(NormalizedNimNode)

proc desym*(n: NormalizedNimNode, sym: NimNode): NormalizedNimNode =
  ## desym all occurences of a specific sym
  n.replace(proc(it: NimNode): bool = it == sym, desym sym).NormalizedNimNode

# fn-IdentDefs

proc newIdentDefs*(n: string, info: NimNode, val = newEmptyNode()): IdentDefs =
  newIdentDefs(ident(n), info, val).IdentDefs

func name*(n: IdentDefs): NimNode =
  n.NimNode[0]

func typ*(n: IdentDefs): NimNode =
  n.NimNode[1]

func hasType*(n: IdentDefs): bool =
  n.typ.kind == nnkEmpty

# fn-VarSection

proc newVarSection*(i: IdentDefs): VarSection =
  (nnkVarSection.newTree i.NimNode).VarSection

# fn-ProcDef

nimNodeConverter(ProcDef)

func returnParam*(n: ProcDef): NimNode =
  ## the return param or empty if void
  n.params[0]

func `returnParam=`*(n: ProcDef, ret: NimNode) =
  ## set the return param
  ## XXX: remove normalizingRewrites once this is typed
  n.params[0] = normalizingRewrites ret

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
  n.addPragma:
    nnkExprColonExpr.newTree(prag, pragArg)