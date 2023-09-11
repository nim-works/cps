import std/macros except newStmtList, newTree
import std/[genasts, setutils]

import help
import normalizedast

## Continuation IR for CPS.
##
## This IR is produced by `cpsAnalyze` and provides a high-level description of
## continuation suspend and resume points.

type
  CirNodeKind* {.pure.} = enum
    None ## Not a node CIR knows about

    Suspend ## Suspend execution here and set the continuation resume point to
            ## the nearest resume point, then run the annotated magic call.
    SuspendLoopNext ## Same as Suspend, but the continuation resume point is set
                    ## to the beginning of the loop containing this node.
    SuspendTerminate ## Same as Suspend, but the continuation will be terminated
                     ## before annotated code is run.
    SuspendExitBlock ## Same as Suspend, but the continuation resume point is set
                     ## after the current block.
    SuspendExitBlockWithLabel ## Same as Suspend, but the continuation resume
                              ## point is set after the named block.
                              ##
                              ## Take one parameter which is the name of the
                              ## block to exit out of.

    ResumePoint ## A continuation unit created as a resume point.
    Next ## Jump to the next resume point.
         ## This is a supporting node to connect execution of code
         ## neighbouring a Suspend to the created resume point.

    Loop ## A block containing a suspend that can be re-entered
         ## via a containing NextLoop node.
         ##
         ## An ExitBlock node can be used to exit this block
         ## early.
    NextLoop ## Jump to the beginning of the loop.

    Block ## A block containing a suspend node.
          ## An ExitBlock node can be used to exit this block early.

    BlockWithLabel ## A labeled block containing a suspend node.
                   ##
                   ## Take one parameter which is the label.
                   ##
                   ## Either an ExitBlock or an ExitBlockWithLabel
                   ## node can be used to exit this block early.

    ExitBlock ## Exit the nearest block.
    ExitBlockWithLabel ## Exit the block with the given label.
                       ## Take one parameter which is the label.

    Terminate ## Terminate the continuation.

const
  CirStmtLists* = {Loop, Block, BlockWithLabel, Suspend, SuspendLoopNext, SuspendTerminate, ResumePoint}
    ## CIR node kinds containing regular Nim code within.
  CirStatements* = fullSet(CirNodeKind) - CirStmtLists - {None}
    ## CIR instruction statements.
  CirSuspendNodes* = {Suspend .. SuspendExitBlockWithLabel}

  CirResumePoints* = {ResumePoint, Loop, Block, BlockWithLabel}
    ## CIR nodes that can act like resume points

  CirNodeParamCount = [
    Suspend: 0,
    SuspendLoopNext: 0,
    SuspendTerminate: 0,
    SuspendExitBlock: 0,
    SuspendExitBlockWithLabel: 1,

    ResumePoint: 0,
    Next: 0,

    Loop: 0,
    NextLoop: 0,

    Block: 0,
    BlockWithLabel: 1,

    ExitBlock: 0,
    ExitBlockWithLabel: 1,

    Terminate: 0
  ]

func getPragmaName(kind: CirNodeKind): string =
  "cir" & $kind

func bindPragma(kind: static[CirNodeKind]): Name =
  bindName(kind.getPragmaName)

macro generateCirPragmas(): untyped =
  func generateParams(count: int): seq[NimNode] =
    result.add newEmptyNode()

    if count > 0:
      let identDefs = newNimNode nnkIdentDefs
      for idx in 0 ..< count:
        identDefs.add ident($(ord('a') + idx))
      identDefs.add bindSym"typed"
      identDefs.add newEmptyNode()

      result.add identDefs

  let symArray = newNimNode(nnkBracket)
  result = macros.newStmtList()

  for kind in succ(CirNodeKind.None) .. high(CirNodeKind):
    let name = ident(kind.getPragmaName())
    result.add:
      newProc(
        name,
        procType = nnkTemplateDef,
        params = generateParams(CirNodeParamCount[kind]),
        body = newEmptyNode(),
        pragmas = nnkPragma.newTree(ident"pragma")
      )

    symArray.add:
      nnkExprColonExpr.newTree(
        newLit kind,
        newCall(bindName"asSym", newCall(bindName"bindPragma", newLit(kind)))
      )

  result.add:
    genAstOpt({}, ident = ident"CirPragmaSyms", symArray):
      let ident {.compileTime.} = symArray

generateCirPragmas()

proc findCirPragma(n: NormNode): (CirNodeKind, PragmaAtom) =
  if n.kind == nnkPragmaBlock:
    let n = n.asPragmaBlock
    for kind, sym in CirPragmaSyms.pairs():
      result[1] = n.pragma.findPragma(sym)
      if result[1].NormNode != nil:
        result[0] = kind
        return

proc cirNodeKind*(n: NormNode): CirNodeKind =
  result = n.findCirPragma[0]

proc newCirNode*(k: CirNodeKind, params: varargs[NormNode]): NormNode =
  if k == None:
    raise newException(ValueError, "cannot create a none node")

  var pragmaCall = newCall(copy CirPragmaSyms[k])
  for idx in 0 ..< params.high:
    pragmaCall.add params[idx]
  let body =
    if params.len > 0:
      if k in CirStatements:
        newStmtList(
          doc("Below is the genesis for this node"),
          params[^1]
        )
      else:
        params[^1]
    else:
      newStmtList(
        doc("This node has no genesis"),
        nnkDiscardStmt.newTree(newEmptyNode())
      )

  nnkPragmaBlock.newTree(
    newPragmaStmt(pragmaCall.asPragmaAtom),
    body
  )

proc cloneCirNode*(n, body: NormNode): NormNode =
  ## Clone a CIR node with a replacement body
  case n.cirNodeKind
  of CirStmtLists:
    result = copyNimNode(n)
    result.add copy(n[0])
    result.add body

  of None:
    raise newException(ValueError):
      "Node is not a CIR node"

  else:
    raise newException(ValueError):
      "CIR instruction nodes cannot be cloned"

proc cirBody*(n: NormNode): NormNode =
  case n.cirNodeKind
  of None:
    n
  of CirStmtLists:
    n.asPragmaBlock.body
  else:
    raise newException(ValueError):
      "CIR node " & $n.cirNodeKind & " is not a stmtlist node type"

proc cirInst*(n: NormNode): NormNode =
  case n.cirNodeKind
  of None:
    raise newException(ValueError):
      "Node is not a CIR node"
  else:
    n.asPragmaBlock.body

proc cirParam*(n: NormNode, idx: Natural): NormNode =
  let (kind, atom) = n.findCirPragma()
  if kind == None:
    raise newException(ValueError):
      "Node is not a CIR node"

  atom[idx + 1]
