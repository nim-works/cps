import std/macros except newStmtList, newTree
import std/sequtils

import cir
import help
import normalizedast
import returns
import rewrites
import spec

proc firstReturn(p: NormNode): NormNode =
  ## Find the first control-flow return statement or cps
  ## control-flow within statement lists; else, nil.
  case p.cirNodeKind
  of CirSuspendNodes, ExitBlock, ExitBlockWithLabel, Next, NextLoop, Terminate:
    result = p
  else:
    case p.kind
    of nnkReturnStmt, nnkRaiseStmt:
      result = p
    of nnkTryStmt, nnkStmtList, nnkStmtListExpr:
      for child in p.items:
        result = child.firstReturn
        if not result.isNil:
          break
    of nnkBlockStmt, nnkBlockExpr, nnkFinally, nnkPragmaBlock:
      result = p.last.firstReturn
    else:
      result = nil

proc simplifyWhile(n: WhileStmt): NormNode =
  ## Convert a while statement into a while-true statement.
  if n.cond == bindSym"true" or (n.cond.kind == nnkIntLit and n.cond.intVal != 0):
    result = n
  else:
    result = copyNimNode(n)
    result.add bindSym"true"
    result.add:
      newNimNode(nnkIfStmt, n.NimNode).add(
        newNimNode(nnkElifBranch, n.NimNode).add(n.cond, n.body),
        newNimNode(nnkElse, n.NimNode).add(
          newNimNode(nnkBreakStmt, n.NimNode).add(newEmptyNode())
        )
      )

proc annotate(n: NormNode): NormNode =
  ## Transform and annotate `n`'s children while retaining the original code
  ## semantics.
  result = copyNimNode(n)

  for idx, child in n.pairs:
    template splitStmtTailAndBreak() =
      ## If the parent is a statement list and it's not the end of the list,
      ## process the remaining nodes as an another split.
      ##
      ## The loop will terminate after this.
      # If the parent is a StmtList and the current node is not the last node
      if n.kind in StmtListNodes and idx < n.len - 1:
        # Collect all nodes following the current node into a new list derived
        # from the current one.
        let splittedStmt = copyNimNode(n)
        for idx in idx + 1 ..< n.len:
          splittedStmt.add n[idx]

        # If there are no early exits, add a jump to the next split
        if splittedStmt.firstReturn.isNil:
          splittedStmt.add newCirNode(Next)

        # Annotate then add the split to result
        result.add:
          newCirNode(ResumePoint):
            annotate(splittedStmt)

        break

    # If the child node is the cps call
    if child.isCpsCall:
      # Annotate then split the remainder
      result.add:
        newCirNode(Suspend):
          annotate(child)

      splitStmtTailAndBreak()

    # If the child node has a cps call
    elif child.findChildRecursive(isCpsCall) != nil:
      case child.kind
      of nnkWhileStmt:
        result.add:
          # Label the loop as the jump point
          newCirNode(Loop):
            # Simplify the loop then annotate the contents
            asWhileStmt:
              annotate:
                simplifyWhile(child.asWhileStmt)

      of nnkBlockStmt, nnkBlockExpr:
        case child[0].kind
        of nnkEmpty:
          result.add:
            newCirNode(Block):
              annotate(child)

        else:
          result.add:
            newCirNode(BlockWithLabel, child[0]):
              annotate(child)

      else:
        result.add annotate(child)

      # If the current node is a statement list
      if n.kind in StmtListNodes:
        # Mark that a jump has to be done, then move the tail into another
        # split.
        result.add newCirNode(Next)
        splitStmtTailAndBreak()

    # In case this is a normal statement
    else:
      # Add as-is
      result.add child

proc processUnlabeledBreak(n: NormNode): NormNode =
  ## Process unlabeled break nodes in cps blocks with break context
  proc annotator(n: NormNode): NormNode =
    case n.cirNodeKind
    of Loop, Block, BlockWithLabel:
      # Rewrite the contained loop/block
      #
      # This is done because annotator ignores loop/block by default to avoid
      # dealing with scopes we weren't supposed to work with.
      let body = copyNimNode(n.cirBody)
      for child in n.cirBody.items:
        body.add child.filter(annotator)

      # Clone the node with the rewritten body
      result = cloneCirNode(n):
        body

    # Don't touch statements
    of CirStatements:
      result = n

    else:
      case n.kind
      of nnkBreakStmt:
        # Annotate unlabeled breaks
        if n[0].kind == nnkEmpty:
          result = newCirNode(ExitBlock):
            n

        else:
          result = n

      of nnkBlockExpr, nnkBlockStmt, nnkWhileStmt, nnkForStmt:
        # Don't process trees with new break context but without splits
        result = n

      else:
        result = nil

  proc initiator(n: NormNode): NormNode =
    ## Small helper to make sure that annotator always start at a loop/block
    case n.cirNodeKind:
    of Loop, Block, BlockWithLabel:
      n.filter(annotator)

    # Don't touch statements
    of CirStatements:
      n

    else:
      nil

  result = n.filter(initiator)

proc processLabeledBreak(n: NormNode): NormNode =
  ## Process labeled break nodes in cps blocks with break context
  proc annotateLabeledBreaks(label, n: NormNode): NormNode =
    n.filter proc (n: NormNode): NormNode =
      case n.cirNodeKind
      of CirStatements:
        n
      elif n.kind == nnkBreakStmt and n[0] == label:
        newCirNode(ExitBlockWithLabel, n[0]):
          n
      else:
        nil

  proc annotator(n: NormNode): NormNode =
    case n.cirNodeKind
    of BlockWithLabel:
      let label = n.cirParam(0) # The only parameter of this node is the label
      result = cloneCirNode(n):
        # Annotate any other labeled blocks in this block
        processLabeledBreak:
          # Annotate all breaks with the given label in the block
          annotateLabeledBreaks(label):
            n.cirBody

    # Don't touch statements
    of CirStatements:
      result = n

    else:
      result = nil

  result = n.filter(annotator)

proc processLoopContinue(n: NormNode): NormNode =
  ## Process continue nodes in cps loops
  proc annotator(n: NormNode): NormNode =
    case n.cirNodeKind
    of Loop:
      # Rewrite the body children
      #
      # This is done because we have to ignore loop nodes to avoid
      # dealing with scopes we weren't supposed to work with.
      let body = copyNimNode(n.cirBody)
      for child in n.cirBody.items:
        body.add child.filter(annotator)

      result = cloneCirNode(n):
        body

    # Don't touch statements
    of CirStatements:
      result = n

    else:
      case n.kind
      of nnkContinueStmt:
        # Annotate continue
        result = newCirNode(NextLoop):
          n

      of nnkWhileStmt, nnkForStmt:
        # Don't process trees with new continue context but without splits
        result = n

      else:
        result = nil

  proc initiator(n: NormNode): NormNode =
    ## Small helper to make sure annotator always start with a loop node
    case n.cirNodeKind
    of Loop:
      n.filter(annotator)

    # Don't touch statements
    of CirStatements:
      n

    else:
      nil

  result = n.filter(initiator)

proc processLoopTrailingJump(n: NormNode): NormNode =
  ## Analyze all cpsLoop in n and rewrite trailing jumps so that they loop
  proc cirFindRecursive(n: NormNode, kinds: set[CirNodeKind]): NormNode =
    ## Recursively search for a CIR node in `kinds`
    if n.cirNodeKind in kinds:
      result = n
    elif n.cirNodeKind notin CirStatements:
      for child in n.items:
        result = child.NormNode.cirFindRecursive(kinds)
        if not result.isNil:
          return

  proc rewriter(n: NormNode): NormNode =
    ## Rewrite all unpaired cpsJumpNext to advance to next loop iteration
    result = copyNimNode(n)

    for idx, child in n.pairs:
      # If there is a Next instruction in this node
      if child.cirFindRecursive({Suspend, Next}) != nil:
        # If the parent is a StmtList and there is a split in following nodes
        if n.kind in StmtListNodes and n[idx + 1 .. ^1].anyIt(it.cirFindRecursive(CirResumePoints) != nil):
          # This Next instruction is paired, skip this node
          result.add child

        else:
          # If this node is the jump node, then it's an orphan and should be
          # rewritten into a loop next
          case child.cirNodeKind
          of Next:
            result.add:
              newCirNode(NextLoop, child)

          of Suspend:
            result.add:
              newCirNode(SuspendLoopNext):
                child.cirBody

          # Don't touch other statements
          of CirStatements - {Next, Suspend}:
            result.add: child

          # If this node is not a jump node, then it might contain one without
          # a split so recurse into it.
          else:
            result.add: rewriter(child)

      # There are no jumps in this node, ignore
      else:
        result.add child

  proc annotator(n: NormNode): NormNode =
    case n.cirNodeKind
    of Loop:
      result = cloneCirNode(n):
        # Handle inner loops too
        processLoopTrailingJump:
          # Rewrite the jumps within this node
          rewriter(n.cirBody)

    # Don't touch statements
    of CirStatements:
      result = n

    else:
      result = nil

  result = filter(n, annotator)

macro cpsAnalyze*(n: typed): untyped =
  ## Analyze and annotate the given block
  debugAnnotation cpsAnalyze, n:
    it = annotate(it)
    it = processUnlabeledBreak(it)
    it = processLabeledBreak(it)
    it = processLoopContinue(it)
    it = processLoopTrailingJump(it)

    # A StmtList is used to contain the result node, so unwrap it at the end.
    it = it[0]
