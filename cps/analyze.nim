import std/macros except newStmtList, newTree
import std/sequtils

import help
import normalizedast
import returns
import rewrites
import spec

template cpsSplit() {.pragma.}
  ## The annotated block is a continuation unit.

template cpsJumpNext() {.pragma.}
  ## Jump to the next split if available.
  ##
  ## Annotates the originating node. A discard statement is used when there are
  ## no originating statement.

template cpsJumpNextVia() {.pragma.}
  ## Jump to the next split after running the annotated magic.

template cpsLoop() {.pragma.}
  ## The annotated block is a loop.
  ##
  ## The loop is transformed into a while-true statement.

template cpsLoopNext() {.pragma.}
  ## Jump to the beginning of the nearest loop.
  ##
  ## The annotated block is the original statement.

template cpsJumpAfterBlock() {.pragma.}
  ## Jump to the next split after the parent cps loop or block.
  ##
  ## The annotated block is the original statement.

func newCpsSplit(n: NormNode): NormNode =
  ## Create a new cpsSplit annotation.
  newPragmaBlock(bindName"cpsSplit", n)

func isCpsSplit*(n: NormNode): bool =
  ## Return whether `n` is a cpsSplit annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsSplit"

func newCpsJumpNextVia(call: Call): NormNode =
  ## Create a new cpsJumpNextVia annotation.
  newPragmaBlock(bindName"cpsJumpNextVia", call)

func isCpsJumpNextVia*(n: NormNode): bool =
  ## Return whether `n` is a cpsJumpNextVia annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsJumpNextVia"

func newCpsJumpNext(n: NormNode = nnkDiscardStmt.newTree(newEmptyNode())): NormNode =
  ## Create a new cpsJumpNext annotation.
  newPragmaBlock(bindName"cpsJumpNext", n)

func isCpsJumpNext*(n: NormNode): bool =
  ## Returns whether `n` is a cpsJumpNext annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsJumpNext"

func newCpsLoop(n: WhileStmt): NormNode =
  ## Create a new cpsLoop annotation.
  newPragmaBlock(bindName"cpsLoop", n)

func isCpsLoop*(n: NormNode): bool =
  ## Returns whether `n` is a cpsLoop annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsLoop"

func newCpsLoopNext(n: NormNode): NormNode =
  ## Create a new cpsLoopNext annotation.
  newPragmaBlock(bindName"cpsLoopNext", n)

func isCpsLoopNext*(n: NormNode): bool =
  ## Returns whether `n` is a cpsLoopNext annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsLoopNext"

func newCpsJumpAfterBlock(n: NormNode): NormNode =
  ## Create a new cpsJumpAfterBlock annotation.
  newPragmaBlock(bindName"cpsJumpAfterBlock", n)

func isCpsJumpAfterBlock*(n: NormNode): bool =
  ## Returns whether `n` is a cpsJumpAfterBlock annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsJumpAfterBlock"

func isCpsStatement*(n: NormNode): bool =
  ## Returns whether `n` is a cps statement annotation.
  ##
  ## Statements are usually pragma blocks annotating the originating statement.
  ## Unless the origin is to be inspected, don't recurse into their tree.
  n.isCpsJumpNext or n.isCpsLoopNext or n.isCpsJumpAfterBlock

func unwrapCpsLoopNextFilter(n: NormNode): NormNode =
  ## Restore a cpsLoopNext annotation back into a standalone continue
  ## statement.
  ##
  ## To be used with filter().
  if n.isCpsLoopNext:
    n.asPragmaBlock.body
  else:
    nil

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
        # Collect all nodes following the current node into a new list
        let splittedStmt = copyNimNode(n)
        for idx in idx + 1 ..< n.len:
          splittedStmt.add n[idx]

        # If there are no early exits, add a jump to the next split
        if splittedStmt.firstReturn.isNil:
          splittedStmt.add newCpsJumpNext()

        # Annotate then add the split to result
        result.add:
          newCpsSplit:
            annotate(splittedStmt)

        break

    # If the child node is the cps call
    if child.isCpsCall:
      # Annotate then split the remainder
      result.add:
        newCpsJumpNextVia:
          asCall(annotate(child))

      splitStmtTailAndBreak()

    # If the child node has a cps call
    elif child.findChildRecursive(isCpsCall) != nil:
      case child.kind
      of nnkWhileStmt:
        result.add:
          # Label the loop as the jump point
          newCpsLoop:
            # Simplify the loop then annotate the contents
            asWhileStmt:
              annotate:
                simplifyWhile(child.asWhileStmt)
      else:
        result.add annotate(child)

      # If the current node is a statement list
      if n.kind in StmtListNodes:
        # Mark that a jump has to be done, then move the tail into another
        # split.
        result.add newCpsJumpNext()
        splitStmtTailAndBreak()

    # In case this is a normal statement
    else:
      # Add as-is
      result.add child

proc processCpsLoop(n: NormNode): NormNode =
  ## Analyze all cpsLoop in n and annotate their control flow
  proc annotateLoopBreak(n: NormNode): NormNode =
    ## Annotate break statements in the loop
    result = copyNimNode(n)

    for idx, child in n.pairs:
      case child.kind
      of nnkBreakStmt:
        # Annotate unlabeled break statements
        if child[0].kind == nnkEmpty:
          result.add:
            newCpsJumpAfterBlock:
              child

        else:
          result.add child

      of nnkBlockExpr, nnkBlockStmt, nnkForStmt, nnkWhileStmt:
        # Children of this node are in a different break context, don't touch
        # them.
        result.add child

      elif child.isCpsStatement:
        # Don't recurse into statements.
        result.add child

      else:
        # Annotate inner nodes
        result.add annotateLoopBreak(child)

  proc annotateLoopContinue(n: NormNode): NormNode =
    ## Annotate continue statements in the loop
    result = copyNimNode(n)

    for idx, child in n.pairs:
      case child.kind
      of nnkContinueStmt:
        # Annotate unlabeled break statements
        result.add:
          newCpsLoopNext:
            child

      of nnkWhileStmt, nnkForStmt:
        # Children of this node are in a different continue context, don't
        # touch them.
        result.add child

      elif child.isCpsStatement:
        # Don't recurse into statements.
        result.add child

      else:
        # Annotate inner nodes
        result.add annotateLoopContinue(child)

  proc rewriteLoopTrailingJump(n: NormNode): NormNode =
    ## Rewrite all unpaired cpsJumpNext to advance to next loop iteration
    result = copyNimNode(n)

    for idx, child in n.pairs:
      # If there is a cpsJumpNext in this node
      if child.findChildRecursive(
        proc(n: NormNode): bool = n.isCpsJumpNext or n.isCpsJumpNextVia
      ) != nil:
        # And its the last node or there are no split in any of its successors
        if (
          idx == n.len - 1 or
          n[idx + 1 .. ^1].anyIt(
            it.findChildRecursive(proc(n: NormNode): bool = n.isCpsSplit).isNil
          )
        ):
          # If this node is the jump node, then it's an orphan and should be
          # rewritten into a loop next
          if child.isCpsJumpNext or child.isCpsJumpNextVia:
            result.add: newCpsLoopNext(child)

          # If this node is not a jump node, then it might contain one without
          # a split so recurse into it.
          else:
            result.add: rewriteLoopTrailingJump(child)

        # There is a pairing split, ignore this node
        else:
          result.add child

      # There are no jumps in this node, ignore
      else:
        result.add child

  proc annotator(n: NormNode): NormNode =
    if n.isCpsLoop:
      var whileLoop = n.asPragmaBlock.body
      # Perform control-flow annotation/rewrites
      whileLoop = whileLoop.annotateLoopBreak()
                           .annotateLoopContinue()
                           .rewriteLoopTrailingJump()
      # Process any inner loops
      whileLoop = whileLoop.processCpsLoop()

      # Return the processed loop
      result = newCpsLoop: asWhileStmt(whileLoop)

  result = filter(n, annotator)

macro cpsAnalyze*(n: typed): untyped =
  ## Analyze and annotate the given block
  debugAnnotation cpsAnalyze, n:
    it = annotate(it)
    it = processCpsLoop(it)

    # A StmtList is used to contain the result node, so unwrap it at the end.
    it = it[0]
