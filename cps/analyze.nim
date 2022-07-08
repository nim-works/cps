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

template cpsBlock() {.pragma.}
  ## The annotated block is an unlabeled nim block.
  ##
  ## This block contains one or more splits.

template cpsBlockLabeled(label: typed) {.pragma.}
  ## The annotated block is a labeled nim block.
  ##
  ## This block contains one or more splits.

template cpsJumpAfterBlock() {.pragma.}
  ## Jump to the next split after the parent cps loop or block.
  ##
  ## The annotated block is the original statement.

template cpsJumpAfterBlockLabeled(label: typed) {.pragma.}
  ## Jump to the next split after the cps block with the given label.
  ##
  ## The annotated block is the original statement.

func newCpsSplit(n: NormNode): NormNode =
  ## Create a new cpsSplit annotation.
  newPragmaBlock(bindName"cpsSplit", n)

func isCpsSplit*(n: NormNode): bool =
  ## Return whether `n` is a cpsSplit annotation.
  result = n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsSplit"

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

func newCpsBlock(n: NormNode): NormNode =
  ## Create a new cpsBlock annotation.
  newPragmaBlock(bindName"cpsBlock", n)

func isCpsBlock*(n: NormNode): bool =
  ## Returns whether `n` is a cpsBlock annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsBlock"

func newCpsBlockLabeled(label, n: NormNode): NormNode =
  ## Create a new cpsBlockLabeled annotation.
  newPragmaBlock(
    newPragmaStmt(
      newPragmaColonExpr("cpsBlockLabeled", label)
    ),
    n
  )

func isCpsBlockLabeled*(n: NormNode): bool =
  ## Returns whether `n` is a cpsBlockLabeled annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsBlockLabeled"

func newCpsJumpAfterBlockLabeled(label, n: NormNode): NormNode =
  ## Create a new cpsJumpAfterBlockLabeled annotation.
  newPragmaBlock(
    newPragmaStmt(
      newPragmaColonExpr("cpsJumpAfterBlockLabeled", label)
    ),
    n
  )

func isCpsJumpAfterBlockLabeled*(n: NormNode): bool =
  ## Returns whether `n` is a cpsJumpAfterBlockLabeled annotation.
  n.kind == nnkPragmaBlock and n.asPragmaBlock.hasPragma"cpsJumpAfterBlockLabeled"

func isCpsStatement*(n: NormNode): bool =
  ## Returns whether `n` is a cps statement annotation.
  ##
  ## Statements are usually pragma blocks annotating the originating statement.
  ## Unless the origin is to be inspected, don't recurse into their tree.
  n.isCpsJumpNext or n.isCpsLoopNext or n.isCpsJumpAfterBlock or n.isCpsJumpAfterBlockLabeled

func isCpsScopeExit(n: NormNode): bool =
  ## Return whether the given node signify a CPS scope exit
  n.isCpsJumpNext or n.isCpsJumpNextVia or n.isCpsJumpAfterBlock or n.isCpsJumpAfterBlockLabeled or n.isCpsLoopNext

proc firstReturn(p: NormNode): NormNode =
  ## Find the first control-flow return statement or cps
  ## control-flow within statement lists; else, nil.
  case p.kind
  of nnkReturnStmt, nnkRaiseStmt:
    result = p
  of nnkTryStmt, nnkStmtList, nnkStmtListExpr:
    for child in p.items:
      result = child.firstReturn
      if not result.isNil:
        break
  of nnkBlockStmt, nnkBlockExpr, nnkFinally, nnkPragmaBlock:
    if p.isCpsScopeExit:
      result = p
    elif p.isCpsStatement:
      result = nil
    else:
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
      of nnkBlockStmt, nnkBlockExpr:
        case child[0].kind
        of nnkEmpty:
          result.add:
            newCpsBlock:
              annotate(child)
        else:
          result.add:
            newCpsBlockLabeled(child[0]):
              annotate(child)
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

proc processUnlabeledBreak(n: NormNode): NormNode =
  ## Process unlabeled break nodes in cps blocks with break context
  proc annotator(n: NormNode): NormNode =
    if n.isCpsLoop or n.isCpsBlock or n.isCpsBlockLabeled:
      # Copy the node headers
      result = copyNimNode(n)
      result.add copy(n[0])

      # Rewrite the body children
      #
      # This is done because we have to ignore loop/block nodes to avoid
      # dealing with scopes we weren't supposed to work with.
      let body = copyNimNode(n.asPragmaBlock.body)
      for child in n.asPragmaBlock.body.items:
        body.add child.filter(annotator)

      result.add body

    # Don't touch statements
    elif n.isCpsStatement:
      result = n

    else:
      case n.kind
      of nnkBreakStmt:
        # Annotate unlabeled breaks
        if n[0].kind == nnkEmpty:
          result = newCpsJumpAfterBlock: n

        else:
          result = n

      of nnkBlockExpr, nnkBlockStmt, nnkWhileStmt, nnkForStmt:
        # Don't process trees with new break context but without splits
        result = n

      else:
        result = nil

  proc initiator(n: NormNode): NormNode =
    if n.isCpsLoop or n.isCpsBlock or n.isCpsBlockLabeled:
      n.filter(annotator)

    # Don't touch statements
    elif n.isCpsStatement:
      n

    else:
      nil

  result = n.filter(initiator)

proc processLabeledBreak(n: NormNode): NormNode =
  ## Process labeled break nodes in cps blocks with break context
  proc annotateLabeledBreaks(label, n: NormNode): NormNode =
    n.filter(
      proc (n: NormNode): NormNode =
        if n.isCpsStatement:
          n
        elif n.kind == nnkBreakStmt and n[0] == label:
          newCpsJumpAfterBlockLabeled(n[0]):
            n
        else:
          nil
    )

  proc annotator(n: NormNode): NormNode =
    if n.isCpsBlockLabeled:
      result = copyNimNode(n)
      result.add n.asPragmaBlock.pragma

      let label = n.asPragmaBlock.pragma.findPragma("cpsBlockLabeled")[1]
      result.add:
        processLabeledBreak:
          annotateLabeledBreaks(label):
            n.asPragmaBlock.body

    # Don't touch statements
    elif n.isCpsStatement:
      result = n

    else:
      result = nil

  result = n.filter(annotator)

proc processLoopContinue(n: NormNode): NormNode =
  ## Process continue nodes in cps loops
  proc annotator(n: NormNode): NormNode =
    if n.isCpsLoop:
      # Copy the node headers
      result = copyNimNode(n)
      result.add copy(n[0])

      # Rewrite the body children
      #
      # This is done because we have to ignore loop nodes to avoid
      # dealing with scopes we weren't supposed to work with.
      let body = copyNimNode(n.asPragmaBlock.body)
      for child in n.asPragmaBlock.body.items:
        body.add child.filter(annotator)

      result.add body

    # Don't touch statements
    elif n.isCpsStatement:
      result = n

    else:
      case n.kind
      of nnkContinueStmt:
        # Annotate continue
        result = newCpsLoopNext: n

      of nnkWhileStmt, nnkForStmt:
        # Don't process trees with new continue context but without splits
        result = n

      else:
        result = nil

  proc initiator(n: NormNode): NormNode =
    if n.isCpsLoop:
      n.filter(annotator)

    # Don't touch statements
    elif n.isCpsStatement:
      n

    else:
      nil

  result = n.filter(initiator)

proc processLoopTrailingJump(n: NormNode): NormNode =
  ## Analyze all cpsLoop in n and rewrite trailing jumps so that they loop
  proc findChildRecursiveNoCpsStatement(n: NormNode, cmp: proc(n: NormNode): bool): NormNode =
    ## same as findChildRecursive but cps statements are not searched further.
    if cmp(n):
      result = n
    elif not n.isCpsStatement:
      for child in n.items:
        result = findChildRecursive(NormNode(child), cmp)
        if not result.isNil:
          return

  proc rewriter(n: NormNode): NormNode =
    ## Rewrite all unpaired cpsJumpNext to advance to next loop iteration
    result = copyNimNode(n)

    for idx, child in n.pairs:
      # If there is a cpsJumpNext in this node
      if child.findChildRecursiveNoCpsStatement(isCpsJumpNext) != nil:
        # And its the last node or there are no split in any of its successors
        if (
          idx == n.len - 1 or
          n[idx + 1 .. ^1].allIt(
            it.findChildRecursiveNoCpsStatement(isCpsSplit).isNil
          )
        ):
          # If this node is the jump node, then it's an orphan and should be
          # rewritten into a loop next
          if child.isCpsJumpNext:
            result.add: newCpsLoopNext(child)

          # If this node is not a jump node, then it might contain one without
          # a split so recurse into it.
          else:
            result.add: rewriter(child)

        # There is a pairing split, ignore this node
        else:
          result.add child

      # There are no jumps in this node, ignore
      else:
        result.add child

  proc annotator(n: NormNode): NormNode =
    if n.isCpsLoop:
      # Copy the node headers
      result = copyNimNode(n)
      result.add copy(n[0])
      result.add:
        # Handle inner loops too
        processLoopTrailingJump:
          rewriter(n.asPragmaBlock.body)

    # Don't touch statements
    elif n.isCpsStatement:
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
