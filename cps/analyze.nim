import std/macros except newStmtList

import help
import normalizedast
import returns
import rewrites
import spec

template cpsSplit() {.pragma.}
  ## The annotated block is a continuation unit.

template cpsJumpNext() {.pragma.}
  ## Jump to the next split if available.

template cpsJumpNextVia() {.pragma.}
  ## Jump to the next split after running the annotated magic.

template cpsLoop() {.pragma.}
  ## The annotated block is a loop.
  ##
  ## The loop is transformed into a while-true statement.

template cpsLoopNext() {.pragma.}
  ## Jump to the beginning of the nearest loop.
  ##
  ## The annotated block is the original continue statement.

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

func newCpsJumpNext(): NormNode =
  ## Create a new cpsJumpNext annotation.
  newPragmaStmt(bindName"cpsJumpNext")

func isCpsJumpNext*(n: NormNode): bool =
  ## Returns whether `n` is a cpsJumpNext annotation.
  n.kind == nnkPragma and n.asPragmaStmt.hasPragma"cpsJumpNext"

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
  if n.cond == bindSym"true":
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

func annotate(n: NormNode): NormNode =
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
      case child.kind
      of nnkContinueStmt:
        # Mark all continue statements as a cpsLoopNext.
        #
        # These will be restored if they are found in non-splitting
        # loops.
        result.add:
          newCpsLoopNext:
            annotate(child)

      of nnkWhileStmt, nnkForStmt:
        # This is a regular loop. Restore all cpsLoopNext back to continue
        # statements.
        result.add:
          child.filter(unwrapCpsLoopNextFilter)

      else:
        result.add annotate(child)

macro cpsAnalyze*(n: typed): untyped =
  ## Analyze and annotate the given block
  debugAnnotation cpsAnalyze, n:
    # A StmtList is used to contain the result node, so unwrap it at the end.
    it = annotate(it)[0]
