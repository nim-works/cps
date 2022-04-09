import cps/[normalizedast, rewrites]
import std/macros except newStmtList

template isNotNil*(x: untyped): bool = not(isNil(x))

proc findTree(n: NormNode, traverseKinds: set[NimNodeKind],
              cond: proc(n: NormNode): bool): NormNode =
  ## Find the first node in the AST tree `n` satisfying `cond`.
  ##
  ## :traverseKinds:
  ##   The AST node kinds to traverse.
  if cond(n):
    result = n
  elif n.kind in traverseKinds:
    for child in n.items:
      result = findTree(child, traverseKinds, cond)
      if result.isNotNil:
        break

proc splitStmtList(n, splitNode: NormNode): seq[NormNode] =
  ## Split the StmtList `n` at `splitNode` to up to two splits.
  ##
  ## The same StmtList hierarchy will be shared on both splits.
  if n == splitNode:
    discard "The node to be split upon should not be in result"

  elif n.kind in {nnkStmtList, nnkStmtListExpr}:
    result.add: copyNimNode(n)
    for idx, child in n.pairs:
      template listTail(): seq[NormNode] =
        ## The remaining nodes in this list, excluding the current node
        n[idx + 1 .. ^1]

      let childSplits = splitStmtList(n[idx], splitNode)
      if childSplits.len > 0:
        # Merge the first split
        result[0].add: childSplits[0]

        # The inner StmtList has two splits
        if childSplits.len > 1:
          # Construct the other split
          result.add: copyNimNode(n)
          # Add the inner split
          result[^1].add childSplits[1]
          # Add the remaining nodes of this list
          result[^1].add listTail()
          # Done
          break

      else:
        # There are no splits, thus this is the split node
        #
        # Construct the other split with the remaining nodes in this list
        result.add:
          copyNimNode(n).add:
            listTail()
        # Done
        break

  else:
    # If it's not a StmtList, just return as is
    result.add n

proc rewriteDefer*(n: NormNode): NormNode =
  ## Rewrite the AST of `n` so that all `defer` nodes are
  ## transformed into try-finally
  proc rewriter(n: NormNode): NormNode =
    let deferNode =
      findTree(n, {nnkStmtList, nnkStmtListExpr}) do (n: NormNode) -> bool:
        n.kind == nnkDefer

    if deferNode.isNotNil:
      let
        splits = splitStmtList(n, deferNode)
        # Construct a finally node with lineinfo of the defer node
        finallyNode = newNimNode(nnkFinally, deferNode).add:
          # Use the defer body as the finally body
          deferNode.last

      if splits.len > 0:
        # Add the first split, or "nodes before defer"
        result = splits[0]

        # Construct a try-finally with the remainder and add it to the end
        result.add:
          # Create a new try statement with the lineinfo of the second split
          newNimNode(nnkTryStmt, splits[1]).add(
            # Put the second split, or "nodes after defer", as the try body
            splits[1],
            finallyNode
          )
      else:
        # There are no splits, thus this is a defer without a container
        #
        # Construct a naked try-finally for it.
        result = NormNode:
          newNimNode(nnkTryStmt, deferNode).add(
            # Use an empty statement list for the body
            newNimNode(nnkStmtList, deferNode),
            finallyNode
          )

      # Also rewrite the result to eliminate all defers in it
      result = rewriteDefer(result)

  result = filter(n, rewriter)
