import std/macros

func hasDefer*(n: NimNode): bool =
  ## Return whether there is a `defer` within the given node
  ## that might cause it to be rewritten.
  case n.kind
  of nnkDefer:
    true
  of nnkStmtList, nnkStmtListExpr:
    for child in n.items:
      if child.hasDefer:
        return true
    false
  else:
    false

proc rewriteDefer*(n: NimNode): NimNode =
  ## Rewrite the AST of `n` so that all `defer` nodes are
  ## transformed into try-finally

  # TODO: This could be made simpler

  proc splitDefer(n: NimNode): tuple[b, d, a: NimNode] =
    ## Cut the AST into three parts:
    ## - b: all nodes before the defer that could affect `n`
    ## - d: the defer node itself
    ## - a: nodes that follow and are affected by the defer
    ##
    ## If there are no defers in the AST, all nodes are left as-is in
    ## `b`.
    case n.kind
    of nnkDefer:
      # it's just a defer node; return it as such
      result = (nil, n, nil)
    of nnkStmtList, nnkStmtListExpr:
      var d, b, a: NimNode
      # Make a copy of our node to the part before defer
      b = copyNimNode n
      # The rest of the split stays in a new node of the same kind
      a = newNimNode(n.kind, n)

      # Look for the defer in the child nodes
      for idx, child in n.pairs:
        if child.hasDefer:
          var xb, xa: NimNode
          (xb, d, xa) = splitDefer child
          if not xb.isNil:
            b.add xb
          # Add nodes coming after the defer to the list of affected nodes
          if not xa.isNil:
            a.add xa
          if idx < n.len - 1:
            a.add n[idx + 1 .. ^1]
          # We are done here
          break

        # If there's no defer in the child node, add as-is
        b.add child
      result = (b, d, a)
    else:
      # there's no defer, so yield the input as unaffected
      result = (n, nil, nil)

  if n.hasDefer and n.kind != nnkDefer:
    let (before, deferNode, affected) = splitDefer n
    result = before

    # Construct the try-finally statement
    let tryStmt = newNimNode(nnkTryStmt)

    if not affected.isNil:
      # Wrap the affected body with the try statement
      tryStmt.add affected
    else:
      # If this doesn't exist, use an empty StmtList
      tryStmt.add newStmtList()
    # Convert the defer node into a finally node
    tryStmt.add:
      newNimNode(nnkFinally, deferNode).add:
        deferNode[0]

    result.add tryStmt
    # Run the transform on the result to cover any
    # nodes nested within this node
    result = rewriteDefer result

  else:
    # This node doesn't have any `defer` that will cause it to be rewritten
    result = copyNimNode n
    # Process its children instead
    for child in n.items:
      result.add:
        rewriteDefer child
