import std/macros
import cps/[spec, normalizedast, help, rewrites]

template cpsMustLift() {.pragma.} ## signify a code block that has to be lifted

proc newCpsMustLift(n: NormalizedNimNode): NormalizedNimNode =
  ## Wrap `n` in a `cpsMustLift` block.
  NormalizedNimNode:
    nnkPragmaBlock.newTree(
      nnkPragma.newTree(bindSym"cpsMustLift"),
      n
    )

proc isCpsMustLift(n: NimNode): bool =
  ## Check whether `n` is a cpsMustLift block
  n.kind == nnkPragmaBlock and n[0].len == 1 and n[0].hasPragma("cpsMustLift")

func hasCpsExpr(n: NormalizedNimNode): bool =
  ## Returns whether `n` has a cps block acting as an expression within it
  ## (ie. the block has a type) and that these expression might need to be
  ## moved outside of `n` for the purpose of transformation.
  # If this node doesn't have a type
  if n.typeKind == ntyNone:
    # Check if its children have any
    case n.kind
    of nnkVarSection, nnkLetSection:
      let n = expectVarLet n
      result = n.val.NormalizedNimNode.hasCpsExpr
    of nnkStmtList, nnkStmtListExpr:
      for child in n.items:
        if child.NormalizedNimNode.hasCpsExpr:
          return
    else:
      result = false
  else:
    # Otherwise check if its a cps block
    result = n.isCpsBlock

func assignTo*(sym: NimNode, n: NormalizedNimNode): NormalizedNimNode =
  ## Rewrite the expression `n` into a statement assigning to symbol `sym`.
  ##
  ## Returns a copy of `n` if `n` is not an expression.
  if n.typeKind == ntyNone:
    return NormalizedNimNode:
      copy n

  case n.kind
  of AtomicNodes, CallNodes, nnkTupleConstr, nnkObjConstr, nnkConv:
    # For calls, conversions, constructions, constants and basic symbols, we
    # just emit the assignment.
    result = NormalizedNimNode newAssignment(sym, copy n)
  of nnkStmtList, nnkStmtListExpr:
    result = NormalizedNimNode copyNimNode n

    # In a statement list, the last node is the expression, so we copy
    # the part before it because we won't touch them.
    for idx in 0 ..< n.len - 1:
      result.add copy(n[idx])

    # Rewrite the last expression to assign to sym.
    # XXX: discard added because `add()` returns a NimNode.
    discard result.add:
      # Convert back to NimNode explicitly because the compiler
      # can't handle our awesome nodes (our node binds to both
      # varargs and normal form of add).
      {.warning: "Compiler workaround here".}
      NimNode:
        assignTo(sym):
          n.last.NormalizedNimNode
  else:
    result = NormalizedNimNode:
      n.errorAst "cps doesn't know how to rewrite this into assignment"

macro cpsExprToTmp(T, n: typed): untyped =
  ## Create a temporary variable with type `T` and rewrite `n` so that the
  ## result is assigned to the temporary, then emit the temporary as the
  ## expression.
  ##
  ## Puts the rewritten `n` into cpsMustLift so that it will be moved outside.
  debugAnnotation cpsExprToTmp, n:
    let
      # The symbol for our temporary
      tmp = genSym(nskVar)

      # The rewritten expression
      body = assignTo(tmp):
        # debugAnnotation puts the expr inside a StmtList, so we have to take
        # it out
        NormalizedNimNode it[0]

    it =
      newStmtList(
        # Mark this part for lifting
        newCpsMustLift(
          # Create a new statement list
          NormalizedNimNode newStmtList(
            # Declare the temporary
            newVarSection(tmp, T),
            # Add the rewritten expression
            body
          )
        ),
        # Then emit our temporary as the new expression
        tmp
      )

macro cpsExprLifter(n: typed): untyped =
  ## Move cpsMustLift blocks from `n` to before `n`.
  ## Does not create a new scope.

  proc lift(n: NimNode): NimNode =
    var lifted = newStmtList()
    proc lifter(n: NimNode): NimNode =
      if n.isCpsMustLift:
        lifted.add:
          # Lift the bodies inside `n` then add it to the list of lifted bodies
          lift n.last # The body to be lifted is the last child
        # Replace `n` with an empty node
        result = newEmptyNode()

    result = newStmtList(lifted):
      filter(n, lifter)

  debugAnnotation cpsExprLifter, n:
    it = lift it

func annotate(n: NormalizedNimNode): NormalizedNimNode =
  ## Annotate expressions requiring flattening in `n`.
  result = NormalizedNimNode copyNimNode(n)

  for idx, child in n.pairs:
    let child = NormalizedNimNode child
    if child.hasCpsExpr:
      case child.kind
      of nnkVarSection, nnkLetSection:
        let child = expectVarLet(child)

        result.add:
          # Puts the section under expression lifter in case its value contains
          # expression that requires lifting.
          newCall(bindSym"cpsExprLifter"):
            newStmtList:
              NimNode:
                # Clone the VarLet and replace its value.
                child.clone:
                  # In the case where the value is a cps call, we don't have to
                  # move it outside as later CPS pass can rewrite this.
                  if child.val.isCpsCall:
                    annotate:
                      NormalizedNimNode child.val
                  else:
                    # Otherwise we transform the expression into a symbol.
                    NormalizedNimNode:
                      # TODO: normalizedast should know to run infer
                      #       automatically on nnkVarTuple because that type
                      #       doesn't have a type specifier
                      newCall(bindSym"cpsExprToTmp", copy(child.def.inferTypFromImpl)):
                        newStmtList:
                          annotate:
                            NormalizedNimNode child.val

      else:
        # Not the type of nodes that needs flattening, rewrites its child
        # and move on.
        result.add:
          NimNode annotate(child)
    else:
      # Nothing interesting here, continue.
      result.add:
        NimNode annotate(child)

macro cpsFlattenExpr*(n: typed): untyped =
  ## Flatten any CPS expression in procedure `n` so that control flow involving
  ## them is linear.
  expectKind n, nnkProcDef
  debugAnnotation cpsFlattenExpr, n:
    # debugAnnotation puts the rewritten `n` inside a StmtList, so we take it
    # out.
    it = it[0]

    # Annotate the proc body
    it.body = annotate(NormalizedNimNode it.body)
