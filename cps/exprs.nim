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
    of nnkElifBranch, nnkElifExpr, nnkWhileStmt:
      result = n[0].NormalizedNimNode.hasCpsExpr
    of nnkStmtList, nnkStmtListExpr, nnkIfStmt, nnkIfExpr, nnkCaseStmt,
       nnkAsgn:
      for child in n.items:
        if child.NormalizedNimNode.hasCpsExpr:
          return true
    else:
      result = false
  else:
    # Otherwise check if its a cps block
    result = n.isCpsBlock

func assignTo*(location: NimNode, n: NormalizedNimNode): NormalizedNimNode =
  ## Rewrite the expression `n` into a statement assigning to `location`.
  ##
  ## Returns a copy of `n` if `n` is not an expression.
  if n.typeKind == ntyNone:
    return NormalizedNimNode:
      copy n

  proc rewriteElifOf(location: NimNode, branch: NormalizedNimNode): NormalizedNimNode =
    ## Rewrite a singular of/elif/else branch
    case branch.kind
    of nnkElifBranch, nnkElifExpr:
      result =
        NormalizedNimNode:
          # Copy the branch and it's condition
          copyNimNode(branch).add(branch[0]):
            NimNode:
              # Then rewrite the body
              assignTo(location):
                NormalizedNimNode branch.last
    of nnkElse, nnkElseExpr:
      result =
        NormalizedNimNode:
          # Copy the branch
          copyNimNode(branch).add:
            NimNode:
              # Then rewrite the body
              assignTo(location):
                NormalizedNimNode branch.last
    of nnkOfBranch:
      result = NormalizedNimNode copyNimNode(branch)
      # Copy all matching conditions, which is every children except the last.
      for idx in 0 ..< branch.len - 1:
        result.add copy(branch[idx])
      # Add the rewritten body
      result.add:
        NimNode:
          assignTo(location):
            NormalizedNimNode branch.last
    else:
      result = NormalizedNimNode:
        n.errorAst "unexpected node kind in case/if expression"

  case n.kind
  of AtomicNodes, CallNodes, nnkTupleConstr, nnkObjConstr, ConvNodes:
    # For calls, conversions, constructions, constants and basic symbols, we
    # just emit the assignment.
    result = NormalizedNimNode newAssignment(copy location, copy n)
  of nnkStmtList, nnkStmtListExpr:
    result = NormalizedNimNode copyNimNode n

    # In a statement list, the last node is the expression, so we copy
    # the part before it because we won't touch them.
    for idx in 0 ..< n.len - 1:
      result.add copy(n[idx])

    # Rewrite the last expression to assign to location.
    result.add:
      # Convert back to NimNode explicitly because the compiler
      # can't handle our awesome nodes (our node binds to both
      # varargs and normal form of add).
      {.warning: "Compiler workaround here".}
      NimNode:
        assignTo(location):
          n.last.NormalizedNimNode
  of nnkBlockStmt, nnkBlockExpr:
    result = NormalizedNimNode copyNimNode(n)
    # Copy the label
    result.add copy(n[0])
    # Rewrite and add the body
    result.add:
      NimNode:
        assignTo(location):
          NormalizedNimNode n[1]
  of nnkIfStmt, nnkIfExpr:
    # It appears that the type of the `if` expression remains if we
    # don't destroy it by creating a new node instead of copying and
    # causes all sort of errors.
    {.warning: "compiler workaround here".}
    result = NormalizedNimNode newNimNode(n.kind, n)

    for branch in n.items:
      result.add:
        NimNode:
          rewriteElifOf(location):
            NormalizedNimNode branch
  of nnkCaseStmt:
    # It appears that the type of the `case` expression remains if we
    # don't destroy it by creating a new node instead of copying and
    # causes all sort of errors.
    {.warning: "compiler workaround here".}
    result = NormalizedNimNode newNimNode(n.kind, n)

    # Copy the matched expression
    result.add copy(n[0])

    # Rewrite and add branches
    for idx in 1 ..< n.len:
      result.add:
        NimNode:
          rewriteElifOf(location):
            NormalizedNimNode n[idx]
  of nnkTryStmt:
    # Similar to other node types, we must erase the type attached to this
    # statement.
    result = NormalizedNimNode newNimNode(n.kind, n)

    # Rewrite the body
    result.add:
      NimNode:
        assignTo(location):
          NormalizedNimNode n[0]

    # Rewrite except/finally branches
    for idx in 1 ..< n.len:
      let branch = n[idx]
      case branch.kind
      of nnkExceptBranch:
        let newBranch = copyNimNode(branch)
        # Copy all exception matching predicates, which is every node but
        # the last
        for idx in 0 ..< branch.len - 1:
          newBranch.add copy(branch[idx])

        # Rewrite and add the body
        newBranch.add:
          NimNode:
            assignTo(location):
              NormalizedNimNode branch.last

        # Add the branch to the new try statement
        result.add newBranch
      of nnkFinally:
        # Per Nim manual, a finally branch cannot contain an expression, thus
        # we can skip the rewrite
        result.add copy(branch)
      else:
        result.add:
          branch.errorAst "unexpected node in a try expression"
  else:
    result = NormalizedNimNode:
      n.errorAst "cps doesn't know how to rewrite this into assignment"

func isMutable(n: NormalizedNimNode): bool =
  ## Determine whether `n` is mutable, as in if `n` value can be mutated.
  case n.kind
  of nnkSym:
    result = n.symKind notin {
      nskGenericParam, nskParam, nskModule, nskType, nskLet, nskConst, nskProc,
      nskFunc, nskMethod, nskIterator, nskConverter, nskMacro, nskTemplate,
      nskEnumField, nskField, nskLabel
    }
  of AtomicNodes - {nnkSym}:
    result = false
  of nnkAddr, nnkHiddenAddr:
    # An expression's address always allow it to be mutated
    result = true
  of nnkDotExpr:
    # The mutability of a dot expression (field access in typed AST) relies
    # solely on its first operand (ie. `o.i` is mutable if `o` is mutable)
    result = n[0].NormalizedNimNode.isMutable
  of CallNodes:
    # TODO: analyze calls, might only require mutability analysis on
    # params + whether the call has side effects.
    #
    # For now we assume that all calls produce an expression that can be
    # mutated by other statements.
    result = true
  of ConvNodes:
    # For conversions the mutability depends on the converted expression
    result = n[1].NormalizedNimNode.isMutable
  else:
    for child in n:
      if child.NormalizedNimNode.isMutable:
        return true

func isMutableLocation(location: NormalizedNimNode): bool =
  ## Determine whether `location` is mutable, that is, the address of it
  ## can be modified by other statements.
  case location.kind
  of nnkHiddenDeref, nnkDerefExpr:
    # This node produces the location that is stored in its child, thus the
    # location is mutable if the child is mutable.
    result = location[0].NormalizedNimNode.isMutable
  of nnkAddr, nnkHiddenAddr:
    # This node produces the location of its child, thus it is mutable if the
    # child's location is mutable.
    result = location[0].NormalizedNimNode.isMutableLocation
  else:
    for child in location:
      if child.NormalizedNimNode.isMutableLocation:
        return true

func isComplexStatement(n: NormalizedNimNode): bool =
  ## Determine whether `n` is a complex statement.
  ##
  ## A complex statement is any statement that can't be copy-pasted across the
  ## AST without causing trouble, which includes:
  ## - Calls
  ## - Assignments
  ## - Conditonals
  ## - Loops
  ## - ProcDefs
  case n.kind
  of AtomicNodes:
    result = false
  of CallNodes, nnkAsgn, nnkBreakStmt, nnkContinueStmt, nnkReturnStmt,
     nnkIfStmt, nnkTryStmt, nnkCaseStmt, nnkWhileStmt, nnkRaiseStmt,
     nnkYieldStmt, RoutineNodes:
    result = true
  else:
    for child in n:
      if child.NormalizedNimNode.isComplexStatement:
        return true

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

macro cpsAsgn(dst, src: typed): untyped =
  ## Flatten the cps expression `src` into explicit assignments to `dst`.
  let dst = NormalizedNimNode normalizingRewrites(dst)
  debugAnnotation cpsAsgn, src:
    if dst.isComplexStatement:
      it = dst.errorAst(
        "The destination is a complex statement, which CPS does not support" &
        " at the moment.\n" &
        "To workaround this, assign the expression to a temporary variable," &
        " then assign it to your destination."
      )
    elif dst.isMutableLocation:
      it = dst.errorAst(
        "The destination's address is mutable, as such CPS cannot guarantee" &
        " that the expression will be assigned to the correct location.\n" &
        "To workaround this, assign the expression to a temporary variable," &
        " then assign it to your destination."
      )
    else:
      it = assignTo(dst):
        # debugAnnotation wrap our typed body in a stmtlist, so we take it
        # out
        NormalizedNimNode it[0]

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

      of nnkElifBranch, nnkElifExpr:
        # If the elif branch is the very first branch, tag it for rewrite.
        if idx == 0:
          result.add:
            copyNimNode(child).add(
              # Tag the condition for rewrite and annotate it
              newCall(bindSym"cpsExprToTmp", getTypeInst(child[0]),
                      annotate(NormalizedNimNode child[0])),
              # Annotate the branch body too
              annotate NormalizedNimNode(child.last)
            )

        # Otherwise, we move the remaining branches into a new if
        # statement and push it as the else branch of this statement.
        #
        # The rationale for this is that we can only lift an elif branch
        # condition outside a container like `IfStmt`.
        else:
          result.add:
            # Create a new else branch and annotate it
            NimNode:
              annotate:
                NormalizedNimNode:
                  nnkElse.newTree:
                    newStmtList:
                      # Put every branch from here on into a new if statement.
                      nnkIfStmt.newTree(n[idx .. ^1])

          # We are done with this tree.
          break

      of nnkIfStmt, nnkIfExpr:
        # Put the if statement under the expression lifter since it
        # has at least one children with a cps expression as condition.
        result.add:
          newCall(bindSym"cpsExprLifter"):
            newStmtList:
              annotate child

      of nnkCaseStmt:
        # Run an annotation pass on the child first so any potential
        # elif branch is rewritten.
        let newCase = annotate child

        # If the case matching expression is a cps expression.
        #
        # We are checking the original condition because it would have not
        # went through any rewriting passes and retain type information.
        if child[0].NormalizedNimNode.hasCpsExpr:
          # However since the annotation pass above already rewritten the
          # condition for us, we can just take it and wrap it in
          # cpsExprToTmp.
          newCase[0] =
            # Again, we are taking the type from the original since it
            # have the correct type information.
            newCall(bindSym"cpsExprToTmp", getTypeInst(child[0])):
              newCase[0]

          # Put the new case in the expression lifter to lift the rewritten
          # matching expression.
          result.add:
            newCall(bindSym"cpsExprLifter"):
              newStmtList:
                newCase
        else:
          # We can just add the case as-is otherwise.
          result.add NimNode(newCase)

      of nnkWhileStmt:
        # Unlike if and/or case, the condition of a while branch is evaluated
        # on every loop, so we can't just move it out of the branch.
        #
        # The solution is to rewrite:
        #
        # while cond:
        #   body
        #
        # into:
        #
        # while true:
        #   if cond:
        #     body
        #   else:
        #     break
        #
        # Which will let us move `cond` outside of `if` and have it evaluated
        # before every loop.
        let newWhile = copyNimNode(child)
        newWhile.add newLit(true)

        # Add the new loop body
        newWhile.add:
          NimNode:
            # Run annotate on the if statement to rewrite its condition and body
            annotate:
              NormalizedNimNode:
                newStmtList:
                  # A new if statement
                  nnkIfStmt.newTree(
                    # The first branch being the condition and the body
                    nnkElifBranch.newTree(child[0], child.last),
                    # The else branch breaks the loop
                    nnkElse.newTree(nnkBreakStmt.newTree(newEmptyNode()))
                  )

        result.add newWhile

      of nnkAsgn:
        if child[0].NormalizedNimNode.hasCpsExpr:
          result.add:
            child[0].errorAst(
              "The left hand side of the following assignment is a CPS expression which is not supported:\n" & repr(child)
            )
        else:
          let asgnCall = newCall(bindSym"cpsAsgn", copy(child[0])):
            NimNode annotate(NormalizedNimNode child[1])

          asgnCall.copyLineInfo(child)
          result.add asgnCall

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
    it.body = annotate:
      NormalizedNimNode:
        # Always put the body under a statement list in the case where there
        # is only one node in the body.
        newStmtList:
          it.body
