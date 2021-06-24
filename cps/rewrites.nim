import std/macros

type
  NodeFilter* = proc(n: NimNode): NimNode
  Matcher* = proc(n: NimNode): bool
    ## A proc that returns whether a NimNode should be replaced

proc filter*(n: NimNode; f: NodeFilter): NimNode =
  ## rewrites a node and its children by passing each node to the filter;
  ## if the filter yields nil, the node is simply copied.  otherwise, the
  ## node is replaced.
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

func isEmpty*(n: NimNode): bool =
  ## `true` if the node `n` is Empty
  result = not n.isNil and n.kind == nnkEmpty

proc errorAst*(s: string, info: NimNode = nil): NimNode =
  ## produce {.error: s.} in order to embed errors in the ast
  ##
  ## optionally take a node to set the error line information
  result = nnkPragma.newTree:
    ident"error".newColonExpr: newLit s
  if not info.isNil:
    result[0].copyLineInfo info

proc errorAst*(n: NimNode; s = "creepy ast"): NimNode =
  ## embed an error with a message,
  ## the line info is copied from the node
  errorAst(s & ":\n" & treeRepr(n) & "\n", n)

proc desym*(n: NimNode): NimNode =
  result = n
  if n.kind == nnkSym:
    result = ident(repr n)
    result.copyLineInfo n

proc genField*(ident = ""): NimNode
  {.deprecated: "pending https://github.com/nim-lang/Nim/issues/17851".} =
  ## generate a unique field to put inside an object definition
  desym genSym(nskField, ident)

proc resym*(n: NimNode; sym: NimNode; field: NimNode): NimNode =
  ## seems we only use this for rewriting local symbols into symbols
  ## in the env, so we'll do custom handling of identDefs here also
  expectKind(sym, nnkSym)
  proc resymify(n: NimNode): NimNode =
    case n.kind
    of nnkIdentDefs:
      # we want to skip the name and rewrite the other children
      for i in 1 ..< n.len:
        if n[i].kind != nnkEmpty:
          n[i] = filter(n[i], resymify)
      result = n
    of nnkSym:
      if n == sym:
        result = field
    else:
      discard
  result = filter(n, resymify)

proc replacedSymsWithIdents*(n: NimNode): NimNode =
  proc desymifier(n: NimNode): NimNode =
    case n.kind
    of nnkSym:
      result = desym n
    else:
      discard
  result = filter(n, desymifier)

proc normalizingRewrites*(n: NimNode): NimNode =
  ## Rewrite AST into a safe form for manipulation without removing semantic
  ## data.
  proc rewriter(n: NimNode): NimNode =
    proc rewriteIdentDefs(n: NimNode): NimNode =
      ## Rewrite an identDefs to ensure it has three children.
      if n.kind == nnkIdentDefs:
        if n.len == 2:
          n.add newEmptyNode()
        elif n[^2].isEmpty:          # add explicit type symbol
          n[^2] = getTypeInst n[^1]
        n[^1] = normalizingRewrites n[^1]
        result = n

    proc rewriteVarLet(n: NimNode): NimNode =
      ## Rewrite a var|let section of multiple identDefs
      ## into multiple such sections with well-formed identDefs
      if n.kind in {nnkLetSection, nnkVarSection}:
        result = newStmtList()
        for child in n.items:
          case child.kind
          of nnkVarTuple:
            # we used to rewrite these, but the rewrite is only safe for
            # very trivial deconstructions.  now we rewrite the symbols
            # into the env during the saften pass as necessary.
            #
            # a new section with a single VarTuple statement
            result.add:
              newNimNode(n.kind, n).add:
                child
          of nnkIdentDefs:
            # a new section with a single rewritten identdefs within
            let defs = rewriteIdentDefs(child)
            for d in defs[0 .. ^3].items: # last two nodes are type and rhs
              result.add:
                newNimNode(n.kind, n).add:
                  newIdentDefs(d, copyNimTree(defs[^2]), copyNimTree(defs[^1]))
          else:
            result.add:
              child.errorAst "unexpected"

    proc rewriteReturn(n: NimNode): NimNode =
      ## Inside procs, the compiler might produce an AST structure like this:
      ##
      ## ```
      ## ReturnStmt
      ##   Asgn
      ##     Sym "result"
      ##     Sym "continuation"
      ## ```
      ##
      ## for `return continuation`.
      ##
      ## This structure is not valid if modified.
      ##
      ## Rewrite this back to `return expr`.
      case n.kind
      of nnkReturnStmt:
        if n[0].kind == nnkAsgn:
          if repr(n[0][0]) != "result":
            result = n.errorAst "unexpected return assignment form"
          else:
            result = copyNimNode(n)
            result.add:
              normalizingRewrites n[0][1]
        else:
          result = n
      else: discard

    proc rewriteFormalParams(n: NimNode): NimNode =
      ## make formal params such as `foo(a, b: int)` into `foo(a: int, b: int)`
      case n.kind
      of nnkFormalParams:
        result = nnkFormalParams.newNimNode(n)
        result.add:
          normalizingRewrites n[0] # return value
        for arg in n[1 .. ^1].items:
          case arg.kind
          of nnkIdentDefs:
            # if there is more than one param defined, then break them up
            let defs = rewriteIdentDefs arg
            for d in defs[0 .. ^3].items: # last two nodes are type and rhs
              result.add:
                newIdentDefs(d, copyNimTree(defs[^2]), copyNimTree(defs[^1]))
          else:
            result.add:
              # sometimes we have symbols, they get desymed elsewhere
              normalizingRewrites arg
      else:
        discard

    proc rewriteExceptBranch(n: NimNode): NimNode =
      ## Rewrites except branches in the form of `except T as e` into:
      ##
      ## ```
      ## except T:
      ##   let e = (ref T)(getCurrentException())
      ## ```
      ##
      ## We simplify this AST so that our rewrites can capture it.
      case n.kind
      of nnkExceptBranch:
        # If this except branch has exactly one exception matching clause
        if n.len == 2:
          # If the exception matching clause is an infix expression (T as e)
          if n[0].kind == nnkInfix:
            let
              typ = n[0][1] # T in (T as e)
              refTyp = nnkRefTy.newTree(typ) # make a (ref T) node
              ex = n[0][2] # our `e`
              body = n[1]

            result = copyNimNode(n) # copy the `except`
            result.add typ # add only `typ`
            result.add:
              newStmtList:
                # let ex: ref T = (ref T)(getCurrentException())
                nnkLetSection.newTree:
                  newIdentDefs(ex, refTyp, newCall(refTyp, newCall(bindSym"getCurrentException")))

            # add the rewritten body
            result.last.add:
              normalizingRewrites body
      else: discard

    proc rewriteVarargsTypedCalls(n: NimNode): NimNode =
      # Unpack `varargs[typed]` calls, usually `echo`.
      #
      # There is little value in keeping the packed form of varargs[typed]
      # parameters, as we cannot replicate them in any transformation,
      # and the compiler will rebuild the typed array every time we unpack it.
      if n.kind in {nnkCall, nnkCommand}:
        proc isMagicalVarargs(n: NimNode): bool =
          ## Check if `n` is a magical varargs type that require special
          ## processing in a call.
          ##
          ## Currently we look for `varargs[typed]`, which is a special type
          ## that can only be used by magic procedures, specifically
          ## `echo`.
          ##
          ## While this type can be used in macros/templates, those calls
          ## would already be resolved by the time it reaches us.
          if n.typeKind == ntyVarargs:
            # This should give us `varargs[T]`
            let typ = getTypeInst n
            # If `T` is of kind Stmt, Expr (which are typed, untyped
            # respectively), then this node need special processing
            if typ[1].typeKind in {ntyStmt, ntyExpr}:
              true
            else:
              false
          else:
            false

        if n.len > 1 and n.findChild(it.isMagicalVarargs) != nil:
          if n.len > 2:
            # The only case we know how to rewrite is when there is only
            # one parameter which is the magical varargs
            result = n.errorAst("unsupported call with magical varargs that have more than two parameters")
          else:
            # We got a magic call, typically echo.
            #
            # Calls with `varargs[typed, conv]` is implemented like this:
            #
            #   echo 1, 2, 3
            #
            # is re-written into:
            #
            #   echo HiddenStdConv([conv(1), conv(2), conv(3)])
            #
            # where HiddenStdConv bares the type `varargs[typed]`, effectively
            # stopping the conversion from being done again.
            #
            # To correctly unwrap these nodes, we must remove HiddenStdConv
            # then move everything within the inner array outside as parameters
            # of the call.

            # Copy our call node
            result = copyNimNode n
            # Add the symbol being called
            result.add:
              normalizingRewrites n[0]
            # Retrieve the array from the magical conversion
            let unwrapped = n[1].last
            # Add everything in this array as parameters of the call
            for i in unwrapped.items:
              result.add i

    case n.kind
    of nnkIdentDefs:
      rewriteIdentDefs n
    of nnkLetSection, nnkVarSection:
      rewriteVarLet n
    of nnkReturnStmt:
      rewriteReturn n
    of nnkFormalParams:
      rewriteFormalParams n
    of nnkExceptBranch:
      rewriteExceptBranch n
    of CallNodes:
      rewriteVarargsTypedCalls n
    else:
      nil

  filter(n, rewriter)

proc workaroundRewrites*(n: NimNode): NimNode =
  ## Rewrite AST after modification to ensure that sem doesn't
  ## skip any nodes we introduced.
  proc rewriteContainer(n: NimNode): NimNode =
    ## Helper function to recreate a container node while keeping all children
    ## to discard semantic data attached to the container.
    ##
    ## Returns the same node if its not a container node.
    result = n
    if n.kind notin AtomicNodes:
      result = newNimNode(n.kind, n)
      for child in n:
        result.add child

  proc workaroundSigmatchSkip(n: NimNode): NimNode =
    ## `sigmatch` skips any call nodes whose parameters have a type attached.
    ## We rewrites all call nodes to remove this data.
    if n.kind in nnkCallKinds:
      # We recreate the nodes here, to set their .typ to nil
      # so that sigmatch doesn't decide to skip it
      result = newNimNode(n.kind, n)
      for child in n.items:
        # The containers of direct children always has to be rewritten
        # since they also have a .typ attached from the previous sem pass
        result.add:
          rewriteContainer:
            workaroundRewrites child

  proc workaroundCompilerBugs(n: NimNode): NimNode =
    proc rewriteHiddenAddrDeref(n: NimNode): NimNode =
      ## Remove nnkHiddenAddr/Deref because they cause the carnac bug
      case n.kind
      of nnkHiddenAddr, nnkHiddenDeref:
        workaroundRewrites(n[0])
      else:
        nil

    proc rewriteConv(n: NimNode): NimNode =
      ## Rewrite a nnkConv (which is a specialized nnkCall) back into nnkCall.
      ## This is because nnkConv nodes are only valid if produced by sem.
      case n.kind
      of nnkConv:
        result = newNimNode(nnkCall, n)
        for child in n:
          result.add:
            workaroundRewrites(child)
      else: discard

    proc rewriteHiddenConv(n: NimNode): NimNode =
      ## Unwrap hidden conversion nodes
      case n.kind
      of nnkHiddenStdConv, nnkHiddenSubConv:
        # These are hidden conversion nodes, used when an implicit conversion
        # happens, like in:
        #
        # let x: int = 0.Natural
        #
        # 0.Natural has to be converted into `int` before it could be assigned
        # to x, and this is how it's done in typed ast.
        #
        # In the compiler, these nodes are added once the compiler has
        # determined that the code is semantically correct, thus any
        # modifications on these node will cause the compiler to not work
        # correctly since it assumes these nodes correctness.
        #
        # Hence we unwrap them here to force the compiler to re-evaluate
        # the modified nodes.
        if n.len == 2 and n[0].kind == nnkEmpty:
          result = workaroundRewrites n[1]
        else:
          raise newException(Defect,
            "unexpected conversion form:\n" & treeRepr(n))

      of nnkHiddenCallConv:
        result = nnkCall.newNimNode(n)
        for child in n.items:
          result.add:
            workaroundRewrites child

      else:
        discard


    case n.kind
    of nnkHiddenAddr, nnkHiddenDeref:
      rewriteHiddenAddrDeref(n)
    of nnkConv:
      rewriteConv(n)
    of nnkHiddenStdConv, nnkHiddenSubConv, nnkHiddenCallConv:
      rewriteHiddenConv(n)
    else:
      nil

  result = filter(n, workaroundCompilerBugs)
  # The previous pass will produce more call nodes, which have to be processed
  # by this pass
  result = filter(result, workaroundSigmatchSkip)

func replace*(n: NimNode, match: Matcher, replacement: NimNode): NimNode =
  ## Replace any node in `n` that is matched by `match` with a copy of
  ## `replacement`
  proc replacer(n: NimNode): NimNode =
    if match(n):
      copyNimTree replacement
    else:
      nil

  filter(n, replacer)

template replace*(n, noob: NimNode; body: untyped): NimNode {.dirty.} =
  ## requires --define:nimWorkaround14447 so...  yeah.
  let match = proc(it {.inject.}: NimNode): bool = body
  replace(n, match, noob)

proc multiReplace*(n: NimNode;
                   replacements: varargs[(Matcher, NimNode)]): NimNode =
  ## Replace any node in `n` that is matched by a matcher in replacements
  ## with a copy of the accompanying NimNode.
  # Nim's closure capture algo strikes again
  let replacements = @replacements
  proc replacer(n: NimNode): NimNode =
    result = nil
    for (match, replacement) in replacements:
      if match(n):
        result = copyNimTree replacement
        break

  filter(n, replacer)

proc ensimilate*(source: NimNode; destination: NimNode): NimNode =
  ## perform a call to convert the destination to the source's type;
  ## the source can be any of a few usual suspects...
  let typ = getTypeImpl source
  block:
    if typ.isNil:
      break
    else:
      case typ.kind
      of nnkEmpty:
        break
      of nnkProcTy:
        result = newCall(typ[0][0], destination)
      of nnkRefTy:
        result = newCall(typ[0], destination)
      elif typ.kind == nnkSym and $typ == "void":
        break
      else:
        result = newCall(typ, destination)
      return

  # fallback to typeOf
  result = newCall(newCall(bindSym"typeOf", source), destination)
