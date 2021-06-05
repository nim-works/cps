import std/macros

type
  NodeFilter* = proc(n: NimNode): NimNode
  Matcher* = proc(n: NimNode): bool
    ## A proc that returns whether a NimNode should be replaced

proc filter*(n: NimNode; f: NodeFilter): NimNode =
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
  ## Rewrite AST into a safe form for manipulation
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

    proc rewriteHiddenAddrDeref(n: NimNode): NimNode =
      ## Remove nnkHiddenAddr/Deref because they cause the carnac bug
      case n.kind
      of nnkHiddenAddr, nnkHiddenDeref:
        result = normalizingRewrites n[0]
      else: discard

    proc rewriteConv(n: NimNode): NimNode =
      ## Rewrite a nnkConv (which is a specialized nnkCall) back into nnkCall.
      ## This is because nnkConv nodes are only valid if produced by sem.
      case n.kind
      of nnkConv:
        result = newNimNode(nnkCall, n)
        result.add:
          normalizingRewrites n[0]
        result.add:
          normalizingRewrites n[1]
      else: discard

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

    proc rewriteHidden(n: NimNode): NimNode =
      ## Unwrap hidden conversion nodes
      case n.kind
      of nnkHiddenCallConv:
        result = nnkCall.newNimNode(n)
        for child in n.items:
          result.add:
            normalizingRewrites child
      of CallNodes - {nnkHiddenCallConv}:
        if n.len > 1 and not n.findChild(it.kind == nnkHiddenStdConv).isNil:
          result = copyNimNode n
          for index, child in n.pairs:
            if child.kind != nnkHiddenStdConv:
              result.add:
                normalizingRewrites child
            else:
              # rewrite varargs conversions; perhaps can be replaced by
              # nnkArgsList if it acquires some special cps call handling
              if child.last.kind == nnkBracket:
                if n.kind in {nnkPrefix, nnkInfix, nnkPostfix} or
                   index < n.len - 1:
                  # if this varargs is not the last argument
                  # or if the call node is a binary/unary node, which
                  # then they can't have more than one/two parameters,
                  # thus the varargs must be in nnkBracket
                  result.add:
                    normalizingRewrites child.last
                else:
                  for converted in child.last.items:
                    result.add:
                      normalizingRewrites converted
              # these are implicit conversions the compiler can handle
              elif child.len > 1 and child[0].kind == nnkEmpty:
                for converted in child[1 .. ^1]:
                  result.add:
                    normalizingRewrites converted
              else:
                raise newException(Defect,
                  "unexpected conversion form:\n" & treeRepr(child))
      else:
        discard

    case n.kind
    of nnkIdentDefs:
      rewriteIdentDefs n
    of nnkLetSection, nnkVarSection:
      rewriteVarLet n
    of nnkHiddenAddr, nnkHiddenDeref:
      rewriteHiddenAddrDeref n
    of nnkConv:
      rewriteConv n
    of nnkReturnStmt:
      rewriteReturn n
    of nnkFormalParams:
      rewriteFormalParams n
    of CallNodes:
      rewriteHidden n
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

  result = filter(n, workaroundSigmatchSkip)

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
