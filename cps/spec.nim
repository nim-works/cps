##[

boring utilities likely useful to multiple pieces of cps machinery

]##
import std/hashes
import std/sequtils
import std/macros
import std/locks

when (NimMajor, NimMinor) < (1, 3):
  {.fatal: "requires nim-1.3".}

const
  cpsDebug* {.booldefine.} = false       ## produce gratuitous output
  cpsTrace* {.booldefine.} = false       ## store "stack" traces
  comments* = cpsDebug         ## embed comments within the transformation

template cpsLift*() {.pragma.}          ## lift this proc|type
template cpsCall*() {.pragma.}          ## a cps call
template cpsCall*(n: typed) {.pragma.}  ## redirection
template cpsPending*() {.pragma.}       ## this is the last continuation
template cpsBreak*(label: typed = nil) {.pragma.} ## this is a break statement in a cps block
template cpsContinue*() {.pragma.}      ## this is a continue statement in a cps block

type
  NodeFilter* = proc(n: NimNode): NimNode

  Continuation* = concept c
    c.fn is ContinuationProc[Continuation]
    c.lock is Lock
    c is ref object
    c of RootObj

  ContinuationProc*[T] = proc(c: T): T {.nimcall.}

  Pair* = tuple
    key: NimNode
    val: NimNode

  AstKind* {.pure.} = enum
    ## The type of the passed AST
    Original = "original"
    Transformed = "transformed"

  Matcher* = proc(n: NimNode): bool
    ## A proc that returns whether a NimNode should be replaced

func isEmpty*(n: NimNode): bool =
  ## `true` if the node `n` is Empty
  result = not n.isNil and n.kind == nnkEmpty

proc filter*(n: NimNode; f: NodeFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

proc desym*(n: NimNode): NimNode =
  result = n
  if n.kind == nnkSym:
    result = ident(repr n)
    result.copyLineInfo n

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
    #of nnkTypeSection:
    #  result = n
    of nnkSym:
      if n.strVal notin ["cpsLift", "cpsCall"]:
        result = desym n
      else:
        result = n
    else:
      discard
  result = filter(n, desymifier)

func stripComments*(n: NimNode): NimNode =
  ## remove doc statements because that was a stupid idea
  result = copyNimNode n
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add stripComments(child)

when not cpsDebug:
  template debug*(ignore: varargs[untyped]) = discard
else:
  import std/strutils

  proc `$`(p: Pair): string {.used.} =
    result = p[0].repr & ": " & p[1].repr

  proc numberedLines*(s: string; first = 1): string =
    for n, line in pairs(splitLines(s, keepEol = true)):
      result.add "$1  $2" % [ align($(n + first), 3), line ]

  proc snippet*(n: NimNode; name: string): string =
    result &= "----8<---- " & name & "\t" & "vvv"
    result &= "\n" & n.repr.numberedLines(n.lineInfoObj.line) & "\n"
    result &= "----8<---- " & name & "\t" & "^^^"

  func debug*(id: string, n: NimNode, kind: AstKind, info: NimNode = nil) =
    ## Debug print the given node `n`, with `id` is a string identifying the
    ## caller and `info` specifies the node to retrieve the line information
    ## from.
    ##
    ## If `info` is `nil`, the line information will be retrieved from `n`.
    let info =
      if info.isNil:
        n
      else:
        info

    let lineInfo = info.lineInfoObj

    let procName =
      if info.kind in RoutineNodes:
        repr info.name
      else:
        ""

    debugEcho "=== $1 $2($3) === $4" % [
      id,
      if procName.len > 0: "on " & procName else: "",
      $kind,
      $lineInfo
    ]
    when defined(cpsTree):
      debugEcho treeRepr(n)
    else:
      debugEcho repr(n).numberedLines(lineInfo.line)

proc getPragmaName(n: NimNode): NimNode =
  ## retrieve the symbol/identifier from the child node of a nnkPragma
  case n.kind
  of nnkCall, nnkExprColonExpr:
    n[0]
  else:
    n

proc hasPragma*(n: NimNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  assert not n.isNil
  case n.kind
  of nnkPragma:
    for p in n:
      # just skip ColonExprs, etc.
      result = p.getPragmaName.eqIdent s
  of RoutineNodes:
    result = hasPragma(n.pragma, s)
  of nnkObjectTy:
    result = hasPragma(n[0], s)
    assert n[0].kind == nnkPragma
  of nnkRefTy:
    result = hasPragma(n.last, s)
  of nnkTypeDef:
    result = hasPragma(n.last, s)
  of nnkTypeSection:
    result = anyIt(toSeq items(n), hasPragma(it, s))
  else:
    result = false

proc filterPragma*(ns: seq[NimNode], liftee: NimNode): NimNode =
  ## given a seq of pragmas, omit a match and return Pragma or Empty
  var pragmas = nnkPragma.newNimNode
  for p in filterIt(ns, it.getPragmaName != liftee):
    pragmas.add p
    copyLineInfo(pragmas, p)
  if len(pragmas) > 0:
    pragmas
  else:
    newEmptyNode()

proc stripPragma*(n: NimNode; s: static[string]): NimNode =
  ## filter a pragma with the matching name from various nodes
  case n.kind
  of nnkPragma:
    result = filterPragma(toSeq n, bindSym(s))
  of RoutineNodes:
    n.pragma = stripPragma(n.pragma, s)
    result = n
  of nnkObjectTy:
    n[0] = filterPragma(toSeq n[0], bindSym(s))
    result = n
  of nnkRefTy:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeDef:
    n[^1] = stripPragma(n.last, s)
    result = n
  of nnkTypeSection:
    result = newNimNode(n.kind, n)
    for item in items(n):
      result.add stripPragma(item, s)
  else:
    result = n

func doc*(s: string): NimNode =
  ## generate a doc statement for debugging
  when comments:
    newCommentStmtNode(s)
  else:
    newEmptyNode()

proc doc*(n: var NimNode; s: string) =
  ## add a doc statement to the ast for debugging
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

template twice*(body: untyped): untyped =
  ## an exercise in futility, apparently
  block twice:
    once:
      break twice
    body

proc hash*(n: NimNode): Hash =
  var h: Hash = 0
  h = h !& hash($n)
  result = !$h

proc init*[T](c: T; l: LineInfo): T =
  warning "provide an init proc for cpsTrace"

proc definedName*(n: NimNode): NimNode =
  ## create an identifier from an typesection/identDef as cached;
  ## this is a copy and it is repr'd to ensure gensym compat...
  assert n.kind in {nnkVarSection, nnkLetSection}, "use this on env[key]"
  result = ident(repr(n[0][0]))

proc stripVar*(n: NimNode): NimNode =
  ## pull the type out of a VarTy
  result = if n.kind == nnkVarTy: n[0] else: n

proc letOrVar*(n: NimNode): NimNodeKind =
  ## choose between let or var for proc parameters
  assert n.kind == nnkIdentDefs
  if len(n) == 2:
    # ident: type; we'll add a default for numbering reasons
    n.add newEmptyNode()
  case n[^2].kind
  of nnkEmpty:
    error "i need a type: " & repr(n)
  of nnkVarTy:
    result = nnkVarSection
  else:
    result = nnkLetSection

proc isLiftable*(n: NimNode): bool =
  ## is this a node we should float to top-level?
  result = n.kind in {nnkProcDef, nnkTypeSection} and n.hasPragma "cpsLift"

proc hasLiftableChild*(n: NimNode): bool =
  ## does this node contain liftable nodes?
  result = anyIt(toSeq items(n), it.isLiftable or it.hasLiftableChild)

when cpsDebug:
  import os
  template lineAndFile*(n: NimNode): string =
    $n.lineInfoObj.line & " of " & extractFilename($n.lineInfoObj.filename)
else:
  template lineAndFile*(n: NimNode): string = "(no debug)"

proc errorAst*(s: string, info: NimNode = nil): NimNode =
  ## produce {.error: s.} in order to embed errors in the ast
  ##
  ## optionally take a node to set the error line information
  result = nnkPragma.newTree:
    ident"error".newColonExpr: newLit s
  if not info.isNil:
    result[0].copyLineInfo info

proc errorAst*(n: NimNode; s = "creepy ast"): NimNode =
  ## embed an error with a message, the line info is copied from the node
  ## too
  # TODO: we might no longer need this now that the other version can
  #       get the line info
  errorAst(s & ":\n" & treeRepr(n) & "\n", n)

proc genField*(ident = ""): NimNode =
  ## generate a unique field to put inside an object definition
  ##
  ## made as a workaround for [nim-lang/Nim#17851](https://github.com/nim-lang/Nim/issues/17851)
  desym genSym(nskField, ident)

proc normalizingRewrites*(n: NimNode): NimNode =
  ## Rewrite AST into a safe form for manipulation
  proc rewriter(n: NimNode): NimNode =
    proc rewriteIdentDefs(n: NimNode): NimNode =
      ## Rewrite an identDefs to ensure it has three children.
      if n.kind == nnkIdentDefs:
        if n.len == 2:
          n.add newEmptyNode()
        elif n[1].isEmpty:          # add explicit type symbol
          n[1] = getTypeInst n[2]
        n[2] = normalizingRewrites n[2]
        result = n

    proc rewriteVarLet(n: NimNode): NimNode =
      ## Rewrite a var|let section of multiple identDefs
      ## into multiple such sections with well-formed identDefs
      if n.kind in {nnkLetSection, nnkVarSection}:
        result = newStmtList()
        for child in n.items:
          case child.kind
          of nnkVarTuple:
            # a new section with a single rewritten identdefs within
            # for each symbol in the VarTuple statement
            for i, value in child.last.pairs:
              result.add:
                newNimNode(n.kind, n).add:
                  rewriteIdentDefs:  # for consistency
                    newIdentDefs(child[i], getType(value), value)
          of nnkIdentDefs:
            # a new section with a single rewritten identdefs within
            result.add:
              newNimNode(n.kind, n).add:
                rewriteIdentDefs child
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
          result = copyNimNode(n)
          doAssert repr(n[0][0]) == "result", "unexpected AST"
          result.add:
            normalizingRewrites n[0][1]
        else:
          result = n
      else: discard

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

proc multiReplace*(n: NimNode, replacements: varargs[(Matcher, NimNode)]): NimNode =
  ## Replace any node in `n` that is matched by a matcher in replacements with
  ## a copy of the accompanying NimNode.
  # Nim's closure capture algo strikes again
  let replacements = @replacements
  proc replacer(n: NimNode): NimNode =
    result = nil
    for (match, replacement) in replacements:
      if match(n):
        result = copyNimTree replacement
        break

  filter(n, replacer)

func newCpsPending*(): NimNode =
  ## Produce a {.cpsPending.} annotation
  nnkPragma.newTree:
    bindSym"cpsPending"

proc isCpsPending*(n: NimNode): bool =
  ## Return whether a node is a {.cpsPending.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsPending")

func newCpsBreak*(label: NimNode = newNilLit()): NimNode =
  ## Produce a {.cpsPending.} annotation with the given label
  doAssert not label.isNil
  let label =
    if label.kind == nnkEmpty:
      newNilLit()
    else:
      label

  nnkPragma.newTree:
    newColonExpr(bindSym"cpsBreak", label)

proc isCpsBreak*(n: NimNode): bool =
  ## Return whether a node is a {.cpsBreak.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsBreak")

func newCpsContinue*(): NimNode =
  ## Produce a {.cpsContinue.} annotation
  nnkPragma.newTree:
    bindSym"cpsContinue"

proc isCpsContinue*(n: NimNode): bool =
  ## Return whether a node is a {.cpsContinue.} annotation
  n.kind == nnkPragma and n.len == 1 and n.hasPragma("cpsContinue")

proc breakLabel*(n: NimNode): NimNode =
  ## Return the break label of a `break` statement or a `cpsBreak` annotation
  if n.isCpsBreak():
    if n[0].len > 1 and n[0][1].kind != nnkNilLit:
      n[0][1]
    else:
      newEmptyNode()
  elif n.kind == nnkBreakStmt:
    n[0]
  else:
    raise newException(Defect, "this node is not a break: " & $n.kind)

func hasDefer*(n: NimNode): bool =
  ## Return whether there is a `defer` within the given node
  ## that might cause it to be rewritten.
  case n.kind
  of nnkDefer:
    result = true
  of nnkStmtList, nnkStmtListExpr:
    for child in n:
      result = hasDefer(child)
      if result:
        break
  else:
    result = false

proc rewriteDefer*(n: NimNode): NimNode =
  ## Rewrite the AST of `n` so that all `defer` nodes are
  ## transformed into try-finally

  # TODO: This could be made simpler

  proc splitDefer(n: NimNode): tuple[before, `defer`, affected: NimNode] =
    ## Cut the AST into three parts:
    ## - One contains all nodes before the defer that could affect `n`
    ## - The defer node itself
    ## - All nodes that comes after the defer node (affected by the defer)
    ##
    ## If there are no defer in the AST, all nodes are left as-is in
    ## `before`.
    case n.kind
    of nnkDefer:
      # Got the defer
      result.defer = n
    of nnkStmtList, nnkStmtListExpr:
      # Make a copy of our node to the part before defer
      result.before = copyNimNode(n)
      # The rest of the split stays in a new node of the same kind
      result.affected = newNimNode(n.kind)

      # Look for the defer in the child nodes
      for idx, child in n:
        if child.hasDefer:
          let collected = splitDefer(child)
          result.defer = collected.defer
          if not collected.before.isNil:
            result.before.add collected.before

          # Add nodes coming after the defer to the list of affected nodes
          if not collected.affected.isNil:
            result.affected.add collected.affected
          if idx < n.len - 1:
            result.affected.add n[idx + 1 .. ^1]
          # We are done here
          break

        # If there are no defer in the child node, add as-is
        result.before.add child
    else:
      # This node doesn't contain any defer, return as-is
      result.before = n

  if n.hasDefer and n.kind != nnkDefer:
    let (before, deferNode, affected) = splitDefer(n)
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
    result = copyNimNode(n)
    # Process its children instead
    for child in n:
      result.add:
        rewriteDefer child
