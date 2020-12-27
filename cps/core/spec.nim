##[

boring utilities likely useful to multiple pieces of cps machinery

]##
import std/hashes
import std/sequtils
import std/macros
import std/options

when (NimMajor, NimMinor) < (1, 3):
  {.fatal: "requires nim-1.3".}

const
  cpsDebug* {.booldefine.} = false       ## produce gratuitous output
  cpsMagicExists* {.booldefine.} = true  ## make use of .cpsMagic.
  cpsZevv* {.booldefine.} = true         ## increment gensyms
  cpsCast* {.booldefine.} = false        ## use cast instead of conversion
  cpsTrace* {.booldefine.} = false       ## store "stack" traces
  cpsExcept* {.booldefine.} = false      ## also stash exceptions
  cpsFn* {.booldefine.} = false          ## multiple fns in continuations
  cpsTrampBooty* {.booldefine.} = false  ## put a tramp in da booty
  comments* = cpsDebug         ## embed comments within the transformation

template cpsLift*() {.pragma.}          ## lift this proc|type
template cpsCall*() {.pragma.}          ## a cps call
template cpsCall*(n: typed) {.pragma.}  ## redirection

type
  NodeFilter* = proc(n: NimNode): NimNode

  ContinuationProc[T] = proc(c: var T) {.nimcall.}

  Continuation* = concept cont
    ## A continuation describes the rest of the computation
    ## until the resumable function or coroutine end.
    cont.fn is ContinuationProc[Continuation]
    # cont.envs is object
    # Low-level details:
    # On the C/C++ backend:
    #   - We use raw "Continuation"
    #     if the continuation doesn't escape its scope
    #     AND involves only trivial types (no ref and no destructors)
    #     (TODO, need compiler support to tell us that
    #      but maybe "owner" can help)
    #   - cont.envs is an union type if only trivial types are involved.
    #     Type erasure via {.union.} doesn't involve RTTI and the GC.
    #   - We use "ref Continuation"
    #     if the continuation does escape.
    #   - cont.envs is an "object of RootObj"
    #     if there are non-trivial types involved (ref or destructors)
    #
    # On the JS backend:
    #   - We always use "ref Continuation"
    #   - cont.envs is always an object of RootObj

  Coroutine*[T] = concept coro
    coro is Continuation
    coro.promise is Option[T]
    coro.hasFinished is bool

  Pair* = tuple
    key: NimNode
    val: NimNode

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
  result = if n.kind == nnkSym: ident(repr n) else: n

proc resym*(n: NimNode; sym: NimNode; field: NimNode): NimNode =
  if sym.kind == nnkSym:
    let sig = signatureHash(sym)
    proc resymify(n: NimNode): NimNode =
      if n.kind == nnkSym:
        if signatureHash(n) == sig:
          result = field
    result = filter(n, resymify)
  else:
    result = n

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

when cpsDebug:
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

proc hasPragma*(n: NimNode; s: static[string]): bool =
  ## `true` if the `n` holds the pragma `s`
  assert not n.isNil
  case n.kind
  of nnkPragma:
    for p in n:
      # just skip ColonExprs, etc.
      if p.kind in {nnkSym, nnkIdent}:
        result = p.strVal == s
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
  for p in filterIt(ns, it != liftee):
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
  # TODO: cleaner fix if we have https://github.com/nim-lang/Nim/issues/9443
  # otherwise "when(compiles(addr n))" works:
  # https://github.com/numforge/laser/blob/d1e6ae6/laser/strided_iteration/foreach_common.nim#L11-L14
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
  result = anyIt(toSeq items(n), it.isLiftable or it.hasLiftableChild)
