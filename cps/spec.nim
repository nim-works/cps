##[

boring utilities likely useful to multiple pieces of cps machinery

]##
import std/hashes
import std/sequtils
import std/macros

when (NimMajor, NimMinor) < (1, 3):
  {.fatal: "requires nim-1.3".}

const
  cpsDebug* {.booldefine.} = false       ## produce gratuitous output
  cpsMagicExists* {.booldefine.} = true  ## make use of .cpsMagic.
  cpsMutant* {.booldefine.} = false      ## mutate continuations
  cpsZevv* {.booldefine.} = true         ## increment gensyms
  cpsCast* {.booldefine.} = false        ## use cast instead of conversion
  cpsTrace* {.booldefine.} = false       ## store "stack" traces
  cpsExcept* {.booldefine.} = false      ## also stash exceptions
  cpsFn* {.booldefine.} = false          ## multiple fns in continuations
  cpsTrampBooty* {.booldefine.} = false  ## put a tramp in da booty
  cpsTrustSigHash* {.booldefine.} = true ## trust signatureHash(symbol)
  comments* = cpsDebug         ## embed comments within the transformation

template cpsLift*() {.pragma.}          ## lift this proc|type
template cpsCall*() {.pragma.}          ## a cps call
template cpsCall*(n: typed) {.pragma.}  ## redirection

type
  NodeFilter* = proc(n: NimNode): NimNode

  Continuation* = concept c
    c.fn is ContinuationProc[Continuation]
    c is ref object
    c of RootObj

  ContinuationProc*[T] = proc(c: T): T {.nimcall.}

  Pair* = tuple
    key: NimNode
    val: NimNode

func cpsHash*(n: NimNode): auto =
  ## hash a symbol to safely disambiguate it from similar nodes
  when cpsTrustSigHash:
    result = signatureHash(n)
  else:
    var h: Hash = 0
    if n.isNil or n.kind == nnkNilLit:
      return h
    expectKind(n, nnkSym)
    if not n.owner.isNil:
      case n.owner.kind
      of nnkSym:
        if n.owner.kind == nnkSym:
          case n.owner.symKind
          of {NimSymKind.low .. NimSymKind.high}:
            h = h !& hash(n.owner.symKind)         # owner kind
            h = h !& cpsHash(n.owner)              # owner
          else:
            # it's, like, not a valid value (24, in my tests)
            #debugEcho ord(n.owner.symKind), " (", $n.owner.symKind, ")"
            discard
      of nnkNilLit:
        discard
      else:
        raise newException(Defect, "expected owner kind of sym or nil")
    h = h !& hash(n.strVal)                    # name
    # ...
    h = h !& hash(n.lineInfoObj.line)          # line
    h = h !& hash(n.lineInfoObj.column)        # column
    #debugEcho n.strVal, " ", n.symKind, " ", ord(n.symKind), " ", n.lineInfoObj.line, " ", n.lineInfoObj.column, " hash ", signatureHash(n)
    result = !$h

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

proc unhide*(n: NimNode): NimNode =
  ## unwrap hidden nodes
  proc unhidden(n: NimNode): NimNode =
    case n.kind
    of nnkHiddenCallConv:
      result = nnkCall.newNimNode(n)
      for child in n.items:
        result.add copyNimTree(unhide child)
    of CallNodes - {nnkHiddenCallConv}:
      # FIXME: a nutty hack to rewrite varargs conversions
      if n.len > 1 and n.last.kind == nnkHiddenStdConv:
        result = copyNimNode(n)
        for index in 0 ..< n.len - 1: # ie, omit last
          result.add copyNimTree(unhide n[index])
        expectKind(n.last.last, nnkBracket)
        for converted in n.last.last.items:
          result.add copyNimTree(unhide converted)
    else:
      discard
  result = filter(n, unhidden)

proc resym*(n: NimNode; sym: NimNode; field: NimNode): NimNode =
  #debugEcho "resym call on ", treeRepr(sym), " into ", repr(field)
  if sym.kind == nnkSym:
    let sig = cpsHash(sym)
    proc resymify(n: NimNode): NimNode =
      if n.kind == nnkSym:
        if n.strVal == sym.strVal:
          when false:
            debugEcho "old ", sym.repr
            debugEcho "old ", getImpl(sym).treeRepr
            debugEcho "old ", cpsHash(sym)
            debugEcho "new ", n.repr
            debugEcho "new ", getImpl(n).treeRepr
            debugEcho "new ", cpsHash(n)
          if cpsHash(n) == sig:
            result = field
          else:
            ##debugEcho "sig ", sym.treeRepr, " hash ", sig, " vs. ",
            ##  n.treeRepr, " hash ", cpsHash(n), " unequal ", field.repr
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
