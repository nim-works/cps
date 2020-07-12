import std/macros
import std/tables
import std/sets
import std/strutils
import std/sequtils

##[
## To transform a CPC program into CPS-convertible form, the CPC
## translator needs to ensure that every call to a cps function is either
## in tail position or followed by a tail call to another cps function.
]##


const
  comments = true

  unexiter = {nnkWhileStmt, nnkBreakStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkElse, nnkStmtList}


func tailCall(n: NimNode): NimNode =
  if n.kind == nnkNilLit:
    nnkReturnStmt.newTree(n)
  else:
    nnkReturnStmt.newTree(newCall(n))

func doc(s: string): NimNode =
  when comments:
    newCommentStmtNode(s)
  else:
    newEmptyNode()

proc doc(n: var NimNode; s: string) =
  when comments:
    if n.kind == nnkStmtList:
      n.add doc(s)

func stripComments(n: NimNode): NimNode =
  result = n.copyNimNode
  for child in items(n):
    if child.kind != nnkCommentStmt:
      result.add child.stripComments

func returnTo(n: NimNode): NimNode =
  ## take a `return foo()` or (proc foo() = ...) and yield `foo`
  let n = n.stripComments
  case n.kind
  of nnkProcDef:
    result = n.name
  of nnkCall:
    result = n[0]
  of nnkIdent:
    result = n
  of nnkNilLit:
    result = n
  else:
    result = returnTo(n[0])

func isReturnCall(n: NimNode): bool =
  let n = n.stripComments
  case n.kind
  # simple `return foo()`
  of nnkReturnStmt:
    if n.len > 0:
      if n[0].kind == nnkCall:
        result = true
  # `return foo(); proc foo() = ...`
  of nnkStmtList:
    result = case n.len
    of 1:
      n[0].isReturnCall
    of 2:
      n[0].isReturnCall and n[1].kind == nnkProcDef and n[1].name == n[0][0][0]
    else:
      false
  else: discard

func asSimpleReturnCall(n: NimNode; r: var NimNode): bool =
  ## fill `r` with `return foo()` if that is a safe simplification
  var n = n.stripComments
  block done:
    while n.kind == nnkStmtList:
      if len(n) != 1:
        break done
      n = n[0]
    result = isReturnCall(n)
    if result:
      r = newStmtList([doc "simple return call: " & n.repr, n])

func isCpsCall(n: NimNode): bool =
  result = n.kind == nnkCall and n[0].strVal.find("cps_") == 0
  #debugEcho treeRepr(n)

# Every block with calls to CPS procs is a CPS block
func isCpsBlock(n: NimNode): bool =
  case n.kind
  of nnkProcDef, nnkElse, nnkElifBranch:
    result = isCpsBlock(n.last)
  #of nnkBreakStmt:
  #  result = insideCps()
  of nnkIfStmt:
    result = any(toSeq(n.children), isCpsBlock)
  of nnkStmtList:
    for i, nc in pairs(n):
      result = (i != n.len-1 and nc.isCpsCall) or nc.isCpsBlock
      if result:
        break
  else:
    discard

  when false:
    if result:
      debugEcho "CPS BLOCK ", n.repr
    else:
      debugEcho "NAH BLOCK ", n.repr


proc xfrm(n: NimNode): NimNode =

  var labels: HashSet[string]

  # identifiers of future break or return targets
  var goto: seq[NimNode]
  var breaks: seq[NimNode]

  func insideCps(): bool = len(goto) > 0 or len(breaks) > 0

  proc mkLabel(s: string): NimNode =
    var i: int
    while true:
      inc i
      let l = s & $i
      if l notin labels:
        labels.incl l
        return ident(l)

  proc foldTailCalls(n: NimNode): NimNode =
    ## this may optimize a `proc foo() = return bar()` to `return bar()`
    result = n
    if n.kind == nnkProcDef:
      if not asSimpleReturnCall(n, result):
        if isReturnCall(n.last):
          result = newStmtList()
          result.doc "optimized proc into tail call"
          result.add n.last
    else:
      assert false, "not a proc"

  template cpsLift() {.pragma.}

  proc makeTail(name: NimNode; n: NimNode): NimNode =
    ## make a tail call and put it in a single statement list;
    ## this will always create a tail call proc and call it
    let
      lifter = bindSym"cpsLift"
    result = newStmtList()
    result.doc "new tail call: " & name.repr
    result.add tailCall(name)
    if n.kind == nnkProcDef:
      result.doc "adding the proc verbatim"
      result.add n
    elif n.kind == nnkStmtList:
      if len(n) == 0:
        {.warning: "creating an empty tail call".}
      result.doc "creating a new proc: " & name.repr
      result.add newProc(name = name, body = n)
      result[^1].addPragma lifter
    else:
      result.doc "created a creepy proc: " & name.repr
      result.add newProc(name = name, body = newStmtList(n))
      result[^1].addPragma lifter

  proc returnTail(name: NimNode; n: NimNode): NimNode =
    ## either create and return a tail call proc, or return nil
    if len(n) == 0:
      # no code to run means we just `return nil`
      result = nnkReturnStmt.newNimNode(newNilLit())
    else:
      # create a tail call with the given body
      result = makeTail(mklabel"tailcall", n)

  proc callTail(n: NimNode): NimNode =
    ## given a node, either turn it into a `return call(); proc call() = ...`
    ## or optimize it into a `return subcall()`
    case n.kind
    of nnkProcDef:
      # if you already put it in a proc, we should just use it
      result = n
    of nnkIdent:
      # if it's an identifier, we'll just issue a call of it
      result = tailCall(n)
    of nnkStmtList:
      # maybe we can optimize it out
      if asSimpleReturnCall(n, result):
        discard "the call was stuffed into result"
      elif isReturnCall(n):
        # just copy the call
        result = newStmtList([doc"verbatim tail call", n])
      else:
        if len(n) == 0:
          # no code to run means we just `return nil`
          result = nnkReturnStmt.newNimNode(newNilLit())
        else:
          # create a tail call and, uh, call it
          result = returnTail(mklabel"tailcall", n)
    else:
      # wrap whatever it is and recurse on it
      result = callTail(newStmtList(n))

  when false:
    var x = newStmtList()
    var z: NimNode
    x.add tailCall(ident"goats")
    assert x.isReturnCall, treeRepr(x)
    assert x.asSimpleReturnCall(z), treeRepr(x)

    var y = makeTail(ident"pigs", x)
    assert y.isReturnCall, treeRepr(y)
    assert not y.asSimpleReturnCall(z), treeRepr(y)

  proc saften(n: NimNode): NimNode

  proc splitAt(n: NimNode; name: string; i: int): NimNode =
    # split a statement list to create a tail call given
    # a label prefix and an index at which to split
    let label = mklabel name
    var body = newStmtList()
    body.doc "split as " & label.repr & " at index " & $i
    if i < n.len-1:
      body.add n[i+1 ..< n.len]
      body = saften(body)
      result = makeTail(label, body)
    else:
      result = callTail(newStmtList())

  func next(ns: seq[NimNode]): NimNode =
    if len(ns) == 0:
      newNilLit()
    else:
      ns[^1]

  template withGoto(n: NimNode; body: untyped): untyped =
    if len(n.stripComments) > 0:
      add(goto, n)
      try:
        body
      finally:
        discard pop(goto)
    else:
      body

  proc optimizeSimpleReturn(into: var NimNode; n: NimNode) =
    var simple: NimNode
    var n = n.stripComments
    if asSimpleReturnCall(n.last.last, simple):
      into.doc "possibly unsafe optimization: " & n.repr
      optimizeSimpleReturn(into, simple)
    else:
      into.doc "add a normal tail call; not " & n.repr
      into.add callTail(n)

  proc liften(n: var NimNode): NimNode =
    let lifter = bindSym"cpsLift"
    result = newStmtList()
    var dad = n.copyNimNode
    for kid in items(n):
      var kid = kid
      for k in liften(kid):
        add(result, k)
      if kid.kind == nnkProcDef and lifter in toSeq(kid.pragma):
        add(result, kid)
      else:
        add(dad, kid)
    n = dad

  # Make sure all CPS calls become tail calls
  proc saften(n: NimNode): NimNode =
    # xfrm the input into a mutually-recursive "cps convertible form".
    result = n.copyNimNode

    let n = n.stripComments
    result.doc "saften $1 with $2 gotos and $3 breaks" %
      [ $n.kind, $len(goto), $len(breaks) ]

    for i, nc in pairs(n):
      # if the child is a cps block (not a call), then push a tailcall
      # onto the stack during the saftening of the child
      if i < n.len-1:
        if nc.kind notin unexiter and nc.isCpsBlock and not nc.isCpsCall:
          withGoto n.splitAt("exit", i):
            result.add saften(nc)
            result.doc "add the exit proc definition"
            result.add next(goto)

            # we've completed the split, so we're done here
            return

      case nc.kind
      of nnkBreakStmt:
        if len(breaks) > 0:
          result.doc "simple break statement"
          result.add tailCall(next(breaks).returnTo)
        else:
          result.doc "no break statements to pop"

      of nnkBlockStmt:
        let bp = n.splitAt("break", i)
        add(breaks, bp)
        try:
          result.add saften(nc)
          if i < n.len-1:
            result.doc "add tail call for block-break proc"
            result.add callTail(next(breaks))
            return
        finally:
          discard pop(breaks)

      of nnkWhileStmt:
        let w = mklabel "while"
        let bp = n.splitAt("break", i)
        add(breaks, bp)
        add(goto, w)
        try:
          var loop = newStmtList()
          result.doc "add tail call for while loop"
          result.add makeTail(w, loop)
          # guys, lemme tell you about where we're goin'
          let (expr, body) = (nc[0], saften(nc[1]))
          loop.add newIfStmt((expr, newStmtList(body)))
          discard pop(goto)
          if i < n.len-1:
            loop.doc "add tail call for break proc"
            loop.add callTail(next(breaks))
            return
        finally:
          discard pop(breaks)

      of nnkIfStmt:
        # if any `if` clause is a cps block, then every clause must be
        # if we've pushed any goto or breaks, then we're already in cps
        if nc.isCpsBlock:
          let x = n.splitAt("if", i)
          withGoto x:
            result.add saften(nc)
            if len(x.stripComments) > 0:
              result.add next(goto)
          return
        elif insideCps():
          result.add saften(nc)
        else:
          result.add nc

      else:
        result.doc "adding normal saften child " & $nc.kind
        result.add saften(nc)

      # if the child isn't last,
      if i < n.len-1:
        # and it's a cps call,
        if nc.isCpsCall or nc.isCpsBlock:
          let x = n.splitAt("tailcall", i)
          optimizeSimpleReturn(result, x)
          # the split is complete
          return

    if n.kind in returner:
      if next(goto).kind != nnkNilLit:
        let duh = result.stripComments
        if len(duh) > 0 and isReturnCall(duh.last):
          result.doc "omit return call from " & $n.kind
        else:
          result.doc "adding return call to " & $n.kind
          result.add tailCall(next(goto).returnTo)
      else:
        result.doc "nil return"

  result = newStmtList(saften(n))
  let decls = liften(result)
  result = newStmtList(decls, result)
  echo repr(result)

macro cps*(n: untyped) =
  assert n.kind == nnkProcDef
  result = n
  result[^1] = xfrm(result[^1])
