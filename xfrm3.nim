import macros, tables, sets, strutils, sequtils

##[
## To transform a CPC program into CPS-convertible form, the CPC
## translator needs to ensure that every call to a cps function is either
## in tail position or followed by a tail call to another cps function.
]##


const
  comments = true
  whiley = true

  unexiter = {nnkWhileStmt, nnkBreakStmt}
  # if statements are not "returners"; it's elif branches we care about
  returner = {nnkBlockStmt, nnkElifBranch, nnkStmtList}


func tailCall(n: NimNode): NimNode =
  if n.kind == nnkNilLit:
    nnkReturnStmt.newTree(n)
  else:
    nnkReturnStmt.newTree(newCall(n))

func doc(s: string): NimNode =
  #debugEcho s
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
      r = newStmtList([doc"simple return call", n])

func isCpsCall(n: NimNode): bool =
  n.kind == nnkCall and n[0].strVal.find("cps_") == 0

# Every block with calls to CPS procs is a CPS block
func isCpsBlock(n: NimNode): bool =
  if n.isCpsCall:
    result = true
  elif n.kind != nnkProcDef:
    for i, nc in n.pairs:
      #if i < n.len-1:
      result = nc.isCpsBlock
      if result:
        break
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
        if isReturnCall(n[^1]):
          result = newStmtList()
          result.doc "optimized proc into tail call"
          result.add n[^1]
    else:
      assert false, "not a proc"

  proc makeTail(name: NimNode; n: NimNode): NimNode =
    ## make a tail call and put it in a single statement list;
    ## this will always create a tail call proc and call it
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
    else:
      result.doc "created a creepy proc: " & name.repr
      result.add newProc(name = name, body = newStmtList(n))

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

  func next(ns: seq[NimNode]): NimNode =
    if len(ns) == 0:
      newNilLit()
    else:
      ns[^1]

  template withGoto(n: NimNode; body: untyped): untyped =
    add(goto, n)
    try:
      body
    finally:
      assert len(goto) > 0
      discard pop(goto)

  # Make sure all CPS calls become tail calls
  proc saften(n: NimNode): NimNode =
    # xfrm the input into a mutually-recursive "cps convertible form".
    result = n.copyNimNode

    let n = n.stripComments
    #result.doc "saften " & $n.kind & " with returnTo " & $next().returnTo
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
          #echo bp.repr
          result.add saften(nc)
          #echo result.repr
          #assert false, "made it"
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
      else:
        result.doc "adding normal saften child"
        result.add saften(nc)

      # if the child isn't last,
      if i < n.len-1:
        # and it's a cps call,
        if nc.isCpsCall:
          let x = n.splitAt("tailcall", i)
          var simple: NimNode
          if asSimpleReturnCall(x[^1][^1], simple):
            result.doc "possibly unsafe optimization"
            result.add simple
          else:
            result.doc "add a normal tail call"
            result.add callTail(x)
          # the split is complete
          return

    if n.kind in returner:
      if next(goto).kind != nnkNilLit:
        let duh = result.stripComments
        if len(duh) > 0 and isReturnCall(duh[^1]):
          result.doc "omit return call from " & $n.kind
        else:
          result.doc "adding return call to " & $n.kind
          result.add tailCall(next(goto).returnTo)

  result = saften(n)

import diffoutput
import diff
macro test(name: static[string], nIn, nExp: untyped) =
  echo "======[ ", name, " ]========="
  let nOutComments = xfrm(nIn)
  let nOut = nOutComments.stripComments
  if nOut.repr != nExp.repr:
    echo "-- in: --------------------"
    echo nIn.repr
    echo "-- out: -------------------"
    echo nOutComments.repr
    echo "-- expected: --------------"
    echo nExp.repr
    echo "---------------------------"
    writeFile("/tmp/nExp", nExp.treerepr)
    writeFile("/tmp/nOut", nOut.treerepr)
    when compiles(outputUnixDiffStr):
      let diff = newDiff(nOut.repr.splitLines, nExp.repr.splitLines)
      echo outputUnixDiffStr(diff)
    quit 1


# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------


when whiley:
  test "while0":
    while exp1:
      stmt2
  do:
    return while1()
    proc while1() =
      if exp1:
        stmt2
        return while1()


  test "while1":
    stmt1
    stmt2
    while exp1:
      stmt3
  do:
    stmt1
    stmt2
    return while1()
    proc while1() =
      if exp1:
        stmt3
        return while1()


  test "while2":
    stmt1
    stmt2
    while exp1:
      stmt3
      stmt4
    stmt5
    stmt6
  do:
    stmt1
    stmt2
    return while1()
    proc while1() =
      if exp1:
        stmt3
        stmt4
        return while1()
      return break1()
      proc break1() =
        stmt5
        stmt6


  test "while3":
    stmt1
    while exp1:
      cps_call1()
    while exp2:
      cps_call2()
    stmt4
  do:
    stmt1
    return while1()
    proc while1() =
      if exp1:
        cps_call1()
        return while1()
      return break1()
      proc break1() =
        return while2()
        proc while2() =
          if exp2:
            cps_call2()
            return while2()
          return break2()
          proc break2() =
            stmt4

  test "flop":
    while not timeout:
      cps_read()
      if rc <= 0:
        break;
      cps_write()
    reset_timeout();
  do:
    return while1()
    proc while1() =
      if not timeout:
        cps_read()
        return tailcall1()
        proc tailcall1() =
          if rc <= 0:
            return break1()
          cps_write()
          return while1()

      return break1()
      proc break1() =
        reset_timeout()



test "cps1":
  stmt1
  cps_call()
do:
  stmt1
  cps_call()


test "cps2":
  stmt1
  cps_call()
  stmt2
do:
  stmt1
  cps_call()
  return tailcall1()
  proc tailcall1() =
    stmt2

test "cps3":
  stmt1
  cps_call1()
  stmt2
  cps_call2()
  stmt3
do:
  stmt1
  cps_call1()
  return tailcall1()
  proc tailcall1() =
    stmt2
    cps_call2()
    return tailcall2()
    proc tailcall2() =
      stmt3

test "cps4":
  if rc < 0:
    cps_yield()
    rc = 0
  stmt1
do:
  if rc < 0:
    cps_yield()
    return tailcall1()
    proc tailcall1() =
      rc = 0
      return exit1()
  return exit1()
  proc exit1() =
    stmt1

test "cps5":
  block:
    cps_yield()
  rc = 0
do:
  block:
    cps_yield()
    return exit1()
  return exit1()
  proc exit1() =
    rc = 0

test "cps6":
  block:
    cps_yield()
    break
  rc = 0
do:
  block:
    cps_yield()
    return exit1()
  return exit1()
  proc exit1() =
    rc = 0

test "cps7":
  block:
    if rc < 0:
      cps_yield()
      break
  rc = 0
do:
  block:
    if rc < 0:
      cps_yield()
      return exit1()
    return exit1()
  return exit1()
  proc exit1() =
    rc = 0
