
import macros, tables, sets, strutils

const
  whiley = true

func tailCall(n: NimNode): NimNode = nnkReturnStmt.newTree(newCall(n))

func isReturnCall(n: NimNode): bool =
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

  proc mkLabel(s: string): NimNode =
    var i: int
    while true:
      inc i
      let l = s & $i
      if l notin labels:
        labels.incl l
        return ident(l)

  proc callTail(name: NimNode; prc: NimNode): NimNode =
    ## make a tail call into a single statement list
    result = newStmtList()
    result.add tailCall(name)
    if prc.kind == nnkProcDef:
      result.add prc
    else:
      result.add newProc(name = name, body = prc)

  proc callTail(prc: NimNode): NimNode =
    ## make a proc or statement list into a tail call
    if prc.kind == nnkProcDef:
      result = callTail(prc.name, prc)
    else:
      result = callTail(newProc(name = mklabel"tailcall", body = prc))

  var
    x = newStmtList()
  x.add tailCall(ident"goats")

  assert x.isReturnCall, treeRepr(x)

  var y = callTail(ident"pigs", x)
  assert y.isReturnCall, treeRepr(y)

  proc saften(n: NimNode; r: NimNode = nil): NimNode

  proc splitAt(n: NimNode; name: string; i: int; r: NimNode = nil): NimNode =
    # split a statement list to create a tail call given
    # a label prefix and an index at which to split
    let label = mklabel name
    echo "converting tail call " & label.repr
    var body = newStmtList(n[i+1 ..< n.len])
    body = saften(body, r)
    result = newProc(name = label, body = body)

  # Make sure all CPS calls become tail calls
  proc saften(n: NimNode; r: NimNode = nil): NimNode =
    # xfrm the input into a mutually-recursive "cps convertible form".
    result = n.copyNimNode

    var rl, ep: NimNode

    const
      unexiter = {nnkWhileStmt, nnkBreakStmt}
      returner = {nnkIfStmt, nnkBlockStmt, nnkElifBranch, nnkStmtList}

    for i, nc in n.pairs:
      var nc = nc

      # if the child is a cps block (not a call), then prepare
      # an extra call and pass it to the child for its use
      if i < n.len-1:
        if nc.isCpsBlock and not nc.isCpsCall and nc.kind notin unexiter:
          ep = n.splitAt("exit", i, r)

          # tailcalls below will use this new return label
          result.add saften(nc, ep.name)

          when false:
            # add the return label call with the exit proc
            result.add callTail(ep)
          else:
            # just add the exit proc definition
            result.add ep

          # we've completed the split, so we're done here
          break

      case nc.kind
      of nnkBreakStmt:
        # simple break statement
        result.add tailCall(r)
      of nnkWhileStmt:
        let w = mklabel "while"
        var bp = n.splitAt("break", i, r)
        # guys, lemme tell you about where we're goin'
        let (expr, body) = (nc[0], saften(nc[1], bp.name))
        let loop = newStmtList(newIfStmt((expr,
                                          newStmtList(body, tailCall(w)))))
        result.add callTail(w, loop)
        if i < n.len-1:
          loop.add callTail(bp)
          break

      else:
        # add the child normally, with the current exit label
        result.add saften(nc, r)

      # if the child isn't last,
      if i < n.len-1:
        # and it's a cps call,
        if nc.isCpsCall:
          # perform a more typical tailcall split with current return label
          let x = n.splitAt("tailcall", i, r)
          result.add callTail(x)
          # the split is complete
          break

    # add the "return" clause if it exists
    if r != nil:
      if n.kind in returner:
        # XXX: ugly hack; we don't add "break" via this naive method
        if not startsWith(r.repr, "break"):
          # if the block ends with a tailcall, we don't add one
          if not isReturnCall(result[^1]):
            echo "adding return call " & r.repr & " to " & $n.kind
            result.add tailCall(r)

  result = n.saften()

import diffoutput
import diff
macro test(name: static[string], nIn, nExp: untyped) =
  echo "======[ ", name, " ]========="
  let nOut = nIn.xfrm
  if nOut.repr != nExp.repr:
    echo "-- in: --------------------"
    echo nIn.repr
    echo "-- out: -------------------"
    echo nOut.repr
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
