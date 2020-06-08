
import macros, tables, sets, strutils


proc isCpsCall(n: NimNode): bool = 
  n.kind == nnkCall and n[0].strVal.find("cps_") == 0

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

  proc tailCall(n: NimNode): NimNode =
    nnkReturnStmt.newTree(
      newCall(n)
    )
  
  # Make sure all CPS calls become tail calls
  proc auxCpsCall(n: NimNode): NimNode =
    result = n.copyNimNode
    for i, nc in n:
      result.add auxCpsCall(nc)
      if nc.isCpscall() and i < n.len-1:
        echo "-- converting CPS call to tail call"
        let lNext = mklabel "tailcall"
        let nBody = newStmtList()
        let nNext = auxCpsCall(newStmtList(n[i+1..<n.len]))
        result.add tailCall(lNext)
        result.add newProc(name=lNext, body=nNext)
        break


  # Make sure all CPS calls become tail calls
  proc auxCpsCall2(n: NimNode): NimNode =
    if n.kind == nnkStmtList:
      result = n.copyNimNode
      var blk: NimNode
      for i, nc in n:
        if blk == nil:
          result.add auxCpsCall(nc)
          if nc.isCpsCall() and i < n.len-1:
            let l = mkLabel "cpscall"
            blk = nnkBlockStmt.newTree(l)
            echo "-- inserting tailcall/proc after ", nc[0].strVal, ": ", l
            result.add nnkCommand.newTree(ident("goto"), l)
            result.add blk
        else:
          blk.add auxCpsCall(nc)
    else:
      result = n.copyNimNode
      for nc in n:
        result.add auxCpsCall(nc)

  # Convert while to if + tail calls
  proc auxWhile(n: NimNode, lBreak: NimNode=nil): NimNode =

    if n.kind == nnkBreakStmt:
      echo "-- converting break to ", lBreak.repr, "()"
      return tailCall(lBreak)

    result = n.copyNimNode
    for i, nc in n:
      if nc.kind == nnkBreakStmt:
        echo "-- converting break to ", lBreak.repr, "()"
        result.add tailCall(lBreak)
      elif nc.kind == nnkWhileStmt:
        let lBreak = mkLabel "break"
        echo "-- converting 'while ", nc[0].repr, "'"
        let lWhile = mkLabel "while"
        let (expr, body) = (nc[0], auxWhile(nc[1], lBreak))
        let nBody = newStmtList(newIfStmt((expr, newStmtList(body, tailCall(lWhile)))))
        result.add tailCall(lWhile)
        result.add newProc(name=lWhile, body=nBody)
        if i < n.len-1:
          nBody.add tailCall(lBreak)
          let nBreak = newStmtList()
          nBody.add newProc(name=lBreak, body=nBreak)
          nBreak.add auxWhile(newStmtList(n[i+1..<n.len]), lBreak)
          break
      else:
        result.add auxWhile(nc, lBreak)


  result = n
  result = result.auxCpsCall()
  result = result.auxWhile()


macro test(name: static[string], nIn, nExp: untyped) =
  echo "======[ ", name, " ]========="
  let nOut = nIn.xfrm
  if nOut.repr == nExp.repr:
    echo "ok  "
  else:
    echo "-- in: --------------------"
    echo nIn.repr
    echo "-- out: -------------------"
    echo nOut.repr
    echo "-- expected: --------------"
    echo nExp.repr
    echo "---------------------------"
    writeFile("/tmp/nExp", nExp.treerepr)
    writeFile("/tmp/nOut", nOut.treerepr)
    quit 1


# ----------------------------------------------------------------------
# Unit tests
# ----------------------------------------------------------------------


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
    stmt2
  while exp2:
    stmt3
  stmt4
do:
  stmt1
  return while1()
  proc while1() =
    if exp1:
      stmt2
      return while1()
    return break1()
    proc break1() =
      return while2()
      proc while2() =
        if exp2:
          stmt3
          return while2()
        return break2()
        proc break2() =
          stmt4
    


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

test "4":
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

