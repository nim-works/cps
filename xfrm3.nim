
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

  # Insert goto/label pair after CPS calls
  proc auxCpsCall(n: NimNode): NimNode =
    if n.kind == nnkStmtList:
      result = n.copyNimNode
      var blk: NimNode
      for i, nc in n:
        if blk == nil:
          result.add auxCpsCall(nc)
          if nc.isCpsCall() and i < n.len-1:
            let l = mkLabel "cpscall"
            blk = nnkBlockStmt.newTree(l)
            echo "-- inserting goto/block after ", nc[0].strVal, ": ", l
            result.add nnkCommand.newTree(ident("goto"), l)
            result.add blk
        else:
          blk.add auxCpsCall(nc)
    else:
      result = n.copyNimNode
      for nc in n:
        result.add auxCpsCall(nc)

  # Convert while to if + gotos
  proc auxWhile(n: NimNode): NimNode =

    var nBreak: NimNode
    result = n.copyNimNode

    for i, nc in n:
      if nc.kind == nnkWhileStmt:
        echo "-- converting 'while ", nc[0].repr, "'"
        let (lWhile, lBreak) = (mkLabel "while", mkLabel "break")
        let (expr, body) = (nc[0], nc[1])
        let nBody = quote do:
          if `expr`:
            `body`
            `lWhile`()

        result.add quote do:
          `lWhile`()
          proc `lWhile`() =
            `nBody`

        if i < n.len-1:
          echo "notlast"
          nBody.add quote do:
            `lBreak`()
          nBreak = quote do:
            echo "break"
          result.add quote do:
            proc `lBreak`() =
              `nBreak`

      else:

        if nBreak == nil:
          result.add auxWhile(nc)
        else:
          echo "addbrk ", nc.repr
          nBreak.add auxWhile(nc)


  result = n
  result = result.auxCpsCall()
  result = result.auxWhile()


macro test(name: static[string], a, b: untyped) =
  echo "======[ ", name, " ]========="
  let aa = a.xfrm
  if aa.repr == b.repr:
    echo "ok  "
  else:
    echo "---------------------------"
    echo a.treeRepr
    echo "-- got: -------------------"
    echo aa.repr
    echo "-- expectd: ---------------"
    echo b.repr
    echo "---------------------------"
    writeFile("/tmp/a", aa.repr)
    writeFile("/tmp/b", b.repr)
    quit 1



template goto(_: untyped) = discard
template label(_: untyped) = discard


test "while0":
  while exp1:
    stmt2
do:
  while1()
  proc while1() =
    if exp1:
      stmt2
      while1()


test "while1":
  stmt1
  stmt2
  while exp1:
    stmt3
do:
  stmt1
  stmt2
  while1()
  proc while1() =
    if exp1:
      stmt3
      while1()
    


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
  while1()
  proc while1() =
    if exp1:
      stmt3
      stmt4
      while1()
    break1()
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
  while1()
  proc while1() =
    if exp1:
      stmt2
      while1()
    break1()
  proc break1() =
    while2()
    proc while2() =
      if exp2:
        stmt3
        while2()
      break2()
    proc break2() =
      stmt3
    


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
  goto cpscall1
  block cpscall1:
    stmt2


test "flop":
  while not timeout:
    cps_read()
    if rc <= 0:
      break;
    cps_write()
  reset_timeout();
do:
  goto while1
  block while1:
    if not timeout:
      cps_read()
      goto cpscall1
      label cpscall1
      if rc <= 0:
        goto break1
      cps_write()
      goto cpscall2
      label cpscall2
      goto while1
  goto break1
  label break1
  reset_timeout()

test "4":
  if rc < 0:
    cps_yield()
    rc = 0
  stmt1
do:
  if rc < 0:
    cps_yield()
    goto cpscall1
    label cpscall1
    rc = 0
  stmt1

