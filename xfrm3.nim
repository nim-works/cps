
import macros, tables, sets, strutils


type Filterproc = proc(n: NimNode): NimNode 

proc filter(n: NimNode, fn: FilterProc): NimNode =
  result = n.copyNimNode
  for nc in n:
    result.add fn(nc)


proc xfrm(n: NimNode): NimNode =

  var labels: HashSet[string]

  proc mkLabel(s: string): NimNode =
    var i = 1
    while true:
      let l = s & $i
      if l notin labels:
        labels.incl l
        return ident(l)

  proc auxCpsCall(n: NimNode): NimNode =
    if n.kind == nnkStmtList:
      result = n.copyNimNode
      for nc in n:
        result.add nc.filter(auxCpsCall)
        if nc.kind == nnkIdent and nc.strVal.find("cps_") == 0:
          let l = mkLabel "cpscall"
          result.add quote do:
            goto `l`
            label `l`
    else:
      result = n.filter(auxCpsCall)

  proc auxWhile(n: NimNode): NimNode =
    if n.kind == nnkWhileStmt:
      let (expr, body) = (n[0], n[1])
      let (lWhile, lBreak) = (mkLabel "while", mkLabel "break")
      result = quote do:
        goto `lWhile`
        label `lWhile`
        if `expr`:
          `body`
          goto `lWhile`
        goto `lBreak`
        label `lBreak`
    else:
      result = n.filter(auxWhile)


  result = n
  result = result.auxCpsCall()
  result = result.auxWhile()


macro verify(a, b: untyped) =
  let aa = a.xfrm
  if aa.repr == b.repr:
    echo "ok"
  else:
    echo "---------------------------"
    echo aa.treeRepr
    echo "---------------------------"
    echo aa.repr
    echo "---------------------------"
    echo b.repr
    echo "---------------------------"
    quit 1



template goto(_: untyped) = discard
template label(_: untyped) = discard




verify do:
  stmt1
  while exp1:
    stmt2
  stmt3
do:
  stmt1
  goto while1
  label while1
  if exp1:
    stmt2
    goto while1
  goto break1
  label break1
  stmt3


verify do:
  stmt1
  cps_stmt2
  stmt3
do:
  stmt1
  cps_stmt2
  goto cpscall1
  label cpscall1
  stmt3
