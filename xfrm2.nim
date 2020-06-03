
import macros
import os
import nativesockets
import strutils
import eventqueue

# Any call to a proc starting with "cps_" is a CPS call
proc isCpsCall(n: NimNode): bool = 
  n.kind == nnkCall and ($n[0]).find("cps_") == 0

# Every block with calls to CPS procs is a CPS block
proc isCpsBlock(n: NimNode): bool =
  if n.isCpsCall():
    return true
  else:
    for nc in n:
      if isCpsBlock(nc):
        return true

template cpsMagic() {.pragma.}

type NimNodeFilter =
  proc(n: NimNode): NimNode

proc ident(n: NimNode): NimNode =
  n

proc filter(n: NimNode, fn: NimNodeFilter=ident): NimNode =
  result = fn(n)
  if result == nil:
    result = copyNimNode(n)
    for nc in n:
      result.add filter(nc, fn)



# Pre-semcheck transformation

proc doCspPre(n: NimNode): NimNode =

  proc mkLabel(s: string): NimNode =
    #return ident(s)
    return genSym(nskLabel, s)


  proc auxSleep(n: NimNode): NimNode =
    if n.kind == nnkCall and n[0].eqIdent("sleep"):
      result = quote do:
        cps_sleep()
      result.add n[1]

  # Split on 'while' CPS blocks
  proc auxWhile(n: Nimnode, curLabel: NimNode=nil): NimNode =
    if n.kind == nnkWhileStmt and n.isCpsBlock:
      let lWhile = mkLabel("while")
      let lBreak = mkLabel("break")
      let (expr, stmt) = (n[0], auxWhile(n[1], lBreak))
      result = quote do:
        goto `lWhile`
        label `lWhile`
        if `expr`:
          `stmt`
          goto `lWhile`
        goto `lBreak`
        label `lBreak`
    elif n.kind == nnkBreakStmt:
      result = quote do:
        goto `curLabel`
    else:
      result = copyNimNode(n)
      for nc in n:
        result.add auxWhile(nc, curLabel)

  result = n.filter(auxSleep)
  result = result.auxWhile()

  echo "======== pre out"
  echo result.repr
  echo "========"

macro spawnPre(n: untyped) =
  doCspPre(n)


# Post-semcheck transformation

proc doCspPost(n: NimNode): NimNode =
  result = n.filter()
  echo "======== post out"
  echo result.repr
  echo "========"

macro spawnPost(n: untyped) =
  doCspPost(n)

template goto(n: untyped) = discard
template label(n: untyped) = discard

proc cps_sleep(f: float) {.cpsMagic.} =
  os.sleep(int(f * 1000))
  discard


proc main() =
  echo "main start"
  var i = 0
  spawnPre:
    echo "spawnPre start"
    var j = 100
    while true:
      inc i
      inc j
      if i < 3:
        echo i, " ", j
      else:
        break
      sleep(0.3)
    #writeStackTrace()
    echo "spawnPre done"
  echo "main done"

main()
