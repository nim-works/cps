import cps
import cps/eventqueue

var r = 0
proc test(): Cont {.cps.} =
  r = 1
  block:
    if true:
      inc r
      break
    quit(1)
  inc r
trampoline test()
if r != 3:
  echo "r was ", r
  quit(1)

proc test2(): Cont {.cps.} =
  r = 1
  block:
    if true:
      cps jield()
      inc r
      break
    quit(1)
  inc r
trampoline test2()
if r != 3:
  echo "r was ", r
  quit(1)
