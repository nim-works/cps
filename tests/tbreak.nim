import cps
import cps/eventqueue

var r = 0
proc test(): Cont {.cps.} =
  r = 1
  while true:
    #cps jield()
    if true:
      break
    inc r
    if r > 2:
      quit(1)
    return
  quit(2)
discard test()
assert r == 2
