import cps
import cps/eventqueue

var r = 0
proc test(): Cont {.cps.} =
  r = 1
  while true:
    cps jield()
    if true:
      break
    r = 2
    return
spawn test()
run()
assert r == 1
