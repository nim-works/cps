import cps
import cps/eventqueue

var r = 0

when true:
  proc test(): Cont {.cps.} =
    r = 1
    while true:
      if true:
        break
      inc r
      if r > 2:
        quit(1)
      return
    quit(2)
  discard test()
  assert r == 1

  proc test2(): Cont {.cps.} =
    r = 1
    while true:
      cps jield()
      if true:
        break
      inc r
      if r > 2:
        quit(1)
      return
    quit(2)
  trampoline test2()
  assert r == 1

when true:
  proc test3(): Cont {.cps.} =
    r = 1
    while true:
      cps jield()
      if true:
        inc r
        if r > 2:
          quit(1)
        else:
          break
  spawn test3()
  run()
  assert r == 3
