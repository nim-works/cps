import cps
import cps/eventqueue

var r = 0

when true:
  proc test() {.cps:Cont.} =
    r = 1
    while true:
      if true:
        break
      inc r
      if r > 2:
        quit(1)
      return
  trampoline test()
  assert r == 1, "r was " & $r

when true:
  proc test2() {.cps:Cont.} =
    r = 1
    while true:
      cps jield()
      if true:
        break
      inc r
      if r > 2:
        quit(1)
      return
  spawn test2()
  run()
  assert r == 1, "r was " & $r

when true:
  proc test3() {.cps:Cont.} =
    r = 1
    while true:
      cps noop()
      if true:
        inc r
        if r > 2:
          quit(6)
        else:
          break
    inc r
  trampoline test3()
  assert r == 3, "r was " & $r

when true:
  proc test4() {.cps:Cont.} =
    r = 1
    block found:
      while true:
        cps noop()
        if r > 2:
          break found
        cps noop()
        inc r
      quit(1)
    r = r * -1
  trampoline test4()
  assert r == -3, "r was " & $r
