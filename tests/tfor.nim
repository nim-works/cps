import cps
import cps/eventqueue

var r = 0

when true:
  proc test() {.cps:Cont.} =
    r = 1
    while true:
      for i in 0 .. 3:
        if i == 0:
          continue
        if i > 2:
          break
        r = r + i
      inc r
      if r == 5:
        break
    inc r
  trampoline test()
  assert r == 6, "r is " & $r
