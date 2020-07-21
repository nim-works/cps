import cps
import cps/eventqueue

proc adder(x: var int) =
  inc x

var cup: int

when false:
  block:
    proc foo(): Cont {.cps.} =
      cup = 1
    trampoline foo()
    assert cup == 1

  block:
    proc foo(): Cont {.cps.} =
      cps_yield()
    trampoline foo()

  block:
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 2:
        adder(i)
      cup = i
      assert cup == 2
    trampoline foo()

block:
  proc foo(): Cont {.cps.} =
    var i: int = 0
    while i < 3:
      cps_sleep(100)
      adder(i)
    cup = i
    assert cup == 3
  trampoline foo()

when false:
  block:
    cps Cont:
      var i: int = 0
      while i < 4:
        adder(i)
      cup = i
    assert cup == 4

run()
