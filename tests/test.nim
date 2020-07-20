import cps
import cps/eventqueue

proc adder(x: var int) =
  inc x

var cup: int

when false:
  block:
    proc foo(): Cont {.cps.} =
      cup = 1
    run foo()
    assert cup == 1


  block:
    proc foo(): Cont {.cps.} =
      cps_yield()
    run foo()

when false:
  block:
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 2:
        adder(i)
      cup = i
    run foo()
    assert cup == 2

when true:
  block:
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 3:
        cps_sleep(1000)
        adder(i)
      cup = i
    run foo()
    assert cup == 3

when false:
  block:
    cps Cont:
      var i: int = 0
      while i < 4:
        adder(i)
      cup = i
    assert cup == 4

run()
