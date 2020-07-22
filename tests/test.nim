import std/unittest

import cps
import cps/eventqueue

proc adder(x: var int) =
  inc x

suite "cps":

  setup:
    var cup: int

  test "simple":
    proc foo(): Cont {.cps.} =
      cup = 1
    trampoline foo()
    check cup == 1

  test "yield":
    proc foo(): Cont {.cps.} =
      cps_yield()
    trampoline foo()

  test "sleep":
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 3:
        cps_sleep(i + 1)
        adder(i)
      cup = i
      check cup == 3
    trampoline foo()
