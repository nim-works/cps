import std/macros
import std/unittest

import cps
import cps/eventqueue

proc adder(x: var int) =
  inc x

suite "cps":
  var cup: int

  test "1":
    proc foo(): Cont {.cps.} =
      cup = 1
    run foo()
    check cup == 1

  test "2":
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 2:
        adder(i)
      cup = i
    run foo()
    check cup == 2

  test "3":
    proc foo(): Cont {.cps.} =
      var i = 0
      while i < 3:
        cps_sleep(1000)
        adder(i)
      cup = i
    run foo()
    check cup == 3

when false:
  test "4":
    cps Cont:
      var i: int = 0
      while i < 4:
        adder(i)
      cup = i
    check cup == 4
