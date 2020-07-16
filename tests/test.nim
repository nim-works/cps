import std/macros
import std/unittest

import cps
import eventqueue

proc adder(x: var int) =
  inc x

suite "cps":
  var cup: int

  test "1":
    proc foo(): Cont {.cps.} =
      cup = 1
    discard foo()
    check cup == 1

  test "2":
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 2:
        adder(i)
    discard foo()
    check cup == 2

  test "3":
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 3:
        yield
        adder(i)
    discard foo()
    check cup == 3
