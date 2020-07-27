import std/unittest

import cps
import cps/eventqueue

proc adder(x: var int) =
  inc x

suite "cps":

  setup:
    var cup: int

  test "while":
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 2:
        let x: int = i
        adder(i)
        assert x < i
        check x < i
      cup = i
      check cup == 2
    trampoline foo()

  test "continue":
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 2:
        let x: int = i
        adder(i)
        if x == 0:
          continue
        assert x > 0
      cup = i
      check cup == 2
    trampoline foo()
