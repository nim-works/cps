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
      cps jield()
    trampoline foo()

  test "noop":
    var j = 2
    proc foo(): Cont {.cps.} =
      var i: int = 3
      j = 4
      cps noop()
      check i == 3
    trampoline foo()
    check j == 4

  test "sleep":
    proc foo(): Cont {.cps.} =
      var i: int = 0
      while i < 3:
        cps sleep(i + 1)
        adder(i)
      cup = i
      check cup == 3
    trampoline foo()

  test "https://github.com/disruptek/cps/issues/16":
    proc foo(): Cont {.cps.} =
      var i, j, k: int = 0
      j = 5
      var p: int
      var q: int = 0
      var r: int = j
      cps jield()
      inc i
      inc j
      inc k
      inc p
      inc q
      inc r
    trampoline foo()

when false:
  test "https://github.com/disruptek/cps/issues/15":
    proc foo(): Cont {.cps.} =
      var (i, j, k) = (1, 2, 3)
      cps noop()
      check i == 1
      check j == 2
      check k == 3
    trampoline foo()
