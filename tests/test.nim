import cps
import cps/eventqueue


#import testes
#testes:
import std/unittest
suite "cps":
  proc adder(x: var int) =
    inc x

  var cup: int

  test "simple":
    proc foo() {.cps:Cont.} =
      cup = 1
    trampoline foo()
    check cup == 1

  test "yield":
    proc foo() {.cps:Cont.} =
      yield jield()
    trampoline foo()

  test "noop":
    var noopJ = 2
    proc foo66() {.cps:Cont.} =
      var i: int = 3
      noopJ = 4
      cps noop()
      check i == 3
    trampoline foo66()
    check noopJ == 4

  test "sleep":
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 3:
        cps sleep(i + 1)
        adder(i)
      cup = i
      check cup == 3
    trampoline foo()

  test "https://github.com/disruptek/cps/issues/22":
    skip()
    when false:
      proc foo(a, b, c: int = 3) {.cps: Cont.} =
        var a: int = 5
        cps noop()
        var b: int = b + a
        cps noop()
        check:
          a == 5
          b == 7
          c == 3
      trampoline foo(1, 2)

  test "https://github.com/disruptek/cps/issues/22 (2nd)":
    proc foo2(a, b, c: var int) {.cps: Cont.} =
      a = 5
      cps noop()
      b = b + a
      cps noop()
      check:
        a == 5
        b == 7
        c == 3
    var (x, y, z) = (1, 2, 3)
    trampoline foo2(x, y, z)

  test "https://github.com/disruptek/cps/issues/16":
    proc foo() {.cps: Cont.} =
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
    proc foo() {.cps: Cont.} =
      var (i, j, k) = (1, 2, 3)
      cps noop()
      check i == 1
      check j == 2
      check k == 3
    trampoline foo()
