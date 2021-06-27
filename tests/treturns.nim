import std/macros
import std/strutils
import balls
import cps

include preamble
import killer

suite "returns and results":

  block:
    ## local assignment to a continuation return value
    var k = newKiller 3
    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 2
      return a * 2

    proc foo() {.cps: Cont.} =
      step 1
      let x = bar(4)
      step 3
      check x == 8

    foo()

  block:
    ## continuations can return values via bootstrap
    var k = newKiller 1
    proc foo(x: int): int {.cps: Cont.} =
      noop()
      step 1
      return x * x

    let x = foo(3)
    check x == 9

  block:
    ## continuations can return values via whelp
    var k = newKiller 1
    proc foo(x: int): int {.cps: Cont.} =
      noop()
      step 1
      return x * x

    var c = whelp foo(5)
    trampoline c
    check "dots work correctly":
      ... c == 25

  block:
    ## assignments to the special result symbol work
    block:
      var k = newKiller 1
      proc foo(x: int): int {.cps: Cont.} =
        noop()
        step 1
        result = x * x

      let x = foo(3)
      check x == 9

    block:
      var k = newKiller 1
      proc foo(x: int): int {.cps: Cont.} =
        noop()
        step 1
        result = x * x

      var c = whelp foo(5)
      trampoline c

  block:
    ## naked returns in continuations with a complication are fine
    var k = newKiller 1
    proc foo() {.cps: Cont.} =
      noop()
      step 1
      if true:
        return
      step 2

    foo()

  block:
    ## dismissing a child continuation is fun
    var k = newKiller 2
    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 2
      result = a * 2
      dismiss()

    proc foo() {.cps: Cont.} =
      step 1
      let x = bar(4)
      step 3
      check x == 8

    foo()

  block:
    ## assignment to a continuation return value
    var k = newKiller 3
    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 2
      return a * 2

    proc foo() {.cps: Cont.} =
      step 1
      var x: int
      x = bar(4)
      step 3
      check x == 8

    foo()
  
  block:
    ## local assignement tuple unpacking a continution return value
    var k = newKiller 3
    proc bar(): (int, int) {.cps: Cont.} =
      noop()
      step 2
      return (1, 2)

    proc foo(): int {.cps: Cont.} =
      step 1
      let (a, b) = bar()
      step 3
      check a == 1
      check b == 2
      (a + b) * 2

    var c = whelp foo()
    trampoline c
    check "dots work correctly":
      ... c == 6
