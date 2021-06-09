import std/macros
import std/strutils
import balls
import cps

include preamble
import killer

suite "returns and results":

  block:
    ## local assignment to a continuation return value
    skip"pending discussion #28":
      var k = newKiller 3
      proc bar(a: int): int {.cps: Cont.} =
        noop()
        step 2
        return a * 2

      proc foo() {.cps: Cont.} =
        step 1
        let x = int bar(4)
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
    skip "not a thing yet"
    var k = newKiller 1
    proc foo(x: int): int {.cps: Cont.} =
      noop()
      step 1
      return x * x

    var c = whelp foo(5)
    trampoline c

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
    ## naked returns in continuations are fine
    var k = newKiller 1
    proc foo() {.cps: Cont.} =
      noop()
      step 1
      return

    foo()
