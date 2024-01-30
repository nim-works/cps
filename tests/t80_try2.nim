import std/strutils

include preamble

from cps/spec import cpsStackFrames

suite "defer statements":

  var r = 0

  block:
    ## a defer statement works across a continuation
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        check r == 2, "defer run before end of scope"
        inc r

      inc r
      noop()
      inc r

    foo()
    check r == 3

  block:
    ## a basic defer statement is supported
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      defer:
        check r == 4
        inc r
      inc r
      defer:
        check r == 3
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## a defer in a nested template is supported
    r = 0

    template deferChk(i: int) =
      inc r
      defer:
        check r == i
        inc r

    proc foo() {.cps: Cont.} =
      deferChk(5)
      inc r
      deferChk(4)
      inc r

    foo()
    check r == 6

  block:
    ## a defer inside a block statement works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        defer:
          check r == 2
          inc r
        inc r
      defer:
        check r == 4
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## a naked defer is not a problem
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        inc r

    foo()
    check r == 1


when defined(gcArc) or defined(gcOrc):
  suite "breaking deterministic memory managers":
    block:
      ## try-except-statement splits
      proc foo() {.cps: Cont.} =
        var k = newKiller(3)
        step 1
        try:
          noop()
          step 2
        except CatchableError:
          fail "this branch should not run"
        step 3

      foo()

    block:
      ## try-except splits with raise
      proc foo() {.cps: Cont.} =
        var k = newKiller(4)
        step 1
        try:
          noop()
          step 2
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except CatchableError:
          step 3
        step 4

      foo()

    block:
      ## try-finally-statement splits
      proc foo() {.cps: Cont.} =
        var k = newKiller(4)
        step 1
        try:
          noop()
          step 2
        finally:
          step 3
        step 4

      foo()

    block:
      ## try-except-finally splits with raise
      proc foo() {.cps: Cont.} =
        var k = newKiller(5)
        step 1
        try:
          noop()
          step 2
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except CatchableError:
          step 3
        finally:
          step 4
        step 5

      foo()
