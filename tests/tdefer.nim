import balls
import cps

include preamble

suite "defer statements":

  var r = 0

  block:
    ## a defer statement works across a continuation
    when true:
      skip "not working, see #80"
    else:
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
