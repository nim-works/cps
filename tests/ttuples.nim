import balls
import cps

include preamble

suite "tuples":

  var r = 0

  block:
    ## tuple deconstruction with a call source works
    r = 0
    proc bar(): (int, float) = (4, 7.0)

    proc foo() {.cps: Cont.} =
      inc r
      let (i, k) = bar()
      noop()
      inc r
      noop()
      check "declared variables":
        i == 4
        k == 7.0
    foo()
    check r == 2

  block:
    ## tuple deconstruction with distinct types works
    type
      Goats = distinct int
      Pigs = distinct float
    proc `==`(a, b: Goats): bool {.borrow.}
    proc `==`(a, b: Pigs): bool {.borrow.}
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let (i, k) = (Goats 4, Pigs 7.0)
      noop()
      inc r
      noop()
      check "declared variables":
        i == 4.Goats
        k == Pigs 7.0
    foo()
    check r == 2

  block:
    ## declaration via tuple deconstruction works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var (i, k) = (1, 3)
      let (x, z) = (4, 6)
      noop()
      inc r
      inc k
      noop()
      check "declared variables":
        i == 1
        k == 4
        x == 4
        z == 6
    foo()
    check r == 2

  block:
    ## multi-variable declaration with shared initialization
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i, k = 3
      let x, z = 4
      noop()
      inc r
      inc k
      noop()
      check "declared variables":
        i == 3
        k == 4
        x == 4
        z == 4
    foo()
    check r == 2

