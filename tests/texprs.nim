include preamble

import killer

suite "expression flattening":
  test "flatten expression list in var/let":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      let
        x = (noop(); step 1; 42)
        y = (noop(); step 2; x)

      check x == y

      var (a, b) = (noop(); step 3; (10, y))
      check a == 10
      check b == y

    foo()

  test "flatten block expression":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1

      let x = block:
        noop()
        step 2
        42

      step 3
      check x == 42

    foo()

  test "flatten if expression":
    var k = newKiller(5)
    proc foo() {.cps: Cont.} =
      step 1

      let x =
        if true:
          noop()
          step 2
          42
        elif true:
          fail "this branch should not run"
          0 # needed because the compiler doesn't recognize fail as noreturn
        else:
          fail "this branch should not run"
          -1 # needed because the compiler doesn't recognize fail as noreturn

      step 3
      check x == 42

      let y =
        if false:
          fail "this branch should not run"
          -1
        elif false:
          fail "this branch should not run"
          0
        else:
          noop()
          step 4
          30

      step 5
      check y == 30

    foo()
