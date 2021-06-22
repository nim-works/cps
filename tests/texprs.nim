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
