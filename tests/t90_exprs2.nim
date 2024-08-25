include preamble

suite "expression flattening":
  test "flatten case matching expression":
    var k = newKiller(4)
    proc foo() {.cps: Cont.} =
      step 1

      case (noop(); step 2; "string")
      of "str":
        fail "This branch should not be run"
      of "string":
        step 3
      else:
        fail "This branch should not be run"

      step 4

    foo()
    check k

  test "flatten case elif branches":
    var k = newKiller(4)
    proc foo() {.cps: Cont.} =
      step 1

      case "string"
      of "str":
        fail "This branch should not be run"
      of "String":
        fail "This branch should not be run"
      elif (noop(); step 2; true):
        step 3
      else:
        fail "This branch should not be run"

      step 4

    foo()
    check k

  test "flatten while condition":
    var k = newKiller(4)
    proc foo() {.cps: Cont.} =
      step 1

      var x = 2
      while (noop(); step x; inc x; x < 4):
        discard

      step 4

    foo()
    check k

  test "flatten assignments with LHS being a symbol":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      var x: int
      x =
        if true:
          noop()
          step 2
          42
        else:
          fail "this branch should not be run"
          -1

      step 3

      check x == 42

    foo()
    check k

  test "flatten assignments with LHS being an object access":
    type
      A = object
        i: int
      O = object
        a: A

    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      var o: O
      o.a.i =
        if true:
          noop()
          step 2
          42
        else:
          fail "this branch should not be run"
          -1

      step 3

      check o.a.i == 42

    foo()
    check k

  test "flatten assignments with LHS being a ref access from immutable location":
    type
      A = object
        i: int
      O = ref object
        a: A

    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      let o = O()
      o.a.i =
        if true:
          noop()
          step 2
          42
        else:
          fail "this branch should not be run"
          return

      step 3

      check o.a.i == 42

    foo()
    check k
