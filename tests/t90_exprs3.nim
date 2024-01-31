include preamble

suite "expression flattening":
  test "flatten unpacking assignments":
    type
      O = object
        x: int
        y: int

    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      var o = O()
      (o.x, o.y) =
        if true:
          noop()
          step 2
          (42, 10)
        else:
          fail "this branch should not be run"
          return

      step 3

      check o.x == 42
      check o.y == 10

    foo()

  test "flatten upcasting assignments":
    when not defined(release) and not defined(isNimSkull):
      skip"compiler crashes on debug"
    else:
      type
        O = ref object of RootObj
          x: int
          y: int
        I = ref object of O

      var k = newKiller(3)
      proc foo() {.cps: Cont.} =
        step 1
        var o = O()
        o =
          if true:
            noop()
            step 2
            I(x: 42, y: 10)
          else:
            fail "this branch should not be run"
            I(x: 42, y: 20)

        step 3

        check o of I
        check o.x == 42
        check o.y == 10

      foo()

  test "flatten implicitly converted assignments":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      let o: int =
        if true:
          noop()
          step 2
          Natural(42)
        else:
          fail "this branch should not be run"
          return

      step 3

      check o == 42

    foo()

  test "flatten explicitly converted assignments":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      let i = int(block: (noop(); step 2; 42.Natural))

      step 3

      check i == 42

    foo()

  test "flatten discard statements":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1
      discard (block: (noop(); step 2; 42.Natural))

      step 3

    foo()

  test "flatten return statements":
    var k = newKiller(2)
    proc foo(): int {.cps: Cont.} =
      step 1
      return (block: (noop(); step 2; 42.Natural))

    check foo() == 42
