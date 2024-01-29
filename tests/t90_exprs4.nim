include preamble

suite "expression flattening":
  test "flatten array construction":
    var k = newKiller(5)
    proc foo() {.cps: Cont.} =
      step 1

      let x = [1, 2, (step 2; 42), (noop(); step 3; 10), (step 4; 20)]

      step 5
      check x == [1, 2, 42, 10, 20]

    foo()

  test "flatten tuple construction":
    var k = newKiller(5)
    proc foo() {.cps: Cont.} =
      step 1

      let x = (a: 1, b: 2, c: (step 2; 42), d: (noop(); step 3; 10), e: (step 4; 20))

      step 5
      # A few checks to verify that the names stay
      check x.a == 1
      check x.b == 2
      check x == (1, 2, 42, 10, 20)

    foo()

  test "flatten object construction":
    type
      O = object of RootObj
        x: int
        y: int
        z: float

    var k = newKiller(5)
    proc foo() {.cps: Cont.} =
      step 1

      let x = O(x: (step 2; 42), y: (noop(); step 3; 10), z: (step 4; 20))

      step 5
      # A few checks to verify that the names stay
      check x == O(x: 42, y: 10, z: 20)

    foo()

  test "flatten calls":
    var k = newKiller(5)

    proc bar(a, b: int) =
      step 3
      check a == 42
      check b == 10

    proc barvar(a: var int, b: int) =
      step 5
      check a == 20
      check b == 20

    proc foo() {.cps: Cont.} =
      step 1
      var x = 42
      bar(x, (noop(); step 2; x = 10; x))

      x = 42
      barvar(x, (noop(); step 4; x = 20; x))

    foo()

  test "flatten and/or with short circuiting":
    var k = newKiller(7)

    proc foo() {.cps: Cont.} =
      step 1
      check (noop(); step 2; true) and (noop(); step 3; true)
      check not((noop(); step 4; false) and (noop(); fail "this should not run"; true))
      check (noop(); step 5; false) or (noop(); step 6; true)
      check (noop(); step 7; true) or (noop(); fail "this should not run"; false)

    foo()

  test "flatten raise statement":
    var k = newKiller(3)

    proc foo() {.cps: Cont.} =
      step 1
      try:
        raise (noop(); step 2; newException(CatchableError, "test"))
      except CatchableError as e:
        step 3
        check e.msg == "test"

    foo()
