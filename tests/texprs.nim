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

  test "flatten case expression":
    var k = newKiller(3)
    proc foo() {.cps: Cont.} =
      step 1

      let x =
        case "true"
        of "truer", "truest", "very true":
          fail "this branch should not run"
          0
        of "false":
          fail "this branch should not run"
          -1
        of "true":
          noop()
          step 2
          42
        elif true:
          fail "this branch should not run"
          -3
        else:
          fail "this branch should not run"
          -2

      step 3
      check x == 42

    foo()

  test "flatten try statement":
    var k = newKiller(4)
    proc foo() {.cps: Cont.} =
      step 1

      let x =
        try:
          raise newException(ValueError, "something")
          0
        except ValueError, IOError:
          noop()
          let e = getCurrentException()
          check e of ValueError
          check e.msg == "something"
          step 2
          42
        except:
          fail "this branch should not run"
          -1
        finally:
          step 3

      step 4
      check x == 42

    foo()

  test "flatten if condition":
    var k = newKiller(5)
    proc foo() {.cps: Cont.} =
      step 1

      if (noop(); step 2; false):
        fail "This branch should not be run"
      elif (noop(); step 3; true):
        step 4
      elif (noop(); fail"This expression should not be evaluated"; false):
        fail "This branch should not be run"
      else:
        fail "This branch should not be run"

      step 5

    foo()

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

  test "flatten while condition":
    var k = newKiller(4)
    proc foo() {.cps: Cont.} =
      step 1

      var x = 2
      while (noop(); step x; inc x; x < 4):
        discard

      step 4

    foo()

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
    when not defined(release) and defined(gcArc):
      skip "not sure why but the compiler dies"
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
            return

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

  test "flatten pragma block expression":
    var k = newKiller(3)

    proc foo() {.cps: Cont.} =
      step 1
      let x =
        block:
          {.cast(gcsafe).}:
            noop()
            step 2
            10
      step 3
      check x == 10

    foo()

  test "flatten result expressions":
    var k = newKiller(1)
    proc foo(): int {.cps: Cont.} =
      noop()
      step 1
      42.Natural

    check foo() == 42

  test "flatten bracket expressions (array access)":
    var k = newKiller(2)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; [42])[(noop(); step 2; 0)] == 42

    foo()

  test "flatten dot expressions":
    type
      P = object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42)).val == 42

    foo()

  test "flatten dereference expressions":
    type
      P = ref object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42))[].val == 42

    foo()

  test "flatten hidden dereference expressions":
    type
      P = ref object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42)).val == 42

    foo()
