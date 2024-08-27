include preamble

suite "expression flattening":
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
    check k

  test "flatten result expressions":
    var k = newKiller(1)
    proc foo(): int {.cps: Cont.} =
      noop()
      step 1
      42.Natural

    check foo() == 42
    check k

  test "flatten bracket expressions (array access)":
    var k = newKiller(2)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; [42])[(noop(); step 2; 0)] == 42

    foo()
    check k

  test "flatten dot expressions":
    type
      P = object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42)).val == 42

    foo()
    check k

  test "flatten dereference expressions":
    type
      P = ref object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42))[].val == 42

    foo()
    check k

  test "flatten hidden dereference expressions":
    type
      P = ref object
        val: int

    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      check (noop(); step 1; P(val: 42)).val == 42

    foo()
    check k

  test "flatten magic calls with mutable variables":
    var k = newKiller(3)

    proc foo() {.cps: Cont.} =
      var x: string
      # add(var string, string) is a magic
      x.add (noop(); step 1; "test")
      check x == "test"

      var y: seq[string]
      # add(var seq[T], T) is a magic with generics
      y.add (noop(); step 2; "test")
      check y == @["test"]

      step 3

    foo()
    check k
