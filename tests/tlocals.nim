import std/sugar

include preamble
include killer

suite "locals":

  var r = 0

  block:
    ## local variables migrate into the continuation
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 3
      noop()
      inc r
      check i == 3
    foo()
    check r == 2

  block:
    ## local variable multi-assignment
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i, j = 2
      check i == 2
      check j == 2
      noop()
      inc r
      inc i
      check i == 3
      check j == 2
    foo()
    check r == 2

  block:
    ## out-of-scope variables operate as expected
    r = 0
    var j = 2
    proc foo() {.cps: Cont.} =
      inc r
      check j == 2
      j = 4
      noop()
      inc r
      check j == 4
      inc j
    foo()
    check r == 2
    check j == 5

  block:
    ## proc parameters pass across continuations
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      noop()
      check x > 0, "parameter value unset"
    shadow(1)
    check r == 1

  block:
    ## local variables may shadow proc parameters
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      let x = 3
      noop()
      check x == 3, "shadowed symbol wrong"
    shadow(1)
    check r == 1

  block:
    ## shadowing variables may impart new mutability
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        noop()
        check x == 4, "shadowing symbol wrong"
      check x == 3, "shadowed symbol corrupted"
    shadow(3)
    check r == 1

  block:
    ## shadowing variables pass across continuations
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        inc x
        noop()
        check x == 5, "shadowing symbol immutable"
      check x == 3, "shadowed symbol corrupted"
    shadow(3)
    check r == 1

  block:
    ## scope-based shadowing is also supported
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        noop()
        block:
          inc x
        check x == 5, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted"
    shadow(3)
    check r == 1

  block:
    ## shadowing is unperturbed by continuation calls
    r = 0
    proc shadow1(x: int) {.cps: Cont.} =
      inc r
      check x == 3, "unexpected input of " & $x
      block:
        var x = x + 2
        check x == 5, "failed to update from lower scope"
        noop()
        inc x
        check x == 6, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted to " & $x

    proc shadow2(x: int) {.cps: Cont.} =
      inc r
      var x = x + 2
      check x == 3, "shadow1 is expecting x == 3"
      shadow1(x)
      noop()
      check x == 3, "x mutated by cps call"
      inc x
      check x == 4, "shadowing symbol corrupted"

    shadow2(1)
    check r == 2

  block:
    ## shadowing and proc param defaults are supported
    r = 0
    proc foo(a, b, c: int = 3) {.cps: Cont.} =
      inc r
      ## a=1, b=2, c=3
      var a = 5
      ## a=5, b=2, c=3
      noop()
      inc r
      ## a=5, b=2, c=3
      var b = b + a
      ## a=5, b=7, c=3
      noop()
      inc r
      ## a=5, b=7, c=3
      check "proc parameters":
        a == 5
        b == 7
        c == 3
    foo(1, 2)
    check r == 3

  block:
    ## a gratuitously complex shadowing test works
    r = 0
    proc b(x: int) {.cps: Cont.} =
      inc r
      check x == 2, "unexpected input to cps call b()"
      noop()
      inc r
      let x = x + 1
      noop()
      inc r
      check x == 3, "let from proc param incorrect"
      noop()
      inc r
      var y = 8
      block:
        noop()
        inc r
        var x = 4
        noop()
        inc r
        inc x
        noop()
        inc r
        dec y
        noop()
        inc r
        check x == 5, "shadowed var x could not be mutated"
        check y == 7, "y could not be mutated in lower scope"
      noop()
      inc r
      check x == 3, "lower scope mutated shadowed x"
      noop()
      inc r
      check y == 7, "lower scope could not mutate y"

    proc a(x: int) {.cps: Cont.} =
      inc r
      check x == 1, "unexpected input to cps call a()"
      noop()
      inc r
      check x == 1, "noop managed to erase x"
      var x = 2
      noop()
      inc r
      check x == 2, "could not shadow proc param x"
      noop()
      inc r
      b(x)
      noop()
      inc r
      check x == 2, "x mutated by cps call"

    a(1)
    check r == 15

  block:
    ## re-entrant scopes reset locals
    proc foo() {.cps: Continuation.} =
      var i = 0
      while i < 2:
        var j: int
        check j == 0, "failed to reset j"
        j = 3
        inc i
      check i == 2

    foo()

  block:
    ## re-entrant scopes reset locals to initialization values
    proc foo() {.cps: Continuation.} =
      var i = 0
      while i < 2:
        var j, k = 3
        check j == 3, "failed to reset j"
        check k == 3, "failed to reset k"
        j = 4
        k = 4
        inc i
      check i == 2

    foo()

  block:
    ## local type inference for procedure symbols
    proc bar(x: int) =
      discard

    proc foo() {.cps: Continuation.} =
      var f = bar
      f(10)

    foo()

  block:
    ## naive callback semantics work with simplified syntax
    type
      CallBack = proc(): Continuation

    proc bar(cb: CallBack) {.cps: Continuation.} =
      discard

    proc thing() {.cps: Continuation.} =
      discard

    proc foo() {.cps: Continuation.} =
      proc gen(): CallBack =
        result =
          proc(): Continuation =
            whelp thing()
      bar gen()

    foo()

  block:
    ## nested procedures may be defined and passed as arguments
    proc two(fn: proc(): bool) {.cps: Cont.} =
      check true == fn()

    proc one() {.cps: Cont.} =
      noop()
      proc fn(): bool = true
      noop()
      two(fn)

    one()

  block:
    ## lambdas may be defined and passed as arguments, with some caveats
    proc two(fn: proc(): bool) {.cps: Cont.} =
      check true == fn()

    proc one() {.cps: Cont.} =
      noop()
      var fn: proc(): bool
      fn = proc(): bool = true
      noop()
      two(fn)

    one()

  block:
    ## pragmas in identdefs are accepted for rewrite
    template checkIt(list: untyped; logic: untyped): untyped =
      for it {.inject.} in list.items:
        logic

    proc bar() {.cps: Cont.} =
      checkIt [true]:
        check it

      checkIt [2]:
        check it == 2

    proc foo() {.cps: Cont.} =
      while true:
        bar()
        break

    foo()

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

  block:
    ## sugary procedure arguments can be used in expressions
    r = 0
    proc bar(x: int): int {.cps: Cont.} =
      inc r
      result = x * 2

    proc foo(fn: (int) -> int): int {.cps: Cont.} =
      inc r
      result = fn: bar(2)
      inc r

    check 12 == foo(x => x * 3)
    check r == 3

type
  K = distinct object

var k = initKiller 9

proc newK(): K = discard

proc reset*(v: var K) =
  inc k
  inc k
  inc k

proc `=destroy`(v: var K) =
  inc k

suite "lifetimes":
  block:
    ## child continuation lifetimes canary
    when not defined(release) and not defined(isNimSkull):
      skip"triggers compiler error outside release"
    else:
      type
        K = object
        C = ref object of Cont
          n: K
        D = ref object of C
          m: K

      var k = initKiller 11

      proc `=destroy`(v: var K) =
        inc k

      proc setup(c: C): C {.cpsMagic.} =
        c.n = K()
        inc k
        c

      proc create(d: D): D {.cpsMagic.} =
        d.m = K()
        inc k
        d

      proc bar() {.cps: D.} =
        step 4
        create()
        check k.step == 6, "expected create/destroy in bar: " & $k.step
        step 7

      proc foo() {.cps: C.} =
        step 1
        setup()
        check k.step == 3, "expected create/destroy in foo: " & $k.step
        bar()
        check k.step == 7, "expected child to exist"
        step 8
        # destroy foo.n; eg. step == 9
        # destroy bar.n; eg. step == 10
        # destroy bar.m; eg. step == 11

      foo()

  block:
    ## lifetime canary for distinct objects

    type
      C = ref object of Cont
        n: K
      D = ref object of C
        m: K

    proc setup(c: C): C {.cpsMagic.} =
      c.n = newK()
      inc k
      c

    proc create(d: D): D {.cpsMagic.} =
      d.m = newK()
      inc k
      d

    proc bar() {.cps: D.} =
      step 3
      create()
      check k.step == 4, "expected create/destroy in bar: " & $k.step
      step 5

    proc foo() {.cps: C.} =
      step 1
      setup()
      check k.step == 2, "expected create/destroy in foo: " & $k.step
      bar()
      check k.step == 5, "expected child to exist: " & $k.step
      step 6
      # destroy foo.n; eg. step == 7
      # destroy bar.n; eg. step == 8
      # destroy bar.m; eg. step == 9

    foo()
