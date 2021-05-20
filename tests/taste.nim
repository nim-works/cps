import balls

import cps

import foreign

type
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c.running:
    c = c.fn(c)
    inc jumps
    if jumps > 1000:
      raise newException(InfiniteLoop, $jumps & " iterations")

proc noop*(c: Cont): Cont {.cpsMagic.} = c

suite "basic testing assumptions":

  block:
    ## the trampoline runs continuations, uh, continuously
    var r = 0
    proc foo() {.cps: Cont.} =
      while true:
        noop()
        inc r
    expect InfiniteLoop:
      trampoline foo()
    check r > 1

  block:
    ## noop magic smoke test
    var r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      inc r
    trampoline foo()
    check r == 2, "who let the smoke out?"

suite "tasteful tests":

  var r = 0

  block:
    ## local variables migrating in/out of env
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 3
      noop()
      inc r
      check i == 3
    trampoline foo()
    check r == 2

  block:
    ## out-of-scope variables operate correctly
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
    trampoline foo()
    check r == 2
    check j == 5

  block:
    ## deconstruction with a call source
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
    trampoline foo()
    check r == 2

  block:
    ## deconstruction with distinct types
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
    trampoline foo()
    check r == 2

  block:
    ## declaration via tuple deconstruction
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
    trampoline foo()
    check r == 2

  block:
    ## multi-var declaration
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
    trampoline foo()
    check r == 2

  block:
    ## shadowing and proc param defaults
    ## https://github.com/disruptek/cps/issues/22
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
    trampoline foo(1, 2)
    check r == 3

  block:
    ## reassignment of var proc params
    ## https://github.com/disruptek/cps/issues/47
    skip"pending issue #47"
    r = 0
    proc foo(a, b, c: var int) {.cps: Cont.} =
      inc r
      a = 5
      noop()
      inc r
      b = b + a
      noop()
      inc r
      check "var param assignment":
        a == 5
        b == 7
        c == 3
    var (x, y, z) = (1, 2, 3)
    trampoline foo(x, y, z)
    check "var param assignment":
      x == 5
      y == 7
      z == 3
    check r == 3

  block:
    ## block with break under if
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        inc r
        if true:
          inc r
          break
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 4

  block:
    ## cps block with break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        inc r
        if true:
          inc r
          noop()
          inc r
          break
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 5

  block:
    ## break statements without cps 🥴
    r = 0
    proc foo() =
      inc r
      while true:
        inc r
        if true:
          inc r
          break
        inc r
        fail"block break failed to break block"
      inc r
    foo()
    check r == 4

  block:
    ## a fairly tame cps break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      while true:
        inc r
        noop()
        inc r
        if true:
          inc r
          break
        inc r
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 5

  block:
    ## break in a nested else (don't ask)
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      while true:
        inc r
        noop()
        inc r
        if true:
          inc r
          check r == 4
          if r != 4:
            fail"unexpected clause"
          else:
            inc r
            break
      inc r
    trampoline foo()
    check r == 6

  block:
    ## named breaks
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        block found:
          inc r
          block:
            inc r
            break
            inc r
          block:
            inc r
            break found
          fail"A: should be unreachable"
        inc r
        break
        fail"B: should be unreachable"
      inc r
    trampoline foo()
    check r == 6

  block:
    ## named breaks from inside a while
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        block found:
          inc r
          while true:
            inc r
            noop()
            inc r
            if true:
              inc r
              break found
            fail"loop tail should be unreachable"
          fail"post loop should be unreachable"
        inc r
        break
        inc r
    trampoline foo()
    check r == 6

  block:
    ## while loops correctly
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        inc i
      inc r
      check i == 2
    trampoline foo()
    check r == 4

  block:
    ## while statement with cps call inside
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        noop()
        inc r
        inc i
      inc r
      check i == 2
    trampoline foo()
    check r == 6

  block:
    ## shadow test A
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      check x > 0, "parameter value unset"
    trampoline shadow(1)
    check r == 1

  block:
    ## shadow test B
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      let x = 3
      check x == 3, "shadowed symbol wrong"
    trampoline shadow(1)
    check r == 1

  block:
    ## shadow test C
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        check x == 4, "shadowing symbol wrong"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test D
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        inc x
        check x == 5, "shadowing symbol immutable"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test E
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        block:
          inc x
        check x == 5, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test F
    r = 0
    proc shadow1(x: int) {.cps: Cont.} =
      inc r
      check x == 3, "unexpected input of " & $x
      block:
        var x = x + 2
        check x == 5, "failed to update from lower scope"
        inc x
        check x == 6, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted to " & $x

    proc shadow2(x: int) {.cps: Cont.} =
      inc r
      var x = x + 2
      check x == 3, "shadow1 is expecting x == 3"
      trampoline shadow1(x)
      check x == 3, "x mutated by cps call"
      inc x
      check x == 4, "shadowing symbol corrupted"

    trampoline shadow2(1)
    check r == 2

  block:
    ## case statements
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 3:
        inc r
        inc i
        case i
        of 1:
          inc r
          discard
        of 2:
          check r == 5
          inc r
          continue
        of 3:
          check r == 7
          inc r
          break
        else:
          fail"unexpected default case"
        let q = r - 2  # work-around
        check i == q, "bad control flow"
        inc r
      inc r
    trampoline foo()
    check r == 9

  block:
    ## continue statement within while
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 3:
        inc r
        inc i
        if i <= 2:
          continue
        check i == 3
        inc r
      inc r
    trampoline foo()
    check r == 6

  block:
    ## for loop with continue, break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      while true:
        for i in 0 .. 3:
          if i == 0:
            continue
          if i > 2:
            break
          r.inc i
        inc r
        if r == 5:
          break
        inc r
      inc r
    trampoline foo()
    check r == 6

  block:
    ## shadow mission impossible
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
      trampoline b(x)
      noop()
      inc r
      check x == 2, "x mutated by cps call"

    trampoline a(1)
    check r == 15

  block:
    ## assignment shim with constant
    when true:
      skip"pending discussion #28"
    else:
      r = 0
      proc bar(a: int): int {.cps: Cont.} =
        inc r
        noop()
        return a * 2

      proc foo() {.cps: Cont.} =
        inc r
        let x = int bar(4)
        inc r
        check x == 8

      trampoline foo()
      check r == 3

  block:
    ## while statement with local var inside
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        let x = i
        noop()
        inc r
        inc i
        noop()
        inc r
        check x == i - 1
      inc r
    trampoline foo()
    check r == 8

  block:
    ## running a function pointer inside an object
    type Fn = object
      fn: proc(i: int): int
    let fn = Fn(fn: proc(i: int): int = i * 2)

    proc foo() {.cps: Cont.} =
      check fn.fn(10) == 20

    trampoline foo()

  block:
    ## call a macro that calls a foreign symbol
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 42
      noop()
      inc r
      check jsonifyImplicit(i) == $i

    trampoline foo()
    check r == 2

  block:
    ## call a template with explicit bind
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 42
      noop()
      inc r
      check jsonifyBind(i) == $i

    trampoline foo()
    check r == 2

  block:
    ## template call in nested call nodes
    r = 0

    # It is crucial that none of these templates call any other templates/macros.
    template negate(b: untyped): untyped =
      not b

    template isEmpty(s: untyped): untyped =
      s == ""

    template makeStr(s: untyped): untyped =
      let r = $s
      r

    proc foo() {.cps: Cont.} =
      inc r
      # It has to have this exact amount of templates nesting or it won't
      # reproduce the bug.
      let chk =
        negate:
          negate:
            negate:
              isEmpty:
                makeStr "foo"
      check chk

    trampoline foo()
    check r == 1

  block:
    ## calling a function pointer produced by an expression
    proc bar(i: int): proc(): int =
      result = proc(): int =
        i * 2

    proc foo() {.cps: Cont.} =
      var i = 2
      noop()
      i = bar(i)()
      noop()
      check i == 4

    trampoline foo()

  block:
    ## simple if-else split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      if true:
        noop()
        inc r
      else:
        fail "this branch should not run"
      inc r

    trampoline foo()
    check r == 3

  block:
    ## simple case statement split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      case true:
      of true:
        noop()
        inc r
      of false:
        fail "this branch should not run"
      inc r

    trampoline foo()
    check r == 3

  block:
    ## try-except-statement splits
    when true:
      skip "not working, see #78"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        try:
          noop()
          inc r
        except:
          fail "this branch should not run"
        inc r

      trampoline foo()
      check r == 3

  block:
    ## try-except splits with raise
    when true:
      skip "not working, see #78"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        try:
          noop()
          inc r
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except:
          inc r
        inc r

      trampoline foo()
      check r == 4

  block:
    ## try-finally-statement splits
    when true:
      skip "not working, see #78"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        try:
          noop()
          inc r
        finally:
          inc r

      trampoline foo()
      check r == 3

  block:
    ## try-except-finally splits with raise
    when true:
      skip "not working, see #78"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        try:
          noop()
          inc r
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except:
          inc r
        finally:
          inc r
        inc r

      trampoline foo()
      check r == 5

  block:
    ## block control flow after split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        inc r
        noop()
        inc r
      inc r

    trampoline foo()
    check r == 4

  block:
    ## if control flow after split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      if true:
        inc r
        noop()
        inc r
      inc r

    trampoline foo()
    check r == 4

  block:
    ## defer with split
    skip "not working, see #80"
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        check r == 2, "defer run before end of scope"
        inc r

      inc r
      noop()
      inc r

    trampoline foo()
    check r == 3

  block:
    ## implicit generics
    when true:
      skip "not working, ref #51"
    else:
      proc foo(v: auto) {.cps: Cont.} =
        discard $v
        inc r

      r = 0
      trampoline foo(42)
      check r == 1

      r = 0
      trampoline foo("string")
      check r == 1

  block:
    ## explicit generics
    when true:
      skip "not working, ref #51"
    else:
      proc foo[T](v: T) {.cps: Cont.} =
        discard $v
        inc r

      r = 0
      trampoline foo(42)
      check r == 1

      r = 0
      trampoline foo("string")
      check r == 1

  block:
    ## nested block breaks with one containing split and one doesn't
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block a:
        inc r
        block:
          inc r
          break a
          fail "inner block continued"

        noop()
        fail "block was not broken out"
      inc r

    trampoline foo()
    check r == 4

  block:
    ## basic defer rewrite
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      defer:
        check r == 4
        inc r
      inc r
      defer:
        check r == 3
        inc r
      inc r

    trampoline foo()
    check r == 5

  block:
    ## defer in nested stmtlist rewrite
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

    trampoline foo()
    check r == 6

  block:
    ## defer in block rewrite
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

    trampoline foo()
    check r == 5

  block:
    ## there is only defer rewrite
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        inc r

    trampoline foo()
    check r == 1

  block:
    ## varargs rewrites
    # various forms of concat
    func concatConv(parts: varargs[string, `$`]): string =
      for i in parts:
        result.add i

    func concat(parts: varargs[string]): string =
      for i in parts:
        result.add i

    func concatDuo(a, b: varargs[string]): string =
      for i in a:
        result.add i
      for i in b:
        result.add i

    func `&!`(parts: varargs[string]): string =
      for i in parts:
        result.add i

    func `&`(s: string, parts: varargs[string]): string =
      result = s
      for i in parts:
        result.add i

    func `%&`(parts: varargs[string], s: string): string =
      for i in parts:
        result.add i
      result.add s

    func `%&%`(a, b: varargs[string]): string =
      for i in a:
        result.add i
      for i in b:
        result.add i

    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      check concatConv("foo ", 42, 0) == "foo 420"
      check concat("foo ", $42, $0) == "foo 420"
      check concatDuo(["f", "o", "o "], $42, $0) == "foo 420"
      check &!["foo ", $42, $0] == "foo 420"
      check "foo " & [$42, $0] == "foo 420"
      check ["f", "o", "o "] %& "420" == "foo 420"
      check ["foo "] %&% [$42, $0] == "foo 420"
      check concatConv(@["foo ", $42, $0]) == "foo 420"
      check concat(@["foo ", $42, $0]) == "foo 420"
      check concatDuo(@["f", "o", "o "], @[$42, $0]) == "foo 420"
      check &!(@["foo ", $42, $0]) == "foo 420"
      check "foo " & @[$42, $0] == "foo 420"
      check @["f", "o", "o "] %& "420" == "foo 420"
      check @["foo "] %&% @[$42, $0] == "foo 420"

    trampoline foo()
    check r == 1

  block:
    ## for loops with continue, break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      for i in 0 .. 3:
        if i == 0:
          continue
        if i > 2:
          break
        r.inc i

    trampoline foo()
    check r == 4

  block:
    ## for loops with continue, break and a split
    when true:
      skip"pending #48"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        for i in 0 .. 3:
          noop()
          if i == 0:
            continue
          if i > 2:
            break
          r.inc i

      trampoline foo()
      check r == 4

  block:
    ## while loop with only one cpsCall
    proc jield(c: Cont): Cont {.cpsMagic.} =
      discard

    r = 0
    proc count() {.cps: Cont.} =
      inc r
      var i = 0

      while i < 2:
        jield()

      fail "this statement should not run"

    trampoline count()
    check r == 1

  block:
    ## cooperative yield
    proc coop(c: Cont): Cont {.cpsMagic.} =
      inc r
      result = c

    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 3:
        inc r
        noop()
        inc i
        if i == 0:
          continue
        if i > 2:
          break

    trampoline foo()
    check r == 7
