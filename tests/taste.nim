import std/macros
import std/strutils
import balls
import cps
import foreign

type
  EmptyLoop = CatchableError
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    mom*: Cont

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c.running:
    # pretends that an exception is raised and handled elsewhere
    setCurrentException(nil)
    c = c.fn(c)
    # no exception should leak outside of the continuation
    check getCurrentException().isNil
    inc jumps
    if jumps > 1000:
      raise InfiniteLoop.newException: $jumps & " iterations"
  if jumps == 0:
    raise EmptyLoop.newException:
      "continuations test best when they, uh, bounce"

proc noop*(c: Cont): Cont {.cpsMagic.} = c

# We have a lot of these for the purpose of control-flow validation
{.warning[UnreachableCode]: off.}

suite "basic testing assumptions":

  block:
    ## the trampoline runs continuations, uh, continuously
    var r = 0
    proc foo() {.cps: Cont.} =
      while true:
        noop()
        inc r
    expect InfiniteLoop:
      trampoline whelp(foo())
    check r > 1

  block:
    ## the noop magic smoke test demonstrates shedding scope
    var r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      inc r
    trampoline whelp(foo())
    check r == 2, "who let the smoke out?"

suite "tasteful tests":

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
    ## whelp operates multi-var proc parameters correctly
    when true:
      skip"pending https://github.com/nim-lang/Nim/issues/18076"
    else:
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
      let c = whelp foo(1, 2)
      trampoline c
      check r == 3

  block:
    ## reassignment of mutable var proc params
    skip"pending issue #47":
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
      foo(x, y, z)
      check "var param assignment":
        x == 5
        y == 7
        z == 3
      check r == 3

  block:
    ## a block statement with a break under an if
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
    foo()
    check r == 4

  block:
    ## a block with a continuation followed by a break
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
    foo()
    check r == 5

  block:
    ## a while statement with a continuation and a break
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
    foo()
    check r == 5

  block:
    ## a break inside a multiply-nested else inside a while
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
    foo()
    check r == 6

  block:
    ## named breaks are supported, with nested breaks
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
    foo()
    check r == 6

  block:
    ## named breaks work from inside a while statement
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
    foo()
    check r == 6

  block:
    ## while loops correctly, uh, loop
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        inc i
      inc r
      check i == 2
    foo()
    check r == 4

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
    ## case statements can support control-flow
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
    foo()
    check r == 9

  block:
    ## a continue statement within a while statement works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 3:
        inc r
        noop()
        inc i
        if i <= 2:
          continue
        check i == 3
        inc r
      inc r
    foo()
    check r == 6

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
    ## local assignment to a continuation return value
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

      foo()
      check r == 3

  block:
    ## a while statement supports a local variable inside
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
    foo()
    check r == 8

  block:
    ## calling a function pointer inside an object works
    type Fn = object
      fn: proc(i: int): int
    let fn = Fn(fn: proc(i: int): int = i * 2)

    proc foo() {.cps: Cont.} =
      noop()
      check fn.fn(10) == 20

    foo()

  block:
    ## calling a macro that calls a foreign symbol works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 42
      noop()
      inc r
      check jsonifyImplicit(i) == $i

    foo()
    check r == 2

  block:
    ## calling a template with explicit symbol binding
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      let i = 42
      noop()
      inc r
      check jsonifyBind(i) == $i

    foo()
    check r == 2

  block:
    ## template calls inside nested call nodes are fine
    r = 0

    # It is crucial that none of these templates call
    # any other templates/macros.
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

    foo()
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

    foo()

  block:
    ## a simple if-else split across continuations works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      if true:
        noop()
        inc r
      else:
        fail "this branch should not run"
      inc r

    foo()
    check r == 3

  block:
    ## a simple case statement split across continuations works
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

    foo()
    check r == 3

  block:
    ## try-except statements may be split across continuations
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
      except:
        fail "this branch should not run"
      inc r

    foo()
    check r == 3

  block:
    ## try-except statements may split and also raise exceptions
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except:
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    foo()
    check r == 4

  block:
    ## exception clauses may split across continuations
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except:
        inc r
        noop()
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## exceptions raised in the current continuation work
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except:
        inc r
        noop()
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## try statements with a finally clause
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

      foo()
      check r == 3

  block:
    ## try statements with an exception and a finally
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

      foo()
      check r == 5

  block:
    ## block control-flow across continuations works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        inc r
        noop()
        inc r
      inc r

    foo()
    check r == 4

  block:
    ## if control-flow across continuations works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      if true:
        inc r
        noop()
        inc r
      inc r

    foo()
    check r == 4

  block:
    ## a defer statement works across a continuation
    when true:
      skip "not working, see #80"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        defer:
          check r == 2, "defer run before end of scope"
          inc r

        inc r
        noop()
        inc r

      foo()
      check r == 3

  block:
    ## implicit generics in continuation creation
    when true:
      skip "not working, ref #51"
    else:
      proc foo(v: auto) {.cps: Cont.} =
        discard $v
        inc r

      r = 0
      foo(42)
      check r == 1

      r = 0
      foo("string")
      check r == 1

  block:
    ## explicit generics in continuation creation
    when true:
      skip "not working, ref #51"
    else:
      proc foo[T](v: T) {.cps: Cont.} =
        discard $v
        inc r

      r = 0
      foo(42)
      check r == 1

      r = 0
      foo("string")
      check r == 1

  block:
    ## nested blocks with an interior break works
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

    foo()
    check r == 4

  block:
    ## a basic defer statement is supported
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      defer:
        check r == 4
        inc r
      inc r
      defer:
        check r == 3
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## a defer in a nested template is supported
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

    foo()
    check r == 6

  block:
    ## a defer inside a block statement works
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

    foo()
    check r == 5

  block:
    ## a naked defer is not a problem
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        inc r

    foo()
    check r == 1

  block:
    ## various varargs usages are supported just fine
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

    foo()
    check r == 1

  block:
    ## for loops with a continue and a break work correctly
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      for i in 0 .. 3:
        if i == 0:
          continue
        if i > 2:
          break
        r.inc i

    foo()
    check r == 4

  block:
    ## for loops with a continue and break across continuations
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

      foo()
      check r == 4

  block:
    ## a while loop with only a single continuing call works
    proc jield(c: Cont): Cont {.cpsMagic.} =
      discard

    r = 0
    proc count() {.cps: Cont.} =
      inc r
      var i = 0

      while i < 2:
        jield()

      fail "this statement should not run"

    count()
    check r == 1

  block:
    ## cooperative yield hooks are used automatically
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

    foo()
    check r == 7

  block:
    ## control-flow tracing hooks are used automatically
    var found: seq[string]
    proc trace(c: Cont; name: string; info: LineInfo) =
      let sub = name.split("_", maxsplit=1)[0]
      found.add sub
      found.add $info.column
      found.add $sizeof(c[])

    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 3:
        noop()
        inc i
        if i == 0:
          continue
        if i > 2:
          break

    foo()
    check found == [ "foo", "4",        "24",
                     "whileLoop", "24", "24", "afterCall", "8", "24",
                     "whileLoop", "24", "24", "afterCall", "8", "24",
                     "whileLoop", "24", "24", "afterCall", "8", "24", ]

  block:
    ## custom continuation allocators are used automatically
    var r = 0
    proc alloc[T: Cont](c: typedesc[T]): c =
      inc r
      new c

    proc foo(x: int) {.cps: Cont.} =
      check x == 3
      noop()
      check x == 3

    foo(3)
    check r == 1, "bzzzt"

  block:
    ## custom continuation deallocators are used automatically
    var r = 0
    proc dealloc[T: Cont](t: typedesc; c: sink T) =
      check r == 0
      inc r

    proc foo(x: int) {.cps: Cont.} =
      check r == 0
      check x == 3
      noop()
      check x == 3
      check r == 0

    foo(3)
    check r == 1, "bzzzt"

  block:
    ## whelp instantiates continuations with arguments
    r = 0
    proc foo(x: int) {.cps: Cont.} =
      check x == 5
      inc r
      let i = 3
      noop()
      inc r
      check i == 3
      check x == 5
    let c = whelp foo(5)
    trampoline c
    check r == 2

  block:
    ## continuations can return values via bootstrap
    r = 0
    proc foo(x: int): int {.cps: Cont.} =
      noop()
      inc r
      return x * x

    let x = foo(3)
    check r == 1
    check x == 9
    var c = whelp foo(5)
    trampoline c
    check r == 2
