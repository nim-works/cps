import std/macros
import foreign

include preamble

suite "tasteful tests":

  var r = 0

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
    ## implicit up-casting
    type O = ref object of RootObj

    var r = 0

    proc foo() {.cps: Continuation.} =
      inc r
      let o: RootRef = new O

    foo()

    check r == 1

  block:
    ## implicit conversion between non-object types
    var r = 0

    proc foo() {.cps: Continuation.} =
      inc r
      let n: Natural = 1
      # implicit conversion in decl
      let i: int = n
      # implicit conversion in assignment
      var j: int
      j = n

    foo()

    check r == 1

  block:
    ## splitting within pragma blocks
    r = 0

    proc foo() {.cps: Cont.} =
      {.cast(gcsafe).}:
        noop()
        inc r

      noop()
      inc r

    foo()

    check r == 2

  block:
    ## accessing a field of a variant object
    type
      O = object
        case switch: bool
        of true:
          x: int
        else:
          discard

    r = 0
    proc foo() {.cps: Cont.} =
      noop()
      inc r
      let o = O(switch: true, x: 42)
      check o.x == 42

    foo()
    check r == 1

  block:
    ## calling a non-local function pointer
    r = 0
    proc bar() =
      inc r

    let fn = bar
    proc foo() {.cps: Cont.} =
      noop()
      inc r
      fn()

    foo()
    check r == 2

  block:
    ## calling a local proc variable
    skip "pending #185":
      r = 0
      proc bar() =
        inc r

      proc foo() {.cps: Cont.} =
        let fn = bar
        noop()
        inc r
        fn()

      foo()
      check r == 2
