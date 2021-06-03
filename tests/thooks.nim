import std/macros
import std/strutils
import balls
import cps

include preamble

suite "hooks":

  var r = 0

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
    ## custom continuation passing hook works
    var r = 0
    proc pass(a, b: Cont): Cont =
      inc r
      result = b

    proc bar() {.cps: Cont.} =
      inc r
      noop()
      inc r

    proc foo() {.cps: Cont.} =
      inc r
      bar()
      inc r

    foo()
    check r == 6, "bzzzt"
