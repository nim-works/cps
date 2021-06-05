import std/macros
import std/strutils

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
      inc r

    foo()
    check r == 7, "bzzzt"

  block:
    ## custom continuation bootstrap hook works
    var r = 0

    proc bar() {.cps: Cont.} =
      noop()

    proc boot(c: Cont): Cont =
      inc r
      result = c

    proc foo() {.cps: Cont.} =
      bar()

    foo()
    check r == 1, "bzzzt"

  block:
    ## custom continuation head/tail setup hooks work
    var h, t = 0

    proc head(c: Cont): Cont =
      inc h
      result = c

    proc tail(mom: Cont; c: Cont): Cont =
      inc t
      result = c
      result.mom = mom

    proc bar() {.cps: Cont.} =
      check h == 1, "parent triggered second"
      noop()

    proc foo() {.cps: Cont.} =
      check h == 1, "parent triggered first"
      check t == 0, "child triggered first"
      bar()
      check t == 1, "child triggered second"

    var c = whelp foo()
    c = cps.trampoline c
    check "bzzzt":
      h == t
      t == 1

    h = 0
    t = 0
    foo()
    check "bzzzt":
      h == t
      t == 1

  block:
    ## custom continuation bootstrap hook works
    var r = 0

    proc bar() {.cps: Cont.} =
      noop()

    proc boot(c: Cont): Cont =
      inc r
      result = c

    proc foo() {.cps: Cont.} =
      bar()

    foo()
    check r == 1, "bzzzt"
