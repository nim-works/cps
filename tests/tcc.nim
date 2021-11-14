import std/macros

include preamble
include killer

suite "calling convention":

  block:
    ## some different callback types
    type
      C = ref object of Continuation
      H {.used.} = proc(x: int): float {.cps: C.}
      I {.used.} = proc(): float {.cps: C.}
      P {.used.} = proc(x: int) {.cps: C.}
      S {.used.} = proc() {.cps: C.}

  block:
    ## produce callbacks from a symbol
    type
      C = ref object of Continuation

    proc toH(x: int): float {.cps: C.} = discard
    proc toI(): float {.cps: C.} = discard
    proc toP(x: int) {.cps: C.} = discard
    proc toS() {.cps: C.} = discard

    let h {.used.} = whelp toH
    let i {.used.} = whelp toI
    let p {.used.} = whelp toP
    let s {.used.} = whelp toS

  block:
    ## produce callbacks that match whelp types
    skip "not supported by the compiler yet":
      type
        C = ref object of Continuation
        H {.used.} = proc(x: int): float {.cps: C.}
        I {.used.} = proc(): float {.cps: C.}
        P {.used.} = proc(x: int) {.cps: C.}
        S {.used.} = proc() {.cps: C.}

      proc toH(x: int): float {.cps: C.} = discard
      proc toI(): float {.cps: C.} = discard
      proc toP(x: int) {.cps: C.} = discard
      proc toS() {.cps: C.} = discard

      let h {.used.}: H = whelp toH
      let i {.used.}: I = whelp toI
      let p {.used.}: P = whelp toP
      let s {.used.}: S = whelp toS

  block:
    ## execute a callback with arguments
    type
      C = ref object of Continuation

    proc foo(x: int): float {.cps: C.} =
      result = x.float

    const cb = whelp foo
    let c = cb.call(3)
    let d = cb.call(5)
    check cb.result(c) == 3.0
    check cb.result(d) == 5.0

  block:
    ## run a callback from inside cps
    var k = newKiller 4

    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 3
      return a * 2

    const cb = whelp bar

    proc foo(c: typeOf cb) {.cps: Cont.} =
      step 1
      let x = c.call(4)
      step 2
      check c.result(x) == 8
      step 4

    foo cb

when false:
  block:
    ## run a callback in cps with natural syntax
    var k = newKiller 3

    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 2
      return a * 2

    const cb = whelp bar
    type Callback = typeOf cb

    proc foo(c: Callback) {.cps: Cont.} =
      step 1
      let x = c(4)
      check x == 8
      step 3

    foo cb
