import std/macros

include preamble
include killer

macro dumper(n: typed): untyped =
  echo getType(n).treeRepr

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
      H = proc(x: int): float {.cps: C.}
      I = proc(): float {.cps: C.}
      P = proc(x: int) {.cps: C.}
      S = proc() {.cps: C.}

    proc toH(x: int): float {.cps: C.} = discard
    proc toI(): float {.cps: C.} = discard
    proc toP(x: int) {.cps: C.} = discard
    proc toS() {.cps: C.} = discard

    let h: H = whelp toH
    let i: I = whelp toI
    let p: P = whelp toP
    let s: S = whelp toS

  block:
    ## execute a callback with arguments
    type
      C = ref object of Continuation

    proc foo(x: int): float {.cps: C.} =
      result = x.float

    let cb = whelp foo
    let c = cb.call(3)
    let d = cb.call(5)
    check c() == 3.0
    check d() == 5.0
