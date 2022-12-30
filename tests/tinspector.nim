import std/macros
import std/options

include preamble
include killer

suite "inspector":

  block:
    ## inspector declarations
    type
      C = ref object of Continuation
        x: Option[int]

    proc bar(b: C) {.cps: C.} =
      var a {.cps: [C].} = continuation()
      check a[] != b[]
      check a.mom[] == b[]

    proc foo(x: int) {.cps: C.} =
      var
        c {.cps.} = continuation()
        d {.cps: [C].} = continuation()
      check c == d
      when false:
        var e {.cps.}: C = continuation()  # unsupported
      elif false:
        var e {.cps.} = C continuation()  # no longer works
      var x = x
      while x > 0:
        d.x = some x
        bar d  # make the while: cps-convertible
        dec x

    iterator iterate(c: sink C): int =
      trampolineIt c:
        if it.C.x.isSome:
          yield get it.C.x
          reset it.C.x

    var c = whelp foo(10)

    var values: seq[int]
    for n in iterate c:
      values.add n
    check values == @[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
