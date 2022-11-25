include preamble

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
