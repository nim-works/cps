import std/sugar

include preamble

suite "high-cal":
  var r = 0
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
