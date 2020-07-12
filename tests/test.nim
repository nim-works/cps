import std/macros
import std/unittest

import cps

proc adder(x: int): int {.cps.} =
  return x + 1

suite "cps":
  test "1":
    proc foo(): bool {.cps.} =
      result = true
    check foo() == true

  test "2":
    expandMacros:
      proc foo(): int {.cps.} =
        var i = 0
        while i < 34:
          i = adder(i)
        return i

    check foo() == 34
