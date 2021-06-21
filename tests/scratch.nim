import std/macros
import std/strutils
import balls
import cps
import foreign

include preamble

## splitting within pragma blocks
var r = 0

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