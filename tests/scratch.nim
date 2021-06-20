import std/macros
import std/strutils
import balls
import cps
import foreign

include preamble

suite "tasteful tests":

  var r = 0
  block:
    ## continuations can return values via bootstrap (using result)
    # r = 0
    # proc foo(x: int): int {.cps: Cont.} =
    #   noop()
    #   inc r
    #   result = x * x

    # let x = foo(3)
    # check r == 1
    # check x == 9
    # var c = whelp foo(5)
    # trampoline c
    # check r == 2