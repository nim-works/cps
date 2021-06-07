import std/macros

include preamble
include killer

suite "bug #145":

  block:
    ## https://github.com/disruptek/cps/issues/145
    type
      C = ref object of Continuation
        val: int

    proc send(c: C, v: int) {.cpsVoodoo.} =
      c.val = v

    proc recv(c: C): int {.cpsVoodoo.} =
      c.val

    var k = newKiller(6)

    expandMacros:
      proc level_two() {.cps:C.} =
        step 2
        send(42)
        step 3
        echo recv()
        step 4

    expandMacros:
      proc level_one() {.cps:C.} =
        step 1
        level_two()
        step 5
        let v = recv()
        echo recv()
        step 6

    var a = whelp level_one()
    trampoline a
