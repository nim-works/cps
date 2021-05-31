import balls

import cps

when not defined(gcArc):
  {.error: "nonsensical without deterministic memory management".}

include preamble
include killer

suite "breaking deterministic memory managers":
  block:
    ## try-except-statement splits
    proc foo() {.cps: Cont.} =
      var k = newKiller(3)
      step 1
      try:
        noop()
        step 2
      except:
        fail "this branch should not run"
      step 3

    foo()

  block:
    ## try-except splits with raise
    proc foo() {.cps: Cont.} =
      var k = newKiller(4)
      step 1
      try:
        noop()
        step 2
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        step 3
      step 4

    foo()

  block:
    ## try-finally-statement splits
    proc foo() {.cps: Cont.} =
      var k = newKiller(4)
      step 1
      try:
        noop()
        step 2
      finally:
        step 3
      step 4

    foo()

  block:
    ## try-except-finally splits with raise
    proc foo() {.cps: Cont.} =
      var k = newKiller(5)
      step 1
      try:
        noop()
        step 2
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        step 3
      finally:
        step 4
      step 5

    foo()
