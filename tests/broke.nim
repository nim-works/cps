import balls

import cps

when not defined(gcArc):
  {.error: "nonsensical without deterministic memory management".}

type
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    mom: Cont

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)
    inc jumps
    if jumps > 1000:
      raise newException(InfiniteLoop, $jumps & " iterations")

proc noop*(c: Cont): Cont {.cpsMagic.} = c

type
  Killer = object
    x: int
    n: int

proc `=destroy`(k: var Killer) =
  if k.x != k.n:
    fail:
      case k.n
      of 0: "uninitialized"
      of 1: "unused"
      else: "misused; " & $(k.n - 1) & " uses, expected " & $(k.x - 1)

proc newKiller(x = 1): Killer =
  Killer(n: 1, x: x + 1)

template step(i: int) {.dirty.} =
  check k.n == i, "out-of-order"
  inc k.n
  k.x = max(i, k.x)

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
