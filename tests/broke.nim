import balls

import cps

when not defined(gcArc):
  {.error: "nonsensical without deterministic memory management".}

type
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

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

suite "breaking deterministic memory managers":
  block:
    ## try-except-statement splits
    proc foo() {.cps: Cont.} =
      var k = newKiller(2)
      inc k.n
      try:
        noop()
        inc k.n
      except:
        fail "this branch should not run"

    trampoline foo()

  block:
    ## try-except splits with raise
    proc foo() {.cps: Cont.} =
      var k = newKiller(4)
      inc k.n
      try:
        noop()
        inc k.n
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        inc k.n
      inc k.n

    trampoline foo()

  block:
    ## try-finally-statement splits
    proc foo() {.cps: Cont.} =
      var k = newKiller(3)
      inc k.n
      try:
        noop()
        inc k.n
      finally:
        inc k.n

    trampoline foo()

  block:
    ## try-except-finally splits with raise
    proc foo() {.cps: Cont.} =
      var k = newKiller(5)
      inc k.n
      try:
        noop()
        inc k.n
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        inc k.n
      finally:
        inc k.n
      inc k.n

    trampoline foo()
