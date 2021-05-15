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
  if k.x == k.n:
    system.`=destroy`(k)
  else:
    raise Defect.newException:
      case k.n
      of 0: "destroy on uninitialized"
      of 1: "destroy on unused"
      else: "destroy on overused"

proc newKiller(x = 2): Killer =
  Killer(n: 1, x: x)

suite "breaking deterministic memory managers":
  block:
    ## try-except-statement splits
    proc foo() {.cps: Cont.} =
      var k = newKiller()
      try:
        noop()
        inc k.n
      except:
        fail"just no"

    trampoline foo()

when false:
  var r: int

  block:
    ## try-except splits with raise
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        inc r
      inc r

    trampoline foo()
    check r == 4

  block:
    ## try-finally-statement splits
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
      finally:
        inc r

    trampoline foo()
    check r == 3

  block:
    ## try-except-finally splits with raise
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except:
        inc r
      finally:
        inc r
      inc r

    trampoline foo()
    check r == 5

  block:
    ## block control flow after split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        inc r
        noop()
        inc r
      inc r

    trampoline foo()
    check r == 4

  block:
    ## if control flow after split
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      if true:
        inc r
        noop()
        inc r
      inc r

    trampoline foo()
    check r == 4

  block:
    ## defer before split
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        check r == 2, "defer run before end of scope"
        inc r

      inc r
      noop()
      inc r

    trampoline foo()
    check r == 3

  block:
    ## basic defer rewrite
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      defer:
        check r == 4
        inc r
      inc r
      defer:
        check r == 3
        inc r
      inc r

    trampoline foo()
    check r == 5

  block:
    ## defer in nested stmtlist rewrite
    r = 0

    template deferChk(i: int) =
      inc r
      defer:
        check r == i
        inc r

    proc foo() {.cps: Cont.} =
      deferChk(5)
      inc r
      deferChk(4)
      inc r

    trampoline foo()
    check r == 6

  block:
    ## defer in block rewrite
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        defer:
          check r == 2
          inc r
        inc r
      defer:
        check r == 4
        inc r
      inc r

    trampoline foo()
    check r == 5

  block:
    ## there is only defer rewrite
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        inc r

    trampoline foo()
    check r == 1
