import cps

type
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    when cpsMutant:
      fn*: proc(c: var Cont) {.nimcall.}
    else:
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

template check(cond: bool, msg: string = "") =
  if not cond:
    raise newException(Defect, "not " & astToStr(cond) & ": " & msg)

template fail(msg: string) =
  check false, msg

var r = 0

proc foo() {.cps: Cont.} =
  inc r
  if true:
    noop()
    inc r
    noop()
    inc r
  inc r

trampoline foo()
check r == 4
