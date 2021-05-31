type
  EmptyLoop = CatchableError
  InfiniteLoop = CatchableError
  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    mom*: Cont

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c.running:
    # pretends that an exception is raised and handled elsewhere
    setCurrentException(nil)
    c = c.fn(c)
    # no exception should leak outside of the continuation
    check getCurrentException().isNil
    inc jumps
    if jumps > 1000:
      raise InfiniteLoop.newException: $jumps & " iterations"
  if jumps == 0:
    raise EmptyLoop.newException:
      "continuations test best when they, uh, bounce"

proc noop*(c: Cont): Cont {.cpsMagic.} = c

# We have a lot of these for the purpose of control-flow validation
{.warning[UnreachableCode]: off.}

suite "basic testing assumptions":

  block:
    ## the trampoline runs continuations, uh, continuously
    var r = 0
    proc foo() {.cps: Cont.} =
      while true:
        noop()
        inc r
    expect InfiniteLoop:
      trampoline whelp(foo())
    check r > 1

  block:
    ## the noop magic smoke test demonstrates shedding scope
    var r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      inc r
    trampoline whelp(foo())
    check r == 2, "who let the smoke out?"
