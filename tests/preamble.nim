import balls

import cps except trampoline

type
  EmptyLoop = CatchableError
  InfiniteLoop = CatchableError
  Cont* = ref object of Continuation

var jumps: int

proc trampoline[T: Continuation](c: T) =
  jumps = 0
  var c = Continuation c
  while c.running:
    # capture the exception in the environment
    let exception = getCurrentException()
    try:
      c = c.fn(c)
    except:
      writeStackFrames c
      raise
    # the current exception should not change
    check getCurrentException() == exception
    inc jumps
    if jumps > 1000:
      raise InfiniteLoop.newException: $jumps & " iterations"
  if jumps == 0:
    raise EmptyLoop.newException:
      "continuations test best when they, uh, bounce"

proc noop*(c: Cont): Cont {.cpsMagic.} = c
proc dismiss*(c: Cont): Cont {.cpsMagic.} = nil

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

template shouldRun(wanted: int; body: untyped) {.used.} =
  var measured {.inject.} = 0
  try:
    body
  finally:
    check measured == wanted:
      if wanted == 0:         "oops; continuation ran"
      elif measured == 0:     "continuation never ran"
      elif measured > wanted: "continuation ran too often"
      else:                   "continuation ran too rarely"

template ran {.used, dirty.} = inc measured
