import pkg/balls

import pkg/cps except trampoline

type
  EmptyLoop = CatchableError
  InfiniteLoop = CatchableError
  Cont* = ref object of Continuation

var jumps: int

proc trampoline[T: Continuation](c: sink T) {.used.} =
  jumps = 0
  var c: Continuation = move c
  while c.running:
    # capture the exception in the environment
    let exception = getCurrentException()
    try:
      var y = c.fn
      var x = y(c)
      c = x
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
