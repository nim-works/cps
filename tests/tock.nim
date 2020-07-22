import std/times

import cps             # .cps. macro
import cps/eventqueue  # cps_sleep(), trampoline, run(), Cont

# a procedure that starts off synchronous and becomes asynchronous
proc tock(name: string; interval: Duration): Cont {.cps.} =
  var count: int = 20
  while count > 0:
    dec count
    # this primitive sends the continuation to the dispatcher
    cps_sleep interval
    # this is executed from the dispatcher
    echo name, " ", count

# the trampoline repeatedly invokes continuations...
trampoline tock("tick", initDuration(milliseconds = 300))
# ...until they complete or are queued in the dispatcher
trampoline tock("tock", initDuration(seconds = 1))

# run the dispatcher to invoke pending continuations
run()
