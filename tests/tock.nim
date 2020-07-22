import cps             # .cps. macro
import cps/eventqueue  # cps_sleep(), trampoline, run(), Cont

# a procedure that starts off synchronous and becomes asynchronous
proc tock(name: string; ms: int): Cont {.cps.} =
  var count: int = 10
  while count > 0:
    dec count
    # this primitive sends the continuation to the dispatcher
    cps_sleep ms
    # this is executed from the dispatcher
    echo name, " ", count

# the trampoline repeatedly invokes continuations...
trampoline tock("tick", 30)
# ...until they complete or are queued in the dispatcher
trampoline tock("tock", 100)

# run the dispatcher to invoke pending continuations
run()
