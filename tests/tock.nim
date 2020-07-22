import std/times

import cps
import cps/eventqueue

proc tock(name: var string; interval: Duration): Cont {.cps.} =
  var count: int = 0
  while true:
    inc count
    cps_sleep interval
    echo name, " ", count

trampoline tock("tick", initDuration(milliseconds = 300))
trampoline tock("tock", initDuration(milliseconds = 700))

run()
