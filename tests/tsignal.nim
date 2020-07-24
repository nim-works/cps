import cps
import cps/eventqueue

var sem = newSemaphore()
var success = false

proc tick(ms: int): Cont {.cps.} =
  cpsSleep ms
  cpsSignal(sem)

proc tock(): Cont {.cps.} =
  cpsWait sem
  success = true

trampoline tick(10)
trampoline tock()

run()
