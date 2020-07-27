import cps
import cps/eventqueue

var sem = newSemaphore()
var success = false

proc tick(ms: int) {.cps:Cont.} =
  cps sleep(ms)
  signal(sem)

proc tock() {.cps:Cont.} =
  cps wait(sem)
  success = true

trampoline tick(10)
trampoline tock()

run()
