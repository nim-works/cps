import cps
import cps/eventqueue

var sem = newSemaphore()
var success = false

proc signalSleeper(ms: int) {.cps: Cont.} =
  yield sleep(ms)
  signal(sem)

proc signalWaiter() {.cps: Cont.} =
  yield wait(sem)
  success = true

trampoline signalSleeper(10)
trampoline signalWaiter()

run()

if not success:
  quit(1)
