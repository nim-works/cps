import cps
import cps/eventqueue

proc tock(name: string; ms: int) {.cps: Cont.} =
  var count: int = 10
  while count > 0:
    dec count
    yield sleep(ms)
    echo name, " ", count

trampoline tock("tick", 30)
trampoline tock("tock", 100)

run()
