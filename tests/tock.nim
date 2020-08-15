import cps
import cps/eventqueue

proc tock(name: string; ms: int) {.cps: Cont.} =
  var count: int = 10
  while count > 0:
    dec count
    sleep(ms)
    echo name, " ", count

tock_clyybber("tick", 300)
tock_clyybber("tock", 700)

run()
