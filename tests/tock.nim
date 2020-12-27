import cps/core
import cps/schedulers

proc tock(name: string; ms: int) {.cps: Cont.} =
  var count: int = 10
  while count > 0:
    dec count
    sleep(ms)
    echo name, " ", count

spawn tock("tick", 300)
spawn tock("tock", 700)

run()
