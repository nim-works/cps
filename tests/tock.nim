import cps
import cps/eventqueue

proc tock(name: string; ms: int) {.cps: Cont.} =
  var count = 10
  while count > 0:
    dec count
    sleep ms
    echo name, " ", count

spawn tock("tick", 300)
spawn tock("tock", 700)

run()
