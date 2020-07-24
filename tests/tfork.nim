import cps
import cps/eventqueue

var r: int

proc adder(): Cont {.cps.} =
  cpsFork()
  inc r

cpsSpawn adder()
run()
if r != 2:
  raise newException(Defect, "uh oh")
