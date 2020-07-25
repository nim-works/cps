import cps
import cps/eventqueue

type

  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

var r = 0
proc test(): Cont {.cps.} =
  while true:
    #cps sleep()
    if true:
      break
    r = 1
    return
run()
assert r == 0
