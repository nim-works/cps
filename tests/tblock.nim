import cps
import cps/eventqueue

var r = 0
proc test1() {.cps:Cont.} =
  r = 1
  echo "pre block"
  block:
    echo "pre if, in block"
    if true:
      echo "in if"
      inc r
      break
    echo "post if"
    quit(1)
    echo "leaving block"
  echo "tail"
  inc r
trampoline test1()
if r != 3:
  echo "r for test1 wasn't 3: ", r
  quit(1)

proc test2() {.cps:Cont.} =
  r = 1
  block:
    if true:
      yield noop()
      inc r
      break
    quit(1)
  inc r
trampoline test2()
if r != 3:
  echo "r for test2 wasn't 3: ", r
  quit(1)
