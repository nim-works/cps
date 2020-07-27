import cps
import cps/eventqueue

proc b(x: int) {.cps:Cont.} =
  doAssert x > 0
  let x: int = 3
  doAssert x == 3
  var y: int = 8
  block:
    var x: int = 4
    inc x
    dec y
    doAssert x == 5
    doAssert y == 7
  doAssert x == 3
  doAssert y == 7

proc a(x: int) {.cps:Cont.} =
  doAssert x > 0
  doAssert x > 0
  doAssert x == 1
  let x: int = 2
  doAssert x == 2
  spawn b(x)
  doAssert x == 2
  doAssert x == 2

spawn a(1)
run()
