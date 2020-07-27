import cps
import cps/eventqueue

proc b(x: int): Cont {.cps.} =
  cps noop()
  doAssert x > 0
  cps noop()
  let x: int = 3
  cps noop()
  doAssert x == 3
  cps noop()
  var y: int = 8
  block:
    cps noop()
    var x: int = 4
    cps noop()
    inc x
    cps noop()
    dec y
    cps noop()
    doAssert x == 5
    doAssert y == 7
  cps noop()
  doAssert x == 3
  cps noop()
  doAssert y == 7

proc a(x: int): Cont {.cps.} =
  cps noop()
  doAssert x > 0
  cps noop()
  doAssert x > 0
  cps noop()
  var x: int = 2
  cps noop()
  doAssert x == 2
  cps noop()
  spawn b(x)
  cps noop()
  doAssert x == 2
  cps noop()
  doAssert x == 2

spawn a(1)
run()
