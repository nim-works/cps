import ../cps/core

var scheduler: seq[ContinuationOpaque]

proc echoingTruth(cont: var Continuation, i: int) {.cpsMagic.}=
  echo "Truth: ", $i

  scheduler.setLen(scheduler.len + 1)
  scheduler[^1] = move cont.typeEraser()

  assert cont.fn.isNil

proc truthOrDare(lo: int, hi: int) {.resumable.} =
  var i: int = lo
  while i <= hi:
    echoingTruth(i)
    inc i


let a = truthOrDare(1, 4)
