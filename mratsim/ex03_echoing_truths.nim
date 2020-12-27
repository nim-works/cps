import ../cps/core

type
  ContinuationErased = object
    ## Gimme gimme gimme a VTable after midnight
    fn: proc(c: var ContinuationErased) {.nimcall.}
    frame: ref RootObj

var scheduler: seq[ContinuationErased]

proc echoingTruth(cont: var Continuation, i: int) {.cpsMagic.}=
  echo "Truth: ", $i

  scheduler.setLen(scheduler.len + 1)
  scheduler[^1] = move cast[var ContinuationErased](cont.addr)

  assert cont.fn.isNil

proc truthOrDare(lo: int, hi: int) {.resumable.} =
  var i: int = lo
  while i <= hi:
    echoingTruth(i)
    inc i
