import ../cps/core
import macros

var scheduler: seq[ContinuationOpaque]

proc echoingTruth(i: int) {.suspend.}=
  echo "Truth: ", $i

  scheduler.setLen(scheduler.len + 1)
  scheduler[^1] = move bindCallerContinuation().typeEraser()

  # assert bindCallerContinuation() = nil
  # how to enforce moves?

proc truthOrDare(lo: int, hi: int) {.resumable.} =
  var i: int = lo
  while i <= hi:
    # TODO: "suspendAfter echoingTruthWorking(i)"
    echoingTruth(i)
    inc i

let a = truthOrDare(1, 4)
