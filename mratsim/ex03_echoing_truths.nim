# Example 3: Testing the proposed continuation API.

import ../cps/core
import macros

var scheduler: seq[ContinuationOpaque]

proc echoingTruth(i: int) {.suspend.}=
  echo "Truth: ", $i

  scheduler.setLen(scheduler.len + 1)
  scheduler[^1] = move bindCallerContinuation()

  # assert bindCallerContinuation() = nil
  # how to enforce moves?

proc truthOrDare(lo: int, hi: int) {.resumable.} =
  var i: int = lo
  while i <= hi:
    suspendAfter echoingTruth(i)
    inc i

var a = truthOrDare(1, 4)
a.resume()

var b = scheduler.pop()
b.resume()

var c = scheduler.pop()
c.resume()
