# Example 6: Testing the proposed coroutine API.

import ../cps/core, options, macros

# A simple counting iterator, will produce all integers from 'lo' to 'high',
# inclusive

proc counter(lo: int, hi: int): int {.coro.} =
  var i: int = lo
  while i <= hi:
    coroYield i
    inc i

var a = counter(3, 7)
echo "produced ", a.pull()
echo "produced ", a.pull()
echo "produced ", a.pull()
echo "produced ", a.pull()
echo "produced ", a.pull()
echo "produced ", a.pull()
