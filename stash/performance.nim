
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps, times
  
template howLong(what, code): float =
  let start = cpuTime()
  block:
    code
  let duration = cpuTime() - start
  echo what, ": ", duration, " s"
  duration



let t1 = howLong "cps iterator":

  type Iterator = ref object of RootObj
    fn*: proc(c: Iterator): Iterator {.nimcall.}
    val: int

  proc jield(c: Iterator, val: int): Iterator =
    c.val = val
    return c

  proc counter(lo: int, hi: int) {.cps:Iterator.} =
    var i:int = lo
    while i <= hi:
      cps jield(i)
      inc i

  var a = counter(1, 10000000)
  while a != nil and a.fn != nil:
    a = a.fn(a)



let t2 = howLong "closure iterator":

  iterator counter(lo: int, hi: int): int {.closure.} =
    var i = lo
    while i <= hi:
      yield i
      inc i

  let f = counter
  while not finished(f):
    discard f(1, 10000000)


echo "Nim closure iterators are ", t1 / t2, " times faster"
