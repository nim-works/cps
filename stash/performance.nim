import std/[hashes, times, monotimes]

#
# This is a performance comparison between CPS and native closure iterators.
#
when not defined(danger):
  {.error: "define danger for benchmark purposes".}

when not defined(gcArc):
  {.warning: "cps is designed for --gc:arc".}

import cps

template howLong(what, code): Duration =
  let start = getMonoTime()
  block:
    code
  let duration = getMonoTime() - start
  echo what, ": ", duration
  duration

const iterations = 1_000_000_000
var h: Hash = 0

let t1 = howLong "cps iterator":

  type
    Iterator = ref object of Continuation
      yielded: bool
      val: int

  proc jield(it: Iterator; val: int): Iterator {.cpsMagic.} =
    it.yielded = true
    it.val = val
    return it

  proc next(it: var Iterator): int =
    while true:
      it = Iterator it.fn(it)
      if it.finished: break
      if it.yielded:
        it.yielded = false
        return it.val

  proc counter(lo: int, hi: int) {.cps: Iterator.} =
    var i = lo
    while i <= hi:
      jield i
      inc i

  var a = Iterator: whelp counter(1, iterations)
  while true:
    let next = a.next()
    if a.finished: break
    h = h !& hash(next)

echo !$h
h = 0

let t2 = howLong "closure iterator":

  proc counter(lo: int, hi: int): iterator(): int =
    result =
      iterator (): int =
        var i = lo
        while i <= hi:
          yield i
          inc i

  let f = counter(1, iterations)
  while true:
    let next = f()
    if f.finished: break
    h = h !& hash(next)

echo !$h

let ratio = t2.inNanoseconds.float / t1.inNanoseconds.float
if ratio < 1:
  echo "Nim closure iterators are ", 1 / ratio, " times faster"
else:
  echo "Nim closure iterators are ", ratio, " times slower"
