import std/[hashes, times]

#
# This is a performance comparison between CPS and native closure iterators.
#
when not defined(danger):
  {.error: "define danger for benchmark purposes".}

when not defined(gcArc):
  {.warning: "cps is designed for --gc:arc".}

import cps

template howLong(what, code): float =
  let start = cpuTime()
  block:
    code
  let duration = cpuTime() - start
  echo what, ": ", duration, " s"
  duration


const iterations = 1_000_000_000
var h: Hash = 0

let t1 = howLong "cps iterator":

  type Iterator = ref object of RootObj
    fn*: proc(c: Iterator): Iterator {.nimcall.}
    val: int

  proc jield(it: Iterator; val: int): Iterator {.cpsMagic.} =
    it.val = val
    return it

  proc counter(lo: int, hi: int) {.cps: Iterator.} =
    var i = lo
    while i <= hi:
      jield i
      inc i

  template finished(x: ref): bool = x == nil or x.fn == nil

  var a = whelp counter(1, iterations)
  while not finished(a):
    h = h !& hash(a.val)
    a = a.fn(a)


echo !$h
h = 0

let t2 = howLong "closure iterator":

  iterator counter(lo: int, hi: int): int {.closure.} =
    var i = lo
    while i <= hi:
      yield i
      yield i
      inc i

  let f = counter
  while not finished(f):
    h = h !& hash(f(1, iterations))

echo !$h

echo "Nim closure iterators are ", t1 / t2, " times faster"
