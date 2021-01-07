#
# This is a performance comparison between CPS and native closure iterators.
#
when not defined(danger):
  {.error: "define danger for benchmark purposes".}

when not defined(gcArc):
  {.warning: "cps is designed for --gc:arc".}

import cps
import criterion

var cfg = newDefaultConfig()
cfg.brief = false

const
  loops = 100

type Iterator = ref object of RootObj
  fn*: proc(c: Iterator): Iterator {.nimcall.}
  val: int

proc jield(it: Iterator; val: int): Iterator {.cpsMagic.} =
  it.val = val
  return it

proc cps_counter(lo: int, hi: int) {.cps: Iterator.} =
  var i = lo
  while i <= hi:
    jield i
    inc i

iterator closure_counter(lo: int, hi: int): int {.closure.} =
  var i = lo
  while i <= hi:
    yield i
    inc i

benchmark cfg:
  proc cps_iterator() {.measure.} =
    var a = cps_counter(1, loops)
    while a != nil and a.fn != nil:
      a = a.fn(a)

  proc closure_iterator() {.measure.} =
    let f = closure_counter
    while not finished(f):
      discard f(1, loops)
