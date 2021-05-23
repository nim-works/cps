
###########################################################################
#
# Lua-style assymetrical coroutines
#
# resume(co, val):
#
#   Starts or continues the execution of coroutine co. The first time you
#   resume a coroutine, it starts running its body. If the coroutine has
#   yielded, resume() restarts it; the value `val` is passed as the results
#   from the yield().
#
#  send(val):
#  yield():
#
#   Suspends the execution of the calling coroutine. The value `val`
#   passed to sent is returned as results from resume().
#
###########################################################################

import cps, options, deques

type
  Coroutine = ref object of RootObj
    fn*: proc(c: Coroutine): Coroutine {.nimcall.}
    val: int

# Magic procs for yielding and receiving. Note: we actually want
# to have yield() and receive() in one single operation so we can
# do `vlaOut = yield(valIn)`

proc jield(c: Coroutine, val: int): Coroutine {.cpsMagic.} =
  c.val = val

proc recv(c: Coroutine): int {.cpsMagic.} =
  return c.val

proc resume(c: Coroutine, val: int): int =
  c.val = val
  discard c.trampoline()
  return c.val

# This coroutine calculates the running total of the passed numbers

proc fn_coro1() {.cps:Coroutine.} =
  var sum = 0
  while true:
    sum += recv()
    jield(sum)

var coro = fn_coro1()

for i in 0..10:
  echo coro.resume(i)
