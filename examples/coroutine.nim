# Import cps and short `=>` syntax for anonymous functions
import cps, std/sugar

type
  Coroutine = ref object of Continuation
    data: int
    next: Coroutine

# Used to both launch and continue the execution of coroutines
proc resume(c: Coroutine): Coroutine =
  var c = Continuation c
  while c.running:
    c = c.fn(c)
  result = Coroutine c

proc recv(c: Coroutine): int {.cpsVoodoo.} =
  c.data

# Suspend execution of the coroutine
proc jield(c: Coroutine): Coroutine {.cpsMagic.} =
  c.next = c
  return nil

proc send(c: Coroutine, n: int) =
  c.data = n
  discard c.next.resume()

# This coroutine receives the data, applies f and sends the result to consumer
proc filter(dest: Coroutine, f: proc(x: int): int) {.cps:Coroutine.} =
  while true:
    jield()
    let n = f(recv())
    dest.send(n)

# This coroutine receives ints through filter and prints them
proc consumer() {.cps:Coroutine.} =
  while true:
    jield()
    let value = recv()
    echo value

let coro2 = whelp consumer()
let coro1 = whelp filter(coro2, x => x * 2)

discard coro1.resume()
discard coro2.resume()

# This prints numbers from 2 to 20 in 2 increment.
for i in 1..10:
  coro1.send(i)
