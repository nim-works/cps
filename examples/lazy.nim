import std/deques
import std/macros
import std/sugar

import pkg/cps

###########################################################################
# Lazy streams
###########################################################################

type
  Stream = ref object of Continuation  ## a stream chains transformations...
    val: int                           ## ...of integers.
    next: Stream

macro stream(n: untyped): untyped =
  ## prettify stream api
  n.addPragma nnkExprColonExpr.newTree(ident"cps", ident"Stream")
  n

proc jield(s: Stream; val: int = 0): Stream {.cpsMagic.} =
  ## emit a value into the stream
  s.val = val

proc next(s: Stream): var Stream {.cpsVoodoo.} =
  ## the next stream in the chain
  s.next

proc run(s: var Stream): int {.discardable.} =
  ## run a stream to produce a value
  discard trampoline s
  result = s.val

template `->`(ca: Stream, b: typed): Stream =
  ## composition operator
  let cb = whelp(b)
  cb.next = ca
  cb

template `->`(a, b: typed): Stream =
  ## composition operator
  let ca = whelp(a)
  let cb = whelp(b)
  cb.next = ca
  cb


###########################################################################

proc toStream(slice: Slice[int]) {.stream.} =
  ## turn any slice into a lazy stream
  var i = slice.a
  while i <= slice.b:
    jield i
    inc i

proc map(fn: proc(x: int): int) {.stream.} =
  ## lazily apply a function to a stream
  while true:
    let v = fn: run next()
    if next().finished: break
    jield(v)

proc filter(fn: proc(x: int): bool) {.stream.} =
  ## lazily apply a predicate to a stream
  while true:
    let v = run next()
    if next().finished: break
    if fn(v):
      jield(v)

proc print() {.stream.} =
  ## lazily echo a stream
  while true:
    let v = run next()
    if next().finished: break
    echo v
    jield()

proc pump() {.stream.} =
  ## iterate the stream processor
  while next().running:
    run next()


when isMainModule:
  # create a lazy stream
  var s =
    toStream(1..50) ->
    map(x => x * 3) ->
    filter(x => (x mod 2) == 0) ->
    print() ->
    pump()

  # lazily run it
  run s
