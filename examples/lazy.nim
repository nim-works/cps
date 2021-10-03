
import cps, deques, macros, sugar

###########################################################################
# Lazy streams
###########################################################################

type
  Stream = ref object of Continuation
    val: int
    sIn: Stream

proc jield(s: Stream, val: int = 0): Stream {.cpsMagic.} =
  s.val = val

proc getSin(s: Stream): Stream {.cpsVoodoo.} =
  s.sIn

proc resume(s: Stream): int =
  block:
    var s = Continuation: s
    while s.running:
      s = s.fn(s)
  result = s.val

macro stream(n: untyped): untyped =
  n.addPragma nnkExprColonExpr.newTree(ident"cps", ident"Stream")
  n

template `->`(ca: Stream, b: typed): Stream =
  let cb = whelp(b)
  cb.sIn = ca
  cb

template `->`(a, b: typed): Stream =
  let ca = whelp(a)
  let cb = whelp(b)
  cb.sIn = ca
  cb


###########################################################################

proc toStream(r: Hslice[int, int]) {.stream.} =
  var i = r.a
  while i <= r.b:
    jield(i)
    inc i

proc map(fn: proc(x: int): int) {.stream.} =
  let sIn = getSin()
  while true:
    let v = fn(sIn.resume())
    if not sIn.running: break
    jield(v)

proc filter(fn: proc(x: int): bool) {.stream.} =
  let sIn = getSin()
  while true:
    let v = sIn.resume()
    if not sIn.running: break
    if fn(v):
      jield(v)

proc print() {.stream.} =
  let sIn = getSin()
  while true:
    let v = sIn.resume()
    if not sIn.running: break
    echo v
    jield()

proc pump() {.stream.} =
  let sIn = getSin()
  while sIn.running:
    discard sIn.resume()


when isMainModule:
  # create a lazy stream
  var s = Continuation:
    toStream(1..10) ->
    map(x => x * 3) ->
    filter(x => (x mod 2) == 0) ->
    print() ->
    pump()

  # lazily "run" it
  while s.running:
    s = s.fn(s)
