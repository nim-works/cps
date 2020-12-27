import
  cps,
  std/[options, macros]

type
  Coro[O: not void] = concept coro
    ## This implements coroutines / closure iterators
    ## using CPS
    coro.suspension is Continuation
    coro.promise is Option[O]

proc resume[O: not void](
       coro: var Coro[O]
     ): Option[O] =
  result = none(O)
  while coro.suspension != nil and
        coro.suspension.fn != nil and
        result.isNone():
    coro.suspension.fn(coro.suspension, coro.promise)
  # Coroutine and returned here
  # coro.suspension == nil at the end
  if coro.suspension != nil:
    # not nil we reached a yield point
    result = move coro.promise

type C = ref object of RootObj
  fn: proc(c: var C)

macro coro(def: untyped): untyped =
  echo def.treerepr

proc jield[O](
       result: O,
       promise: var Option[O]
     ): C {.cpsMagic.} =
  ## Send result to promise
  promise = some result

proc counter(lo: int, hi: int): int {.cps:C.} =
  var i: int = lo
  while i <= hi:
    jield(i)
    inc i
