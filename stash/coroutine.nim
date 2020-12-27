
import cps, options

type Coro = ref object of RootObj
  fn*: proc(c: Coro): Coro {.nimcall.}
  val: Option[int]

proc resume(c: var Coro): Option[int] =
  while c != nil and c.fn != nil and c.val.isNone:
    c = c.fn(c)
  if c != nil and c.val.isSome:
    let val = c.val
    c.val = none(int)
    return val
  else:
    return none(int)


proc jield(c: Coro, val: int): Coro =
  c.val = some(val)
  return c


# Counting iterator

proc counter(lo: int, hi: int) {.cps: Coro.} =
  var i: int = lo
  while i <= hi:
    cps jield(i)
    inc i

# Resume the iterator a bunch of times

var a = counter(3, 7)
echo "resumed ", a.resume()
echo "resumed ", a.resume()
echo "resumed ", a.resume()
echo "resumed ", a.resume()
echo "resumed ", a.resume()
echo "resumed ", a.resume()
