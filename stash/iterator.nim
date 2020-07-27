
import cps, options

type Iterator = ref object of RootObj
  fn*: proc(c: Iterator): Iterator {.nimcall.}
  val: Option[int]


proc produce(c: var Iterator): Option[int] =
  while c != nil and c.fn != nil and c.val.isNone:
    c = c.fn(c)
  if c != nil and c.val.isSome:
    let val = c.val
    c.val = none(int)
    return val
  else:
    return none(int)


proc jield(c: Iterator, val: int): Iterator =
  c.val = some(val)
  return c


# Counting iterator

proc counter(lo: int, hi: int): Iterator {.cps.} =
  var i:int = lo
  while i <= hi:
    cps jield(i)
    inc i

# Resume the iterator a bunch of times

var a = counter(3, 7)
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
