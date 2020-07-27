
import cps

type Iterator = ref object of RootObj
  fn*: proc(c: Iterator): Iterator {.nimcall.}
  haveVal: bool
  val: int


proc resume(c: var Iterator): (int, bool) =
  while c != nil and c.fn != nil and not c.haveVal:
    c = c.fn(c)
  if c != nil and c.haveVal:
    c.haveVal = false
    return (c.val, true)
  else:
    return (0, false)


proc jield(c: Iterator, val: int): Iterator =
  c.val = val
  c.haveVal = true
  return c


# Counting iterator

proc counter(lo: int, hi: int): Iterator {.cps.} =
  var i:int = lo
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
