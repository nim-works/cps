
#
# This is an example implementation for a basic CPS-based iterator.
#

# Assumes cpsMutant

import cps, options

# This is our iterator type. It holds the continuation function
# and an Option[int] to pass the last produced value

type Iterator = ref object of RootObj
  fn*: proc(c: var Iterator) {.nimcall.}
  val: Option[int]

# The `produce` proc is called to pump the iterator. It will trampoline the
# continuation until a value is available in `val`.

proc produce(c: var Iterator): Option[int] =
  while c != nil and c.fn != nil and c.val.isNone:
    c.fn(c)
  if c != nil and c.val.isSome:
    result = c.val
    c.val = none(int)

# The `jield` proc is cps magic to generate a new value from within an
# interator

proc jield(c: var Iterator, val: int) =
  c.val = some(val)

# A simple counting iterator, will produce all integers from 'lo' to 'high',
# inclusive

proc counter(lo: int, hi: int) {.cps:Iterator.} =
  var i:int = lo
  while i <= hi:
    cps jield(i)
    inc i


# Create an instance of the iterator, counting from 3 up to 7

var a = counter(3, 7)


# Resume the iterator a bunch of times

echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
