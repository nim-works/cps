
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps/core, options

# This is our iterator type. It holds the continuation function
# and an Option[int] to pass the last produced value

type Iterator[T] = ref object of RootObj
  fn*: proc(c: Iterator[T]): Iterator[T] {.nimcall.}
  val: Option[T]

# The `produce` proc is called to pump the iterator. It will trampoline the
# continuation until a value is available in `val`.

proc produce[T](c: var Iterator[T]): Option[T] =
  while c != nil and c.fn != nil and c.val.isNone:
    c = c.fn(c)
  if c != nil and c.val.isSome:
    result = c.val
    c.val = none(T)


# The `jield` proc is cps magic to generate a new value from within an
# interator

proc jield[T](c: Iterator[T], val: T): Iterator[T] =
  c.val = some(val)
  return c


# A simple counting iterator, will produce all integers from 'lo' to 'high',
# inclusive

proc counter[T](lo: T, hi: T) {.cps:Iterator[T].} =
  var i: T = lo
  while i <= hi:
    cps jield(i)
    inc i


# Create an instance of the iterator, counting from 3 up to 7

var a = counter[int](3, 7)


# Resume the iterator a bunch of times

var v = a.produce()

echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
echo "produced ", a.produce()
