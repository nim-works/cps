
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps, tables


type
  C = ref object of Continuation
    labels: Table[string, Continuation.fn]

# Define the CPS magic 'label' and 'goto' procs

proc label(c: C, id: string): C {.cpsMagic.} =
  c.labels[id] = c.fn
  return c

proc goto(c: C, id: string): C {.cpsMagic.} =
  c.fn = c.labels[id]
  result = c


# A  little function with gotos

proc foo() {.cps:C.} =
  echo "one"
  label"here"
  echo "two"
  echo "three"
  goto"here"
  echo "four"


# Trampoline

var x = 0
var c: Continuation = whelp foo()
while c.running and x < 100:
  c = c.fn(c)
  inc x
