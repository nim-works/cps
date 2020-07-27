
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps, tables


type Tc = ref object of RootObj
  fn*: proc(c: Tc): Tc {.nimcall.}

var labels: Table[string, Tc]


# Define the CPS magic 'label' and 'goto' procs

proc label(c: Tc, id: string): Tc =
  labels[id] = c
  return c

proc goto(c: Tc, id: string): Tc =
  return labels[id]


# A  little function with gotos

proc foo(): Tc {.cps.} =
  echo "one"
  cps label("here")
  echo "two"
  echo "three"
  cps goto("here")
  echo "four"


# Trampoline

var c = foo()
while c != nil and c.fn != nil:
  c = c.fn(c)
