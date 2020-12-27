
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps/core, tables


type
  TcFn = proc(c: Tc): Tc {.nimcall.}

  Tc = ref object of RootObj
    fn: TcFn
    labels: Table[string, TcFn]


# Define the CPS magic 'label' and 'goto' procs

proc label(c: Tc, id: string): Tc =
  c.labels[id] = c.fn
  return c

proc goto(c: Tc, id: string): Tc =
  c.fn = c.labels[id]
  result = c


# A  little function with gotos

proc foo() {.cps:Tc.} =
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
