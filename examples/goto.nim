
#
# This is an example implementation for a basic CPS-based iterator.
#

import cps, tables


type
  C = ref object of Continuation
    setjmps: Table[string, Continuation.fn]
    val: int

# Define the CPS magic 'setjmp' and 'longjmp' procs

proc setjmp(c: C, id: string): int {.cpsVoodoo.} =
  c.setjmps[id] = c.fn
  c.val

proc longjmp(c: C, id: string, val: int): C {.cpsMagic.} =
  c.val = val
  c.fn = c.setjmps[id]
  c


# A  little function with gotos

proc foo() {.cps:C.} =
  echo "call setjmp"
  let r = setjmp "label"
  echo "setjmp returned ", r
  echo "something else"
  echo "calling longjmp"
  longjmp "label", 5
  echo "the end"


# Trampoline

var c = whelp foo()
var x = 0
trampolineIt c:
  if x > 3:
    break
  inc x
