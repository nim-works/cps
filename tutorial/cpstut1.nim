
# Baby steps: my first cps program

import cps
import deques

type
  MyCont = ref object of Continuation

proc hello() {.cps:MyCont.} =
  echo "Hello, world!"

var c: Continuation = whelp hello()

doAssert c.running()

c = c.fn(c)

doAssert c.finished
