# This explores way to prevent https://github.com/disruptek/cps/issues/44
# Since the continuation is a ref or mutable
# it automatically makes all the frames it holds mutable
# even if the original variable wasn't mutable.
#
# This means that the CPS transformation would lead to compiling incorrect code

import std/decls

type FakeContinuation = object
  a: int
  b: int


when false:
  proc theOriginal(a: int) =
    let b = 10 # immutable
    if true:
      b += 10 # this won't compile
      echo a + b
      return
    else:
      echo a + b
      return

proc theCPSed(c: var FakeContinuation) =
  let a {.byaddr.} = c.a
  let b {.byaddr.} = c.b

  b += 10 # Does this compile? Can we mutate through a let byaddr.
  echo a + b

var c = FakeContinuation(a: 1, b: 2)
theCPSed(c)
# It compiles :/, so {.byaddr.} is not a solution
