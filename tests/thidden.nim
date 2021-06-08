import balls
import cps

include preamble

suite "hidden":

  block:
    ## implicit up-casting
    type O = ref object of RootObj

    var r = 0

    proc foo() {.cps: Continuation.} =
      inc r
      let o: RootRef = new O

    foo()

    check r == 1

  block:
    ## implicit conversion between non-objects types
    var r = 0

    proc foo() {.cps: Continuation.} =
      inc r
      let n: Natural = 1
      # implicit conversion in decl
      let i: int = n
      # implicit conversion in assignment
      var j: int
      j = n

    foo()

    check r == 1
