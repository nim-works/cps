# Example 1: Checking that simple CPS transform compiles
import ../cps/core
import macros

type C = ref object of RootObj
  fn: proc(c: C): C {.nimcall.}
  storage: array[64, byte]

expandMacros:
  proc foo(x: int) {.cps: C.} =
    echo "Hello World"
    return
