
import cps
import testes

type C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}

proc foo(c: C, fd: int16): C {.cpsMagic.} =
  discard

testes:

  test "int16a":
    proc test1() {.cps:C} =
      foo(1'i16)
    discard test1()
  
  test "int16b":
    proc test1() {.cps:C} =
      foo(int16(1))
    discard test1()
  
  test "int16c":
    proc test1() {.cps:C} =
      foo(1.int16)
    discard test1()
