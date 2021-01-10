
import cps
import testes
import epoll
import posix

type C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}

proc foo(c: C, fd: int16): C {.cpsMagic.} =
  discard


testes:
  
  test "int":
    proc test1() {.cps:C} =
      foo(1)
    discard test1()

  test "'i16":
    proc test1() {.cps:C} =
      foo(1'i16)
    discard test1()
  
  test "int16()":
    proc test1() {.cps:C} =
      foo(int16(1))
    discard test1()
  
  test ".int16":
    proc test1() {.cps:C} =
      foo(1.int16)
    discard test1()
  
