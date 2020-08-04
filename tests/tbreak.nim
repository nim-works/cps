import cps
import cps/eventqueue

#import testes
#testes:
import std/unittest
suite "break":
  var r = 0

  test "1":
    proc test() =
      r = 1
      check r != 1
      while true:
        if true:
          break
        inc r
        check r <= 2
        return
    test()
    checkpoint "r was " & $r
    check r == 1

  test "2":
    proc test2() {.cps:Cont.} =
      r = 1
      while true:
        cps jield()
        if true:
          break
        inc r
        if r > 2:
          quit(1)
        return
    spawn test2()
    run()
    checkpoint "r was " & $r
    check r == 1

  test "3":
    proc test3() {.cps:Cont.} =
      r = 1
      while true:
        cps noop()
        if true:
          inc r
          if r > 2:
            quit(1)
          else:
            break
      inc r
    trampoline test3()
    checkpoint "r was " & $r
    check r == 3

  test "4":
    proc test4() {.cps:Cont.} =
      r = 1
      block found:
        while true:
          cps noop()
          if r > 2:
            break found
          cps noop()
          inc r
        quit(1)
      r = r * -1
    trampoline test4()
    checkpoint "r was " & $r
    check r == -3
