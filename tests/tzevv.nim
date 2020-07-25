
import std/unittest
import cps


type
  
  C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}


var count = 0

proc addOne(): C {.cpsMagic.} = 
  inc count
  return c


proc run(c: C) =
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)


# Shortcut for creating a cps function and running it, and verify
# if the number of calls to addOne matches
template countPrims(expect: int, body: untyped) =
  count = 0
  proc t(): C {.cps} =
    body
  run t()
  doAssert count == expect

# Disabled test, needs to be fixed
template countPrimsSkip(expect: int, body: untyped) =
  skip()


suite "cps":
  
  test "nocall":
    countPrims 0:
      discard
  
  test "onecall":
    countPrims 1:
      cps addOne()

  test "twocall":
    countPrims 2:
      cps addOne()
      cps addOne()

  test "if true":
    countPrims 3:
      var a: int
      cps addOne()
      if true:
        cps addOne()
      cps addOne()
  
  test "if false":
    countPrims 2:
      var a: int
      cps addOne()
      if false:
        cps addOne()
      cps addOne()
 
  test "if true if false":
    countPrims 3:
      cps addOne()
      if true:
        cps addOne()
      if false:
        cps addOne()
      cps addOne()

  test "nested if 1":
    countPrims 4:
      var a: int
      cps addOne()
      if true:
        cps addOne()
        if true:
          cps addOne()
      cps addOne()
  
  test "nested if 2":
    countPrims 3:
      var a: int
      cps addOne()
      if true:
        cps addOne()
        if false:
          cps addOne()
      cps addOne()
  
  test "nested if 3":
    countPrims 2:
      var a: int
      cps addOne()
      if false:
        cps addOne()
        if true:
          cps addOne()
      cps addOne()

  test "block1":
    countPrims 3:
      cps addOne()
      block:
        cps addOne()
      cps addOne()
       
#  test "variable juggling":
#    countPrims:
#      var a: int
#      cps addOne()
#      var b: int = a
#      cps addOne(b)
#      doAssert a == 1
#      doAssert b == 2
#     
#  test "while1":
#    countPrims:
#      var a: int
#      cps addOne()
#      while false:
#        cps addOne()
#      doAssert a == 1
#     
#  test "while2":
#    countPrims:
#      var a: int
#      cps addOne()
#      var b: int
#      while b < 2:
#        cps addOne()
#        inc b
#      doAssert a == 3
#  
#  test "while2":
#    countPrims:
#      var a: int
#      cps addOne()
#      var b: int
#      while b < 2:
#        cps addOne()
#        inc b
#      doAssert a == 3
#  
#  test "break1":
#    countPrims:
#      var a: int
#      cps addOne()
#      block:
#        break
#        cps addOne()
#      doAssert a == 1
#      done = true
#
#  test "break2":
#    countPrimsSkip:
#      var a: int
#      cps addOne()
#      block:
#        cps addOne()
#        break
#        cps addOne()
#      doAssert a == 2
#  
#  test "stuff1":
#    countPrimsSkip:
#      var a: int
#      cps addOne()
#      var b: int
#      while true:
#        cps addOne()
#      doAssert a == 3
#  
#
