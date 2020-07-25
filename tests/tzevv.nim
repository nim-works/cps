
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
  var i = 0
  while c != nil and c.fn != nil:
    c = c.fn(c)
    inc i
    doAssert i < 1000, "Too many iterations on trampoline, looping?"


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
      echo "Done"
       
  test "while1":
    countPrims 5:
      cps addOne()
      var a: int = 0
      while a < 3:
        cps addOne()
        inc a
      cps addOne()
  
  test "break1":
    countPrims 3:
      cps addOne()
      while true:
        cps addOne()
        break
        cps addOne()
      cps addOne()
     
  test "nested while":
    countPrims 100:
      var i: int
      var j: int
      while i < 10:
        inc i
        while j < 10:
          inc j
  
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
