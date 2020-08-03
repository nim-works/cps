const
  skippy {.booldefine.} = false

import std/unittest
import cps


type

  C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}


# Trampoline with count safeguard

var jumps = 0

proc run(c: C) =
  jumps = 0
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)
    inc jumps
    doAssert jumps < 1000, "Too many iterations on trampoline, looping?"

# Helper templates.

# This checks if the trampoline jumped the right number of times

template expJumps(expect: int, body: untyped) =
  body
  doAssert jumps == expect, "Trampoline jumped " & $jumps & " times, expected " & $expect

# is a primitive that keeps track of how often it is called, verify with
# `expPrims` macro

var prims = 0

proc prim(): C {.cpsMagic.} =
  inc prims
  return c

template expPrims(expect: int, body: untyped) =
  prims = 0
  body
  doAssert prims == expect, "prim was called " & $prims & " times, expected " & $expect


# Wrapper for defining a cps function and sending it to the trampoline

template runCps(body: untyped) =
  proc t() {.cps:C.} = body
  run t()


var r: int

suite "cps":

  test "nocall":
    expPrims 0: runCps:
      discard

  test "onecall":
    expPrims 1: runCps:
      cps prim()

  test "twocall":
    expPrims 2: runCps:
      cps prim()
      cps prim()

  test "if true":
    expPrims 3: runCps:
      var a: int
      cps prim()
      if true:
        cps prim()
      cps prim()

  test "if false":
    expPrims 2: runCps:
      var a: int
      cps prim()
      if false:
        cps prim()
      cps prim()

  test "if true if false":
    expPrims 3: runCps:
      cps prim()
      if true:
        cps prim()
      if false:
        cps prim()
      cps prim()

  test "nested if 1":
    expPrims 4: runCps:
      var a: int
      cps prim()
      if true:
        cps prim()
        if true:
          cps prim()
      cps prim()

  test "nested if 2":
    expPrims 3: runCps:
      var a: int
      cps prim()
      if true:
        cps prim()
        if false:
          cps prim()
      cps prim()

  test "nested if 3":
    expPrims 2: runCps:
      cps prim()
      if false:
        cps prim()
        if true:
          cps prim()
      cps prim()

  test "block1":
    expPrims 3: runCps:
      cps prim()
      block:
        cps prim()
      cps prim()

  test "while1":
    expPrims 5: runCps:
      cps prim()
      var a: int = 0
      while a < 3:
        cps prim()
        inc a
      cps prim()

  test "break1":
    expPrims 3: runCps:
      cps prim()
      while true:
        cps prim()
        break
        cps prim()
      cps prim()

  test "break2":
    expPrims 3: runCps:
      cps prim()
      block:
        cps prim()
        break
        cps prim()
        cps prim()
      cps prim()

  test "for1":
    runCps:
      var a: int = 0
      for i in 0..3:
        inc a, 1
      check a == 4

  test "for2":
    expPrims 1: runCps:
      var a: int = 0
      cps prim()
      for i in 0..3:
        inc a, 1
      check a == 4

  test "multiple variables in one var":
    runCps:
      var a, b: int16
      check $type(a) == "int16"
      check $type(b) == "int16"

  test "continue":
    expPrims 8: runCps:
      cps prim()
      var i: int = 0
      while i < 10:
        inc i
        if i < 5:
          continue
        cps prim()
      cps prim()

  test "for3":
    expPrims 1: runCps:
      var a: int = 0
      for i in 0..3:
        inc a, 1
      check a == 4
      cps prim()

  test "defer":
    expPrims 3: runCps:
      cps prim()
      defer:
        cps prim()
      cps prim()

  test "nested while":
    when skippy:
      skip()
    else:
      expPrims 100: runCps:
        var i: int
        var j: int
        while i < 10:
          inc i
          while j < 10:
            inc j
            cps prim()

  test "paper example 1":
    expPrims 2: runCps:
      var t: bool = false
      while not t:
        cps prim()
        break
        cps prim()
      cps prim()
