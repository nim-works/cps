const
  strictTrampoline {.booldefine.} = false

import balls
import posix
import std/macros
#import std/unittest

import cps


type

  C = ref object of Continuation
    #fn*: proc(c: C): C {.nimcall.}
    #mom*: C


# Trampoline with count safeguard

var jumps = 0

when strictTrampoline:
  proc run(c: C) =
    jumps = 0
    var c = c
    while c != nil and c.fn != nil:
      c = c.fn(c)
      inc jumps
      doAssert jumps < 1000, "Too many iterations on trampoline, looping?"
else:
  template run(c) = c

# Helper templates.

# This checks if the trampoline jumped the right number of times

template expJumps(expect: int, body: untyped) =
  body
  doAssert jumps == expect, "Trampoline jumped " & $jumps & " times, expected " & $expect

# is a primitive that keeps track of how often it is called, verify with
# `expPrims` macro

var prims = 0

proc prim(c: C): C {.cpsMagic.} =
  inc prims
  return c

template expPrims(expect: int, body: untyped) =
  prims = 0
  body
  doAssert prims == expect, "prim was called " & $prims & " times, expected " & $expect


# Wrapper for defining a function and sending it to the trampoline

template runCps(body: untyped) =
  proc t() {.cps:C.} = body
  run t()

# We have a lot of these for the purpose of control-flow validation
{.warning[UnreachableCode]: off.}

var r: int

suite "suite, suite zevv":

  test "nocall":
    expPrims 0: runCps:
      discard

  test "onecall":
    expPrims 1: runCps:
      prim()

  test "twocall":
    expPrims 2: runCps:
      prim()
      prim()

  test "if true":
    expPrims 3: runCps:
      var a: int
      prim()
      if true:
        prim()
      prim()

  test "if false":
    expPrims 2: runCps:
      var a: int
      prim()
      if false:
        prim()
      prim()

  test "if true if false":
    expPrims 3: runCps:
      prim()
      if true:
        prim()
      if false:
        prim()
      prim()

  test "nested if 1":
    expPrims 4: runCps:
      var a: int
      prim()
      if true:
        prim()
        if true:
          prim()
      prim()

  test "nested if 2":
    expPrims 3: runCps:
      var a: int
      prim()
      if true:
        prim()
        if false:
          prim()
      prim()

  test "nested if 3":
    expPrims 2: runCps:
      prim()
      if false:
        prim()
        if true:
          prim()
      prim()

  test "block1":
    expPrims 3: runCps:
      prim()
      block:
        prim()
      prim()

  test "while1":
    expPrims 5: runCps:
      prim()
      var a: int = 0
      while a < 3:
        prim()
        inc a
      prim()

  test "break1":
    expPrims 3: runCps:
      prim()
      while true:
        prim()
        break
        prim()
      prim()

  test "break2":
    expPrims 3: runCps:
      prim()
      block:
        prim()
        break
        prim()
        prim()
      prim()

  test "for1":
    runCps:
      var a: int = 0
      for i in 0..3:
        inc a, 1
      check a == 4

  test "for2":
    expPrims 1: runCps:
      var a: int = 0
      prim()
      for i in 0..3:
        inc a, 1
      check a == 4

  test "multiple variables in one var":
    runCps:
      var a, b: int16
      check $type(a) == "int16"
      check $type(b) == "int16"

  test "wrongreturn":
    runCps:
      var n = 0
      while n == 0:
        echo "one ", n
        let s = len("")
        inc n

  test "continue":
    expPrims 8: runCps:
      prim()
      var i: int = 0
      while i < 10:
        inc i
        if i < 5:
          continue
        prim()
      prim()

  test "for3":
    expPrims 1: runCps:
      var a: int = 0
      for i in 0..3:
        inc a, 1
      check a == 4
      prim()

  test "defer":
    when true:
      skip "pending try-finally rewrite"
    else:
      expPrims 3: runCps:
        prim()
        defer:
          prim()
        prim()

  test "nested while":
    expPrims 100: runCps:
      var i: int
      var j: int
      while i < 10:
        inc i
        j = 0
        while j < 10:
          inc j
          prim()

  test "paper example 1":
    expPrims 2: runCps:
      var t: bool = false
      while not t:
        prim()
        break
        prim()
      prim()

  proc foo(c: C, fd: int16): C {.cpsMagic.} =
    discard

  test "int":
    proc test1() {.cps:C} =
      foo(1)
    test1()

  test "'i16":
    proc test1() {.cps:C} =
      foo(1'i16)
    test1()

  test "int16()":
    proc test1() {.cps:C} =
      foo(int16(1))
    test1()

  test ".int16":
    proc test1() {.cps:C} =
      foo(1.int16)
    test1()

  test "type problem":
    type Thing = distinct int
    proc foo(): Thing = 1.Thing
    runCps:
      var a = foo()

  test "Running -> Lampable -> Done":

    proc running(c: C): bool = c != nil and c.fn != nil
    proc dismissed(c: C): bool = c == nil
    proc done(c: C): bool = c != nil and c.fn == nil

    var save: C

    proc jield(c: C): C {.cpsMagic.} =
      save = c

    proc count() {.cps:C.} =
      var i = 0
      while i < 2:
        jield()
        echo i
        inc i

    var c = whelp count()
    c = c.fn(c) # boot
    check c.state == Running
    c = c.fn(c) # first jield
    check c.state == Dismissed
    c = save
    check c.state == Running
    c = c.fn(c) # echo
    check c.state == Running
    c = c.fn(c) # second jield
    check c.state == Dismissed
    c = save
    check c.state == Running
    c = c.fn(c) # echo
    check c.state == Running
    c = c.fn(c) # done
    check c.state == Finished

