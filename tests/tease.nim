import std/macros
import std/os

import testes

import cps
import cps/eventqueue

testes:
  var r = 0

  proc adder(x: var int) =
    inc x

  block trampoline:
    proc foo() {.cps:Cont.} =
      r = 1
    trampoline foo()
    check r == 1

  block yield_magic:
    proc foo() {.cps:Cont.} =
      yield jield()
    trampoline foo()

  block noop_magic:
    var noopJ = 2
    proc foo() {.cps:Cont.} =
      var i: int = 3
      noopJ = 4
      cps noop()
      check i == 3
    trampoline foo()
    check noopJ == 4

  block sleep_magic:
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 3:
        cps sleep(i + 1)
        adder(i)
      r = i
      check r == 3
    trampoline foo()

  block:
    ## shadowing and proc param defaults
    ## https://github.com/disruptek/cps/issues/22
    when true:
      skip("broken until scopes are improved")
    else:
      proc foo(a, b, c: int = 3) {.cps: Cont.} =
        ## a=1, b=2, c=3
        var a: int = 5
        ## a=5, b=2, c=3
        cps noop()
        ## a=5, b=2, c=3
        var b: int = b + a
        ## a=5, b=7, c=3
        cps noop()
        ## a=5, b=7, c=3
        check a == 5
        check b == 7
        check c == 3
      trampoline foo(1, 2)

  block:
    ## reassignment of var proc params
    ## https://github.com/disruptek/cps/issues/22 (2nd)
    proc foo(a, b, c: var int) {.cps: Cont.} =
      a = 5
      cps noop()
      b = b + a
      cps noop()
      check a == 5
      check b == 7
      check c == 3
    var (x, y, z) = (1, 2, 3)
    trampoline foo(x, y, z)

  block:
    ## multiple variable declaration
    ## https://github.com/disruptek/cps/issues/16
    ## this is the test of `var i, j, k: int = 0`
    proc foo() {.cps: Cont.} =
      var i, j, k: int = 0
      j = 5
      var p: int
      var q: int = 0
      var r: int = j
      cps jield()
      inc i
      inc j
      inc k
      inc p
      inc q
      inc r
    trampoline foo()

  block:
    ## declaration via tuple deconstruction
    ## https://github.com/disruptek/cps/issues/15
    when true:
      skip("broken until cps macro is typed")
    else:
      proc foo() {.cps: Cont.} =
        var (i, j, k) = (1, 2, 3)
        cps noop()
        check i == 1
        check j == 2
        check k == 3
      trampoline foo()

  block:
    ## simple block with break
    proc test1() {.cps:Cont.} =
      r = 1
      block:
        if true:
          inc r
          break
        assert false
      inc r
    trampoline test1()
    if r != 3:
      echo "r for test1 wasn't 3: ", r
      assert false

  block:
    ## block with yield and break
    proc test2() {.cps:Cont.} =
      r = 1
      block:
        if true:
          yield noop()
          inc r
          break
        assert false
      inc r
    trampoline test2()
    if r != 3:
      echo "r for test2 wasn't 3: ", r
      assert false

  block:
    ## semaphores
    var sem = newSemaphore()
    var success = false

    proc signalSleeper(ms: int) {.cps: Cont.} =
      yield sleep(ms)
      signal(sem)

    proc signalWaiter() {.cps: Cont.} =
      yield wait(sem)
      success = true

    trampoline signalSleeper(10)
    trampoline signalWaiter()

    run()

    if not success:
      raise newException(AssertionDefect, "signal failed")

  block:
    ## break statements without cps 🥴
    proc break1() =
      r = 1
      check r == 1
      while true:
        if true:
          break
        inc r
        check r <= 2
        return
    break1()
    assert r == 1, "r was " & $r

  block:
    ## a fairly tame cps break
    proc break2() {.cps:Cont.} =
      r = 1
      while true:
        cps jield()
        if true:
          break
        inc r
        if r > 2:
          assert false
        return
    spawn break2()
    run()
    assert r == 1, "r was " & $r

  block:
    ## break in a nested else (don't ask)
    proc break3() {.cps:Cont.} =
      r = 1
      while true:
        cps noop()
        if true:
          inc r
          if r > 2:
            assert false
          else:
            break
      inc r
    trampoline break3()
    assert r == 3, "r was " & $r

  block:
    ## named breaks
    proc break4() {.cps:Cont.} =
      r = 1
      block found:
        while true:
          cps noop()
          if r > 2:
            break found
          cps noop()
          inc r
        assert false
      r = r * -1
    trampoline break4()
    assert r == -3, "r was " & $r

  block:
    ## while statement
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 2:
        let x: int = i
        adder(i)
        assert x < i
        check x < i
      r = i
      check r == 2
    trampoline foo()

  block:
    ## while with break
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while true:
        let x: int = i
        adder(i)
        if i >= 2:
          break
        assert x < i
        check x < i
      r = i
      check r == 2
    trampoline foo()

  block:
    ## while with continue
    proc foo() {.cps:Cont.} =
      var i: int = 0
      while i < 2:
        let x: int = i
        adder(i)
        if x == 0:
          continue
        assert x > 0
      r = i
      check r == 2
    trampoline foo()

  block:
    ## simple name shadowing test
    proc b(x: int) {.cps:Cont.} =
      doAssert x > 0
      let x: int = 3
      doAssert x == 3
      var y: int = 8
      block:
        var x: int = 4
        inc x
        dec y
        doAssert x == 5
        doAssert y == 7
      doAssert x == 3
      doAssert y == 7

    proc a(x: int) {.cps:Cont.} =
      doAssert x > 0
      doAssert x > 0
      doAssert x == 1
      let x: int = 2
      doAssert x == 2
      spawn b(x)
      doAssert x == 2
      doAssert x == 2

    spawn a(1)
    run()

  block:
    ## for loop with continue, break
    proc test() {.cps:Cont.} =
      r = 1
      while true:
        for i in 0 .. 3:
          if i == 0:
            continue
          if i > 2:
            break
          r = r + i
        inc r
        if r == 5:
          break
      inc r
    trampoline test()
    assert r == 6, "r is " & $r

  block:
    ## fork
    when not declaredInScope(fork):
      skip("fork() not declared")
    else:
      proc adder() {.cps:Cont.} =
        cps fork()
        inc r

      spawn adder()
      run()
      if r != 2:
        raise newException(Defect, "uh oh")

  block:
    ## the famous tock test
    proc tock(name: string; ms: int) {.cps: Cont.} =
      var count: int = 10
      while count > 0:
        dec count
        yield sleep(ms)
        echo name, " ", count

    trampoline tock("tick", 3)
    trampoline tock("tock", 7)

    run()

  block:
    ## shadow mission impossible
    when true:
      skip("will not work until new scopes go in")
    else:
      proc b(x: int) {.cps:Cont.} =
        cps noop()
        doAssert x > 0
        cps noop()
        let x: int = 3
        cps noop()
        doAssert x == 3
        cps noop()
        var y: int = 8
        block:
          cps noop()
          var x: int = 4
          cps noop()
          inc x
          cps noop()
          dec y
          cps noop()
          doAssert x == 5
          doAssert y == 7
        cps noop()
        doAssert x == 3
        cps noop()
        doAssert y == 7

      proc a(x: int) {.cps:Cont.} =
        cps noop()
        doAssert x > 0
        cps noop()
        doAssert x > 0
        cps noop()
        var x: int = 2
        cps noop()
        doAssert x == 2
        cps noop()
        spawn b(x)
        cps noop()
        doAssert x == 2
        cps noop()
        doAssert x == 2

      spawn a(1)
      run()

  block:
    ## the sluggish yield test
    when defined(release):
      skip("too slow for release mode")
    const
      start = 2
      tiny = 0
      big = start * 2
    var count = start

    proc higher(ms: int) {.cps:Cont.} =
      while count < big and count > tiny:
        inc count
        cps sleep(ms)
        cps jield()
        cps jield()
        cps jield()
        cps jield()
        cps jield()
        cps jield()

    proc lower(ms: int) {.cps:Cont.} =
      while count < big and count > tiny:
        dec count
        cps sleep(ms)
        cps jield()

    trampoline higher(1)
    trampoline lower(1)

    run()

    if count != tiny:
      raise newException(ValueError, "you're a terrible coder")
