import testes

import cps
import cps/eventqueue except trampoline

var jumps: int

proc trampoline(c: Cont) =
  jumps = 0
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)
    inc jumps
    check jumps < 1000, "Too many iterations on trampoline, looping?"

var r = 0
proc adder(x: var int) =
  inc x

testes:

  block:
    ## the trampoline runs continuations, uh, continuously
    r = 0
    proc foo() {.cps: Cont.} =
      r = 1
    trampoline foo()
    check r == 1

  block:
    ## noop magic smoke test
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      inc r
    trampoline foo()
    check r == 2, "who let the smoke out?"

  block:
    ## noop is a primitive that merely sheds scope
    r = 0
    var j = 2
    proc foo() {.cps: Cont.} =
      inc r
      check j == 2
      j = 4
      let i = 3
      noop()
      inc r
      check i == 3, "i was " & $i
      check j == 4, "j was " & $j
      inc j
    trampoline foo()
    check r == 2
    check j == 5

  block:
    ## yield is a primitive that enters the dispatcher
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      jield()
      inc r
    trampoline foo()
    check r == 1
    run()
    check r == 2

  block:
    ## declaration via tuple deconstruction
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var (i, k) = (1, 3)
      let (x, z) = (4, 6)
      noop()
      inc r
      inc k
      noop()
      check "declared variables":
        i == 1
        k == 4
        x == 4
        z == 6
    trampoline foo()
    check r == 2

  block:
    ## multi-var declaration
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i, k = 3
      let x, z = 4
      noop()
      inc r
      inc k
      noop()
      check "declared variables":
        i == 3
        k == 4
        x == 4
        z == 4
    trampoline foo()
    check r == 2

  block:
    ## sleep is a primitive that rests in the dispatcher
    r = 0
    const q = 3
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < q:
        inc r
        sleep(i + 1)
        inc r
        inc i
      check i == q
    spawn foo()
    run()
    check r == q * 2

  block:
    ## shadowing and proc param defaults
    ## https://github.com/disruptek/cps/issues/22
    proc foo(a, b, c: int = 3) {.cps: Cont.} =
      ## a=1, b=2, c=3
      var a = 5
      ## a=5, b=2, c=3
      noop()
      ## a=5, b=2, c=3
      var b = b + a
      ## a=5, b=7, c=3
      noop()
      ## a=5, b=7, c=3
      check "proc parameters":
        a == 5
        b == 7
        c == 3
    trampoline foo(1, 2)

  block:
    ## reassignment of var proc params
    ## https://github.com/disruptek/cps/issues/47
    skip"pending issue #47"
    r = 0
    proc foo(a, b, c: var int) {.cps: Cont.} =
      inc r
      a = 5
      noop()
      inc r
      b = b + a
      noop()
      inc r
      check "var param assignment":
        a == 5
        b == 7
        c == 3
    var (x, y, z) = (1, 2, 3)
    trampoline foo(x, y, z)
    check "var param assignment":
      x == 5
      y == 7
      z == 3
    check r == 3

  block:
    ## simple block with break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        if true:
          inc r
          break
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 3

  block:
    ## block with break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        if true:
          inc r
          noop()
          inc r
          break
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 4

  block:
    ## semaphores
    var sem = newSemaphore()
    var success = false

    proc signalSleeper(ms: int) {.cps: Cont.} =
      sleep ms
      signal sem

    proc signalWaiter() {.cps: Cont.} =
      wait sem
      success = true

    trampoline signalSleeper(10)
    trampoline signalWaiter()
    run()
    check success, "signal failed"

  block:
    ## break statements without cps 🥴
    r = 0
    proc foo() =
      inc r
      while true:
        inc r
        if true:
          inc r
          break
        inc r
        fail"block break failed to break block"
      inc r
    foo()
    check r == 4

  block:
    ## a fairly tame cps break
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      while true:
        inc r
        noop()
        inc r
        if true:
          inc r
          break
        inc r
        fail"block break failed to break block"
      inc r
    trampoline foo()
    check r == 5

  block:
    ## break in a nested else (don't ask)
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      while true:
        inc r
        noop()
        inc r
        if true:
          inc r
          check r == 4
          if r != 4:
            fail"unexpected clause"
          else:
            inc r
            break
      inc r
    trampoline foo()
    check r == 6

  block:
    ## named breaks
    r = 1
    proc foo() {.cps: Cont.} =
      block found:
        while true:
          noop()
          if r > 2:
            break found
          noop()
          inc r
        fail"unreachable"
      r = r * -1
    trampoline foo()
    check r == -3, "r was " & $r

  block:
    ## while statement L
    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 2:
        inc i
      check i == 2
      r = i
    trampoline foo()
    check r == 2, "r was " & $r

  block:
    ## while statement Q
    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 2:
        noop()
        inc i
      r = i
    trampoline foo()
    check r == 2, "r was " & $r

  block:
    ## while statement N
    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 2:
        let x = i
        inc i
        check x < i
      r = i
    trampoline foo()
    check r == 2, "r was " & $r

  block:
    ## while with break
    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while true:
        let x = i
        adder i
        if i >= 2:
          break
        check x < i
      r = i
    trampoline foo()
    check r == 2, "r was " & $r

  block:
    ## while with continue
    r = 0
    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 2:
        let x = i
        adder i
        if x == 0:
          continue
        check x > 0
      r = i
    trampoline foo()
    check r == 2, "r was " & $r

  block:
    ## shadow test A
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      check x > 0, "parameter value unset"
    trampoline shadow(1)
    check r == 1

  block:
    ## shadow test B
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      let x = 3
      check x == 3, "shadowed symbol wrong"
    trampoline shadow(1)
    check r == 1

  block:
    ## shadow test C
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        check x == 4, "shadowing symbol wrong"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test D
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        inc x
        check x == 5, "shadowing symbol immutable"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test E
    r = 0
    proc shadow(x: int) {.cps: Cont.} =
      r = 1
      block:
        var x = 4
        block:
          inc x
        check x == 5, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted"
    trampoline shadow(3)
    check r == 1

  block:
    ## shadow test F
    r = 0
    proc shadow1(x: int) {.cps: Cont.} =
      inc r
      check x == 3, "unexpected input of " & $x
      block:
        var x = x + 2
        check x == 5, "failed to update from lower scope"
        inc x
        check x == 6, "failed to update from lower scope"
      check x == 3, "shadowed symbol corrupted to " & $x

    proc shadow2(x: int) {.cps: Cont.} =
      inc r
      var x = x + 2
      check x == 3, "shadow1 is expecting x == 3"
      trampoline shadow1(x)
      check x == 3, "x mutated by cps call"
      inc x
      check x == 4, "shadowing symbol corrupted"

    spawn shadow2(1)
    run()
    check r == 2

  block:
    ## for loop with continue, break
    proc foo() {.cps: Cont.} =
      r = 1
      while true:
        for i in 0 .. 3:
          if i == 0:
            continue
          if i > 2:
            break
          r.inc i
        inc r
        if r == 5:
          break
      inc r
    trampoline foo()
    check r == 6, "r is " & $r

  block:
    ## fork
    when not defined(fork):
      skip"fork() not declared"
    else:
      proc foo() {.cps: Cont.} =
        fork()
        inc r

      trampoline foo()
      if r != 2:
        raise newException(Defect, "uh oh")

  block:
    ## the famous tock test
    skip"lives in its own test now"
    proc foo(name: string; ms: int) {.cps: Cont.} =
      var count = 10
      while count > 0:
        dec count
        sleep ms
        checkpoint name, " ", count

    spawn foo("tick", 3)
    spawn foo("tock", 7)
    run()

  block:
    ## shadow mission impossible
    proc b(x: int) {.cps: Cont.} =
      noop()
      check x > 0
      noop()
      let x = 3
      noop()
      check x == 3
      noop()
      var y = 8
      block:
        noop()
        var x = 4
        noop()
        inc x
        noop()
        dec y
        noop()
        check x == 5
        check y == 7
      noop()
      check x == 3
      noop()
      check y == 7

    proc a(x: int) {.cps: Cont.} =
      noop()
      check x > 0
      noop()
      check x > 0
      noop()
      var x = 2
      noop()
      check x == 2
      noop()
      trampoline b(x)
      noop()
      check x == 2
      noop()
      check x == 2

    spawn a(1)
    run()

  block:
    ## the sluggish yield test
    when defined(release):
      skip"too slow for release mode"
    const
      start = 2
      tiny = 0
      big = start * 2
    var count = start

    proc higher(ms: int) {.cps: Cont.} =
      while count < big and count > tiny:
        inc count
        sleep(ms)
        jield()
        jield()
        jield()
        jield()
        jield()
        jield()

    proc lower(ms: int) {.cps: Cont.} =
      while count < big and count > tiny:
        dec count
        sleep(ms)
        jield()

    trampoline higher(1)
    trampoline lower(1)
    run()

    check count != tiny, "you're a terrible coder"

  block:
    ## assignment shim with constant
    r = 0
    proc bar(a: int): int {.cps: Cont.} =
      inc r
      noop()
      return a * 2

    proc foo() {.cps: Cont.} =
      inc r
      let x = int bar(4)
      inc r
      check x == 8

    trampoline foo()
    check r == 3

  block:
    ## assignment shim passing env var
    r = 0
    proc bar(a: int): int {.cps: Cont.} =
      inc r
      noop()
      return a * 2

    proc foo() {.cps: Cont.} =
      inc r
      let w = 4
      let x = int bar(w)
      inc r
      check x == 8

    trampoline foo()
    check r == 3
