include preamble

suite "loops":

  var r = 0

  block:
    ## while loops correctly, uh, loop
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        inc i
      inc r
      check i == 2
    foo()
    check r == 4

  block:
    ## a continue statement within a while statement works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 3:
        inc r
        noop()
        inc i
        if i <= 2:
          continue
        check i == 3
        inc r
      inc r
    foo()
    check r == 6

  block:
    ## a while statement supports a local variable inside
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      var i = 0
      while i < 2:
        inc r
        let x = i
        noop()
        inc r
        inc i
        noop()
        inc r
        check x == i - 1
      inc r
    foo()
    check r == 8

  block:
    ## a while loop with only a single continuing call works
    proc jield(c: Cont): Cont {.cpsMagic.} =
      discard

    r = 0
    proc count() {.cps: Cont.} =
      inc r
      var i = 0

      while i < 2:
        jield()

      fail "this statement should not run"

    count()
    check r == 1

  block:
    ## whelp instantiates continuations with arguments
    r = 0
    proc foo(x: int) {.cps: Cont.} =
      check x == 5
      inc r
      let i = 3
      noop()
      inc r
      check i == 3
      check x == 5
    let c = whelp foo(5)
    trampoline c
    check r == 2

  block:
    ## a while statement with a continuation and a break
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
    foo()
    check r == 5

  block:
    ## a break inside a multiply-nested else inside a while
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
    foo()
    check r == 6

  block:
    ## for loops with a continue and a break work correctly
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      for i in 0 .. 3:
        if i == 0:
          continue
        if i > 2:
          break
        r.inc i

    foo()
    check r == 4

  block:
    ## for loops with a continue and break across continuations
    when true:
      skip"pending #48"
    else:
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        for i in 0 .. 3:
          noop()
          if i == 0:
            continue
          if i > 2:
            break
          r.inc i

      foo()
      check r == 4

  block:
    ## named breaks work from inside a while statement
    var k = newKiller 6
    proc foo() {.cps: Cont.} =
      step 1
      block:
        block found:
          step 2
          while true:
            step 3
            noop()
            step 4
            if true:
              step 5
              break found
            fail"loop tail should be unreachable"
          fail"post loop should be unreachable"
        step 6
        break
        step 7
    foo()
