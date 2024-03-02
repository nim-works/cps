import std/strutils

include preamble

from cps/spec import cpsStackFrames

suite "try statements":

  block:
    ## handling exception across multiple continuations
    var k = newKiller(6)
    proc foo() {.cps: Cont.} =
      noop()
      step 4
      raise newException(ValueError, "foo")

    proc bar() {.cps: Cont.} =
      noop()
      step 3
      foo()

    proc barbar() {.cps: Cont.} =
      try:
        noop()
        step 2
        bar()
      except ValueError as e:
        step 5
        doAssert e.msg == "foo"

    proc foobar() {.cps: Cont.} =
      step 1
      barbar()
      step 6

    trampoline whelp(foobar())

  block:
    ## try statement with a single statement which is a cps assignment
    var k = newKiller(2)
    proc bar(): int {.cps: Cont.} =
      step 1
      42

    proc foo() {.cps: Cont.} =
      var x = 0
      try:
        x = bar()
      except CatchableError:
        fail "This branch should not be executed"

      step 2
      check x == 42

    trampoline whelp(foo())

  block:
    ## try-finally-reraise escape via break statements.
    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      while true:
        try:
          noop()
          step 1
          raise newException(ValueError, "")
        finally:
          break
        fail "statement in while-loop after break"
      fail "statement after unhandled exception"

    expect ValueError:
      trampoline whelp(foo())

  block:
    ## try-finally-reraise escape via continue statements.
    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      while true:
        try:
          noop()
          step 1
          raise newException(ValueError, "")
        finally:
          continue
        fail "statement in while-loop after finally"
      fail "statement after unhandled exception"

    expect ValueError:
      trampoline whelp(foo())

  block:
    ## try: raise() except: continue
    var k = newKiller(2)

    proc foo() {.cps: Cont.} =
      try:
        while true:
          noop()
          inc k
          if k.step == 2:
            break
          try:
            raise newException(ValueError, "oops")
          except ValueError:
            continue
          fail "statement in while-loop after continue"
      except ValueError:
        fail "uncaught exception"

    trampoline whelp(foo())

  block:
    ## try-finally-reraise escape via return statements.
    var k = newKiller(1)

    proc foo() {.cps: Cont.} =
      try:
        noop()
        step 1
        raise newException(ValueError, "")
      finally:
        return
      fail "statement after return"

    expect ValueError:
      trampoline whelp(foo())

  block:
    ## try-finally-reraise handle after escape attempt
    var k = newKiller(2)

    proc foo() {.cps: Cont.} =
      try:
        while true:
          try:
            noop()
            step 1
            raise newException(ValueError, "")
          finally:
            break
      except ValueError:
        step 2

    trampoline whelp(foo())

  block:
    ## the stack trace probably still works
    when not cpsStackFrames:
      skip"--stacktrace:off specified"
    else:
      var r = 0
      proc foo() {.cps: Cont.} =
        noop()
        inc r
        try:
          raise newException(CatchableError, "test")
        except CatchableError:
          let frames = renderStackFrames()
          check frames.len > 0, "expected at least one stack trace record"
          check "t80_try2.nim" in frames[0], "couldn't find t80_try2.nim in the trace"
          raise

      try:
        trampoline whelp(foo())
        inc r
      except CatchableError as e:
        check e.msg == "test", "unable to pass exception message from cps"
      check r == 1

suite "defer statements":

  var r = 0

  block:
    ## a defer statement works across a continuation
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        check r == 2, "defer run before end of scope"
        inc r

      inc r
      noop()
      inc r

    foo()
    check r == 3

  block:
    ## a basic defer statement is supported
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      noop()
      defer:
        check r == 4
        inc r
      inc r
      defer:
        check r == 3
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## a defer in a nested template is supported
    r = 0

    template deferChk(i: int) =
      inc r
      defer:
        check r == i
        inc r

    proc foo() {.cps: Cont.} =
      deferChk(5)
      inc r
      deferChk(4)
      inc r

    foo()
    check r == 6

  block:
    ## a defer inside a block statement works
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      block:
        defer:
          check r == 2
          inc r
        inc r
      defer:
        check r == 4
        inc r
      inc r

    foo()
    check r == 5

  block:
    ## a naked defer is not a problem
    r = 0
    proc foo() {.cps: Cont.} =
      defer:
        inc r

    foo()
    check r == 1


when defined(gcArc) or defined(gcOrc):
  suite "breaking deterministic memory managers":
    block:
      ## try-except-statement splits
      proc foo() {.cps: Cont.} =
        var k = newKiller(3)
        step 1
        try:
          noop()
          step 2
        except CatchableError:
          fail "this branch should not run"
        step 3

      foo()

    block:
      ## try-except splits with raise
      proc foo() {.cps: Cont.} =
        var k = newKiller(4)
        step 1
        try:
          noop()
          step 2
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except CatchableError:
          step 3
        step 4

      foo()

    block:
      ## try-finally-statement splits
      proc foo() {.cps: Cont.} =
        var k = newKiller(4)
        step 1
        try:
          noop()
          step 2
        finally:
          step 3
        step 4

      foo()

    block:
      ## try-except-finally splits with raise
      proc foo() {.cps: Cont.} =
        var k = newKiller(5)
        step 1
        try:
          noop()
          step 2
          raise newException(CatchableError, "")
          fail "statement run after raise"
        except CatchableError:
          step 3
        finally:
          step 4
        step 5

      foo()
