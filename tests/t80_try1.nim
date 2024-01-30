import std/strutils

include preamble

from cps/spec import cpsStackFrames

suite "try statements":

  var r = 0

  block:
    ## try-except statements may be split across continuations
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
      except CatchableError:
        fail "this branch should not run"
      inc r

    trampoline whelp(foo())
    check r == 3

  block:
    ## try-except statements may split and also raise exceptions
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except CatchableError:
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    trampoline whelp(foo())
    check r == 4

  block:
    ## exception clauses may split across continuations
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except CatchableError:
        inc r
        noop()
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    trampoline whelp(foo())
    check r == 5

  block:
    ## exceptions raised in the current continuation work
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except CatchableError:
        inc r
        noop()
        check getCurrentExceptionMsg() == "test"
        inc r
      inc r

    trampoline whelp(foo())
    check r == 5

  block:
    ## except statement catching multiple exception types across splits
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(ValueError, "test")
        fail "statement run after raise"
      except ValueError, IOError:
        check getCurrentExceptionMsg() == "test"
        inc r

      inc r

    proc bar() {.cps: Cont.} =
      # Same as foo(), but with the constraints switched
      inc r
      try:
        noop()
        inc r
        raise newException(ValueError, "test")
        fail "statement run after raise"
      except IOError, ValueError:
        check getCurrentExceptionMsg() == "test"
        inc r

      inc r

    r = 0
    trampoline whelp(foo())
    check r == 4

    r = 0
    trampoline whelp(bar())
    check r == 4

  block:
    ## try statements with a finally clause
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
      finally:
        inc r

    trampoline whelp(foo())
    check r == 3

  block:
    ## try statements with a finally and a return
    r = 0

    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        return
        fail"statement run after return"
      finally:
        inc r

      fail"statement run after try-finally containing a return"

    trampoline whelp(foo())
    check r == 3

  block:
    ## try statements with an exception and a finally
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "")
        fail "statement run after raise"
      except CatchableError:
        inc r
      finally:
        inc r
      inc r

    trampoline whelp(foo())
    check r == 5

  block:
    ## try statements with a split in finally
    r = 0
    proc foo() {.cps: Cont.} =
      inc r

      try:
        noop()
        inc r
      finally:
        noop()
        inc r

      inc r

    trampoline whelp(foo())
    check r == 4

  block:
    ## try statements with a split in finally with an unhandled exception
    r = 0
    proc foo() {.cps: Cont.} =
      inc r

      try:
        noop()
        inc r
        raise newException(ValueError, "test")
        fail"code run after raise"
      finally:
        noop()
        inc r

      fail"code run after raising try-finally"

    expect ValueError:
      trampoline whelp(foo())
    check r == 3

  block:
    ## nested try statements within the except branch
    r = 0
    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
        fail "statement run after raise"
      except CatchableError:
        check getCurrentExceptionMsg() == "test"
        inc r

        try:
          noop()
          inc r
          raise newException(CatchableError, "test 2")
          fail "statement run after raise"
        except CatchableError:
          check getCurrentExceptionMsg() == "test 2"
          inc r

        check getCurrentExceptionMsg() == "test"
        inc r

      inc r

    trampoline whelp(foo())
    check r == 7

  block:
    ## calling a continuation that handles exception while handling an exception
    r = 0
    proc foo() {.cps: Cont.} =
      inc r

      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
      except CatchableError:
        noop()
        inc r
        check getCurrentExceptionMsg() == "test"

      inc r

    try:
      raise newException(CatchableError, "outside cps test")
    except CatchableError:
      trampoline whelp(foo())

      check r == 4
      check getCurrentExceptionMsg() == "outside cps test"

  block:
    ## running a continuation that handles exception then raises while handling
    ## an exception in the exception handler
    r = 0

    # This is a very delicate test designed to demonstrate an issue with
    # Nim's exception stack mechanism and CPS

    proc foo() {.cps: Cont.} =
      inc r

      try:
        noop()
        inc r
        raise newException(CatchableError, "test")
      except CatchableError:
        noop()
        inc r
        check getCurrentExceptionMsg() == "test"
        raise

      fail"this statement cannot be run"

    var c: Continuation = whelp foo()
    # Run two iterations, which should place us right after the raise
    #
    # At this point, the parent of our `raise` is `nil`, because there wasn't
    # any exception being handled at the point of raise.
    for _ in 1 .. 2:
      c = c.fn(c)

    try:
      raise newException(CatchableError, "outside cps test")
    except CatchableError:
      # Now we handle an exception, which the current exception is now
      # "outside cps test"
      try:
        # Run the tramp to finish `c`, which will end in a re-raise.
        trampoline c
        fail"continuing `c` should raise"
      except CatchableError:
        check r == 3
        # Confirm that this is the exception from cps
        check getCurrentExceptionMsg() == "test"

      # Confirm that the stack has been fixed and the parent of the inner
      # exception is the outer.
      check getCurrentExceptionMsg() == "outside cps test"

  block:
    ## calling a continuation with finally while handling an exception
    r = 0
    proc foo() {.cps: Cont.} =
      inc r

      try:
        noop()
        inc r
      finally:
        noop()
        inc r

      inc r

    try:
      raise newException(CatchableError, "outside cps test")
    except CatchableError:
      trampoline whelp(foo())

      check r == 4
      check getCurrentExceptionMsg() == "outside cps test"

  block:
    ## except T as e keep the type T in cps
    r = 0

    type
      SpecialError = object of CatchableError
        extra: int ## An extra field so we can verify that we can access it

    proc newSpecialError(msg: string, extra: int): ref SpecialError =
      result = newException(SpecialError, msg)
      result.extra = extra

    proc foo() {.cps: Cont.} =
      inc r
      try:
        noop()
        inc r
        raise newSpecialError("test", 42)
        fail "statement run after raise"
      except SpecialError as e:
        noop()
        inc r
        check e.msg == "test"
        # The reason we test access is because `is` is expanded before `e` is
        # processed by cps. By testing access we can be sure that even after
        # cps processing it's still the correct type.
        check e.extra == 42

    foo()
    check r == 3

  block:
    ## try statement with one cps jump as the body
    r = 0

    proc noop(c: Cont): Cont {.cpsMagic.} =
      inc r
      result = c

    proc foo() {.cps: Cont.} =
      try:
        noop()
      except CatchableError:
        fail"this except branch should not run"

      inc r

    trampoline whelp(foo())
    check r == 2

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
      r = 0
      proc foo() {.cps: Cont.} =
        noop()
        inc r
        try:
          raise newException(CatchableError, "test")
        except CatchableError:
          let frames = renderStackFrames()
          check frames.len > 0, "expected at least one stack trace record"
          check "t80_try.nim" in frames[0], "couldn't find t80_try.nim in the trace"
          raise

      try:
        trampoline whelp(foo())
        inc r
      except CatchableError as e:
        check e.msg == "test", "unable to pass exception message from cps"
      check r == 1
