import balls
import cps

include preamble

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
      except:
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
      except:
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
      except:
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
      except:
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
      except:
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
    skip"pending https://github.com/nim-lang/Nim/pull/18247":
      r = 0
      proc foo() {.cps: Cont.} =
        inc r
        try:
          noop()
          inc r
          raise newException(CatchableError, "test")
          fail "statement run after raise"
        except:
          check getCurrentExceptionMsg() == "test"
          inc r

          try:
            noop()
            inc r
            raise newException(CatchableError, "test 2")
            fail "statement run after raise"
          except:
            check getCurrentExceptionMsg() == "test 2"
            inc r

          check getCurrentExceptionMsg() == "test"
          inc r

        inc r

      trampoline whelp(foo())
      check r == 7

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
