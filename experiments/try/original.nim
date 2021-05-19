proc noop() = echo "noop"

var r = 0
proc foo() =
  try:
    try:
      try:
        raise newException(CatchableError, "some error")
      except CatchableError as e:
        noop()
        inc r
        doAssert e.msg == "some error"

      raise newException(ValueError, "something")
    finally:
      noop()
      inc r
      doAssert getCurrentExceptionMsg() == "something"

    doAssert false, "this should not run"
  finally:
    inc r

doAssertRaises ValueError:
  foo()

doAssert r == 3
