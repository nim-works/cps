var r = 0

type
  C = ref object
    fn: proc (c: C): C {.nimcall.}
    e: ref CatchableError
    x1: ref Exception
    x2: ref Exception

proc noop(c: C): C =
  echo "noop"
  c

template post_exception_body(goto: typed; body: untyped) {.dirty.} =
  if getCurrentException().isNil:
    body
  c.fn = goto

template sensitive_exit {.dirty.} =
  if c.fn.isNil and not getCurrentException().isNil:
    raise
  return c

template catch_and_go(ex: untyped; goto: typed; body: untyped) {.dirty.} =
  try:
    body
  except Exception as `ex`:
    c.`ex` = `ex`
  c.fn = goto
  sensitive_exit()

proc f2(c: C): C =
  ## finally from the "toplevel" try
  echo "f2"

  # we look for non-nil exceptions
  if not c.x2.isNil:
    # it's code that's inside a try
    setCurrentException c.x2 # added boilerplate
  elif not c.x1.isNil:
    # it's code that's inside a try
    setCurrentException c.x1 # added boilerplate
  elif not c.e.isNil:
    # finally when exception was thrown
    setCurrentException c.e # added boilerplate

  inc r

  post_exception_body nil:
    # this would be the body after the toplevel try statement
    discard

  sensitive_exit()

proc b2(c: C): C =
  # we look for non-nil exceptions
  if not c.x1.isNil:
    # it's code that's inside a try
    setCurrentException c.x1 # added boilerplate
  elif not c.e.isNil:
    # finally when exception was thrown
    setCurrentException c.e # added boilerplate

  # it's in a try so we need to catch it
  catch_and_go x2, f2:
    inc r
    doAssert getCurrentExceptionMsg() == "something"

    post_exception_body f2:
      # proceed into the toplevel try body
      doAssert false, "this should not run"

proc f1(c: C): C =
  ## finally from the "middle" try
  echo "f1"

  # we look for non-nil exceptions
  if not c.x1.isNil:
    # it's code that's inside a try
    setCurrentException c.x1 # added boilerplate
  elif not c.e.isNil:
    # finally when exception was thrown
    setCurrentException c.e # added boilerplate

  catch_and_go x2, f2:
    # finally body starts with a noop, so ...
    c.fn = b2
    return noop c

proc b(c: C): C =
  ## inner-most try body successful; this is the code
  ## after that try statement

  catch_and_go x1, f1:
    # user code for body after the try
    raise newException(ValueError, "something")

proc b1(c: C): C =
  # interior of clause
  if not c.e.isNil:
    setCurrentException c.e  # added boilerplate

  catch_and_go x1, f1:
    # user code for clause
    inc r
    doAssert c.e.msg == "some error"

    # after the try
    raise newException(ValueError, "something")

proc foo(): C =
  var c = C()

  try:
    raise newException(CatchableError, "some error")
    c.fn = b
    return noop c
  except CatchableError as e:
    echo "e"
    c.e = e
    c.fn = b1
    return noop c

doAssertRaises ValueError:
  var c = foo()
  while c != nil and c.fn != nil:
    c = c.fn(c)

doAssert r == 3, "r is " & $r
