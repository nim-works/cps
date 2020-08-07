
import cps

# Middleware

type C = ref object of RootObj
  fn*: proc(c: C): C {.nimcall.}

proc sleep(c: C): C =
  echo "sleep"
  return c

# User code

proc two() {.cps:C.} =
  echo "two a"
  cps sleep()
  echo "two b"

proc one() {.cps:C.} =
  echo "one a"
  return two()
  echo "one b"


var c = one()

while c != nil and c.fn != nil:
  c = c.fn(c)
