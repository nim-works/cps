
import cps/core
import cps/schedulers

# Middleware

when false:
  type C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}

  proc sleep(c: C): C =
    echo "sleep"
    return c

else:
  type C = Cont

# User code

proc two() {.cps:C.} =
  echo "two a"
  cps sleep(2000)
  echo "two b"

proc one() {.cps:C.} =
  echo "one a"
  cps two()
  echo "one b"


var c = one()

run()
#while c != nil and c.fn != nil:
#  c = c.fn(c)
