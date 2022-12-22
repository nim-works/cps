
#
# Proof of concept try/catch/throw
#

import cps


type
  Tc = ref object of Continuation

var
  err: bool
  msg: string
  where: ContinuationProc[Continuation]

# CPS magic functions

proc start(c: Tc): Tc {.cpsMagic.} =
  where = c.fn
  return c

proc throw(c: Tc, m: string): Tc {.cpsMagic.} =
  err = true
  msg = m
  c.fn = where
  reset c.mom
  return c


# This is a cps func that will throw an error

proc deeper() {.cps:Tc.} =
  echo "throwing"
  throw("Boom")
  echo "you will never see this"


# 'main' cps function, sets up try/cach and calls another
# function that will throw

proc foo() {.cps:Tc.} =

  start()

  if not err:
    echo "one"
    echo "two"
    deeper()
    echo "three"
    echo "four"

  if err:
    echo "** caught error: ", msg, " **"

  echo "bye"


foo()
