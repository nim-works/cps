
#
# Proof of concept try/catch/throw
#

import cps/core


type
  Tc = ref object of RootObj
    fn*: proc(c: Tc): Tc {.nimcall.}

var
  err: bool
  msg: string
  where: Tc

# CPS magic functions

proc start(c: Tc): Tc =
  where = c
  return c

proc throw(c: Tc, m: string): Tc =
  err = true
  msg = m
  return where


# This is a cps func that will throw an error

proc deeper() {.cps:Tc.} =
  echo "throwing"
  cps throw("Boom")
  echo "you will never see this"


# 'main' cps function, sets up try/cach and calls another
# function that will throw

proc foo() {.cps:Tc.} =

  cps start()

  if not err:
    echo "one"
    echo "two"
    return deeper()
    echo "three"
    echo "four"

  if err:
    echo "** caught error: ", msg, " **"

  echo "bye"



# Trampoline

var c = foo()
while c != nil and c.fn != nil:
  c = c.fn(c)
