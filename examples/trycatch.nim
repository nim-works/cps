
#
# Proof of concept try/catch/throw
#

import cps


type
  Tc = ref object of RootObj
    fn*: proc(c: Tc): Tc {.nimcall.}
    mom: Tc

var
  err: bool
  msg: string
  where: Tc

# CPS magic functions

proc start(c: Tc): Tc {.cpsMagic.} =
  where = c
  return c

proc throw(c: Tc, m: string): Tc {.cpsMagic.} =
  err = true
  msg = m
  return where


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



# Trampoline

var c = whelp foo()
while c.running:
  c = c.fn(c)
