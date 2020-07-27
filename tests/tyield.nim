import std/os

import cps
import cps/eventqueue

const
  start = 2
  tiny = 0
  big = start * 2
var count = start

proc higher(ms: int) {.cps:Cont.} =
  while count < big and count > tiny:
    inc count
    cps sleep(ms)
    cps jield()
    cps jield()
    cps jield()

proc lower(ms: int) {.cps:Cont.} =
  while count < big and count > tiny:
    dec count
    cps sleep(ms)
    cps jield()

trampoline higher(1)
trampoline lower(1)

run()

if count != tiny:
  raise newException(ValueError, "you're a terrible coder")
