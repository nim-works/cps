import std/os

import cps
import cps/eventqueue

const
  start = 2
  tiny = 0
  big = start * 2
var count = start

proc higher(ms: int): Cont {.cps.} =
  while count < big and count > tiny:
    inc count
    cps_sleep ms
    cps_yield()
    cps_yield()
    cps_yield()

proc lower(ms: int): Cont {.cps.} =
  while count < big and count > tiny:
    dec count
    cps_sleep ms
    cps_yield()

trampoline higher(1)
trampoline lower(1)

run()

if count != tiny:
  raise newException(ValueError, "you're a terrible coder")
