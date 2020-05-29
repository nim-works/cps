import os
import eventqueue



# Yield/sleep

proc sleep(t: float, c: Cont): Cont =
  discard addTimer(t, c)
  return Cont()


# proc ticker() =
#   var i: int
#   while true:
#     echo "tick"
#     inc i
#     sleep()

type
  ContTicker1 = ref object of Cont
    i: int

  ContTicker = ref object of Cont
    i: int

proc ticker_1(c: Cont): Cont =
  var i = c.ContTicker1.i
  echo "tick ", i
  inc i
  let c2 = ContTicker1(fn: ticker_1, i: i)
  sleep(0.3,  c2)

proc ticker(c: Cont): Cont =
  var i = c.ContTicker.i
  return ContTicker1(fn: ticker_1, i: i)


# proc tocker() =
#   var j: int
#   while true:
#     sleep()
#     echo "tock"
#     dec j

type
  ContTocker1 = ref object of Cont
    j: int
  
  ContTocker2 = ref object of Cont
    j: int
  
  ContTocker = ref object of Cont
    j: int

proc tocker_1(c: Cont): Cont
proc tocker_2(c: Cont): Cont

proc tocker_1(c: Cont): Cont =
  var j = c.ContTocker1.j
  echo "tock ", j
  dec j
  return ContTocker2(fn: tocker_2, j: j)
  
proc tocker_2(c: Cont): Cont =
  var j = c.ContTocker2.j
  let c2 = ContTocker1(fn: tocker_1, j: j)
  sleep(0.5, c2)

proc tocker(c: Cont): Cont =
  var j = c.ContTocker.j
  return ContTocker2(fn: tocker_2, j: j)


# Start tickers and tockers

ContTicker(fn: ticker, i:   0).run()
ContTicker(fn: ticker, i: 100).run()
ContTocker(fn: tocker, j:   0).run()

# Run event loop forever

run()

