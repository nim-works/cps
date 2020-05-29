import os
import eventqueue


type
  Fn = proc(i: int): Cont

  Cont = object
    fn: Fn
    arg: int

proc newCont(fn: Fn, arg: int): Cont =
  Cont(fn: fn, arg: arg)

# Trampoline

proc run(cont: Cont) =
  var cont = cont
  while cont.fn != nil:
    cont = cont.fn(cont.arg)

# Yield/sleep

proc sleep(cont: Cont): Cont =

  if false:
    sleep(10)
    return cont

  discard addTimer(0.3, proc(): bool =
    cont.run())
  
  return newCont(nil, 0)


# proc ticker() =
#   var i: int
#   while true:
#     echo "tick"
#     inc i
#     sleep()

proc ticker_1(i: int): Cont

proc ticker_1(i: int): Cont =
  var i = i
  echo "tick ", i
  inc i
  return sleep(newCont(ticker_1, i))

proc ticker(i: int): Cont =
  var i = i
  return newCont(ticker_1, i)


# proc tocker() =
#   var j: int
#   while true:
#     sleep()
#     echo "tock"
#     dec j

proc tocker_1(j: int): Cont
proc tocker_2(j: int): Cont

proc tocker_1(j: int): Cont =
  var j = j
  echo "tock ", j
  dec j
  return newCont(tocker_2, j)
  
proc tocker_2(j: int): Cont =
  var j = j
  return sleep(newCont(tocker_1, j))

proc tocker(j: int): Cont =
  var j = j
  return newCont(tocker_2, j)


# Start tickers and tockers

newCont(ticker,   0).run()
newCont(ticker, 100).run()
newCont(tocker,   0).run()

# Run event loop forever

run()

