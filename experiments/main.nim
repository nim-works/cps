
import eventqueue

# Yield/sleep

proc sleep(c: Cont, t: float): Cont =
  discard addTimer(t, c)
  return Cont()


# proc ticker() =
#   var i: int
#   while true:
#     echo "tick"
#     inc i
#     sleep()

type
  Ticker1Cont = ref object of Cont
    i: int

  TickerCont = ref object of Cont
    i: int

proc ticker_1(c: Cont): Cont =
  var i = c.Ticker1Cont.i
  echo "tick ", i
  inc i
  Ticker1Cont(fn: ticker_1, i: i).sleep(0.3)

proc ticker(c: Cont): Cont =
  var i = c.TickerCont.i
  Ticker1Cont(fn: ticker_1, i: i)


# proc tocker() =
#   var j: int
#   while true:
#     sleep()
#     echo "tock"
#     dec j

type
  Tocker1Cont = ref object of Cont
    j: int
  
  Tocker2Cont = ref object of Cont
    j: int
  
  TockerCont = ref object of Cont
    j: int

proc tocker_1(c: Cont): Cont
proc tocker_2(c: Cont): Cont

proc tocker_1(c: Cont): Cont =
  var j = c.Tocker1Cont.j
  echo "tock ", j
  dec j
  Tocker2Cont(fn: tocker_2, j: j)
  
proc tocker_2(c: Cont): Cont =
  var j = c.Tocker2Cont.j
  Tocker1Cont(fn: tocker_1, j: j).sleep(0.5)

proc tocker(c: Cont): Cont =
  var j = c.TockerCont.j
  Tocker2Cont(fn: tocker_2, j: j)


# Start tickers and tockers

TickerCont(fn: ticker, i:   0).run()
TickerCont(fn: ticker, i: 100).run()
TockerCont(fn: tocker, j:   0).run()

# Run event loop forever

run()

