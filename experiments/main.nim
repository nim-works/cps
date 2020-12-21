import eventqueue

proc ticker(c: var Cont): pointer =
  inc c.i
  echo "tick ", c.i
  if c.i < 200:
    c.addTimer(ticker, 0.2 * 1000)

run Cont(i:   0): ticker
run Cont(i: 100): ticker

forever()
