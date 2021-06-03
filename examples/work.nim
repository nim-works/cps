
import cps, deques

###########################################################################
# Implementation of a minimal scheduler, just a dequeue of work
###########################################################################

type
  Work = ref object of RootObj
    fn*: proc(w: Work): Work {.nimcall.}
    mom: Work
    pool: Pool

  Pool = ref object
    workQueue: Deque[Work]

proc push(pool: Pool, w: Work) =
  if w.running:
    w.pool = pool
    pool.workQueue.addLast(w)

template push(pool: Pool; c: untyped) =
  let d = whelp c
  d.pool = pool
  pool.push d

proc jield(c: Work): Work {.cpsMagic.} =
  c.pool.push c

proc run(pool: Pool) =
  while pool.workQueue.len > 0:
    var w = pool.workQueue.popFirst
    pool.push w.trampoline

###########################################################################
# Main code
###########################################################################

proc job(id: string, n: int) {.cps:Work.} =
  echo "job ", id, " in"
  var i = 0
  while i < n:
    echo "job ", id, ": ", i
    jield()
    inc i
  echo "job ", id, " out"

let pool = Pool()
pool.push job("cat", 3)
pool.push job("dog", 5)
pool.push job("pig", 3)
pool.run()

