
import cps, deques

###########################################################################
# Implementation of a minimal scheduler, just a dequeue of work
###########################################################################

type
  Work = ref object of RootObj
    fn*: proc(c: Work): Work {.nimcall.}
    pool: Pool

  Pool = ref object
    workQueue: Deque[Work]

proc push(pool: Pool, c: Work) =
  if c.running:
    c.pool = pool
    pool.workQueue.addLast(c)

proc jield(c: Work): Work {.cpsMagic.} =
  c.pool.push c

proc run(pool: Pool) =
  while pool.workQueue.len > 0:
    var c = pool.workQueue.popFirst
    c = c.fn(c)
    pool.push c

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
pool.push whelp(job("cat", 3))
pool.push whelp(job("dog", 5))
pool.push whelp(job("pig", 3))
pool.run()

