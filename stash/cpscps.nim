
import cps, deques

###########################################################################
# Implementation of a minimal scheduler, just a dequeue of work
###########################################################################

type
  Work = ref object of RootObj
    fn*: proc(c: Work): Work {.nimcall.}
    mom: Work
    pool: Pool

  Pool = ref object
    workQueue: Deque[Work]

proc push(pool: Pool, c: Work) =
  if c.running:
    c.pool = pool
    pool.workQueue.addLast(c)

proc jield(c: Work): Work {.cpsMagic.} =
  echo "jield"
  c.pool.push c

proc run(pool: Pool) =
  while pool.workQueue.len > 0:
    var c = pool.workQueue.popFirst
    while c.running:
      c.pool = pool
      c = c.fn(c)
    pool.push c

###########################################################################
# Main code
###########################################################################
  
proc foo() {.cps:Work.} =
  echo "foo() in"
  jield()
  echo "foo() out"
  
proc bar() {.cps:Work.} =
  echo "bar() in"
  foo()
  echo "bar() out"


var pool = Pool()
pool.push whelp bar()
pool.run()

