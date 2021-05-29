
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

proc deeper() {.cps:Work.} =
  # "not so deep youre hurting me"
  echo "deeper() in"
  jield()
  echo "deeper() out"
  
proc foo() {.cps:Work.} =
  echo "foo() in"
  echo "foo() yield()"
  jield()
  echo "foo() yield done()"
  echo "foo() calls deeper()"
  deeper()
  echo "foo() returned from deeper()"
  echo "foo() out"
  
proc bar() {.cps:Work.} =
  echo "bar() in"
  echo "bar() yield"
  jield()
  echo "bar() yield done"
  echo "bar() calls foo()"
  foo()
  echo "bar() returned from foo()"
  echo "bar() out"


var pool = Pool()
pool.push whelp bar()
pool.run()

