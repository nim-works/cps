
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
    yields: int

proc push(pool: Pool, c: Work) =
  if c.running:
    echo "pool was supplied to push"
    c.pool = pool
    pool.workQueue.addLast(c)

proc jield(c: Work): Work {.cpsMagic.} =
  inc c.pool.yields
  c.pool.push c

proc run(pool: Pool) =
  while pool.workQueue.len > 0:
    var c = pool.workQueue.popFirst
    # During trampolining we need to make sure the continuation always has
    # a proper pointer to the pool, due to momification
    while c.running:
      c = c.fn(c)
    pool.push c

proc tail(mom, c: Work): Work =
  echo "tail copied the pool"
  result = c
  result.mom = mom
  result.pool = mom.pool

###########################################################################
# Main code
###########################################################################

var total: int

proc deeper(b: ref int) {.cps:Work.} =
  echo "  deeper() in, b: ", b[]
  jield()
  inc total, b[]
  echo "  deeper() out"

proc foo(a: int) {.cps:Work.} =
  echo " foo() in a: ", a
  echo " foo() yield()"
  jield()
  echo " foo() yield done()"
  echo " foo() calls deeper()"
  # CPS does not support var parameters yet. We can box an int tho
  var b = new int;
  b[] = a * 2
  deeper(b)
  echo " foo() returned from deeper(), b: ", b[]
  echo " foo() out"

proc bar() {.cps:Work.} =
  echo "bar() in"
  echo "bar() yield"
  jield()
  echo "bar() yield done"
  echo "bar() calls foo(1)"
  foo(1)
  echo "bar() returned from foo()"
  echo "bar() calls foo(2)"
  foo(2)
  echo "bar() returned from foo()"
  echo "bar() out"


var pool = Pool()
pool.push whelp bar()
pool.run()

echo pool.yields
doAssert pool.yields == 5
echo total
doAssert total == 6
