
import cps, deques

###########################################################################
# Implementation of a minimal scheduler, just a dequeue of work
###########################################################################

type
  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    id: string
    pool: Pool

  Pool = ref object
    work: Deque[Cont]

proc newPool(): Pool =
  result = Pool()

proc work(pool: Pool) =
  while pool.work.len > 0:
    var c = pool.work.popFirst
    c = c.fn(c)
    if c != nil:
      pool.work.addLast c

proc push(pool: Pool, c: Cont) =
  echo "pushing ", c.id
  pool.work.addLast(c)

proc setPool(c: Cont, pool: Pool, id: string): Cont {.cpsMagic.} =
  c.pool = pool
  c.id = id
  return c

proc jield(c: Cont): Cont {.cpsMagic.} =
  echo "jield ", c.id
  assert(c.pool != nil)
  if c != nil:
    c.pool.work.addLast(c)

###########################################################################
# Main code
###########################################################################

proc job(p: Pool, id: string, n: int) {.cps:Cont.} =
  setPool(p, id)
  var i = 0
  while i < n:
    echo id, ": ", i
    jield()
    inc i

let pool = Pool()
pool.push job(pool, "cat", 3)
pool.push job(pool, "dog", 3)
work(pool)

