
#
# Simple test of a naive threadpool for scheduling CPS threads. This demo
# creates 1 million 'threads' scheduled on all available CPUs in the system
#
# nim r --gc:arc --threads:on --threadanalysis:off stash/threadpool.nim 
#

import cps, math, std/locks, deques, cpuinfo

###########################################################################
# Implementation of the thread pool
###########################################################################

type Cont = ref object of RootObj
  fn*: proc(c: Cont): Cont {.nimcall.}
  mom: Cont


type Pool = ref object
  work: Deque[Cont]
  lock: Lock

proc newPool(): Pool =
  result = Pool()
  initLock(result.lock)


var pool = newPool()
  
proc doWork(pool: Pool) {.thread.} =

  while true:

    pool.lock.acquire
    if pool.work.len == 0:
      echo "Deque empty"
      pool.lock.release
      return

    var c = pool.work.popFirst
    pool.lock.release

    if c == nil:
      break

    while c.running:
      c = c.fn(c)

    if c != nil:
      pool.lock.acquire
      pool.work.addLast c
      pool.lock.release


proc work(nThreads: int) =
  var threads = newSeq[Thread[Pool]](nThreads)
  for i in 0..<nThreads:
    createThread(threads[i], doWork, pool)
  joinThreads(threads)


proc jield(c: Cont): Cont {.cpsMagic.} =
  if c != nil:
    pool.lock.acquire
    pool.work.addLast(c)
    pool.lock.release

###########################################################################
# Main code
###########################################################################

proc slow(id: int, n: float) {.cps:Cont.} =
  var i, j, b: float

  while i < n:
    i += 0.01
    j = 0
    while j < 100_000:
      j += 0.01
      b += sin(i) + cos(j)
    jield()

  echo id, ": ", b

proc ticker() {.cps:Cont.} =
  while true:
    echo "tick"
    jield()

for i in 1..100:
  pool.work.addLast whelp slow(i, 100)

pool.work.addLast whelp ticker()

work(countProcessors())

