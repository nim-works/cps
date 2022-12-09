
#
# Simple test of a naive threadpool for scheduling CPS threads. This demo
# creates many 'threads' scheduled on all available CPUs in the system
#
# nim r --gc:arc --threads:on --threadanalysis:off stash/threadpool.nim
#

import cps, math, std/locks, deques, cpuinfo

###########################################################################
# Implementation of the thread pool
###########################################################################

type Cont = ref object of Continuation


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
      pool.lock.release
      return

    var c = Continuation: pool.work.popFirst
    pool.lock.release

    if c.dismissed:
      break
    else:
      while c.running:
        c = c.fn(c)


proc work(nThreads: int) =
  var threads: seq[Thread[Pool]]
  newSeq(threads, nThreads)
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
    i += 1
    j = 0
    while j < 1_000:
      j += 0.01
      b += sin(i) + cos(j)
    jield()

  echo id, ": ", b

when defined(gcArc):
  for i in 1..32:
    pool.work.addLast:
      whelp slow(i, 4)


  work(countProcessors())
else:
  echo "this example doesn't work outside --gc:arc"
