
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


type Pool = ref object
  work: Deque[Cont]
  lock: Lock

proc newPool(): Pool =
  result = Pool()
  initLock(result.lock)


var pool = newPool()

proc work(nThreads: int) =

  proc aux(pool: Pool) {.thread.} =

    while true:

      var c: Cont

      pool.lock.acquire
      if pool.work.len == 0:
        echo "Deque empty"
        return
      c = pool.work.popFirst
      pool.lock.release

      if c == nil:
        break

      c = c.fn(c)

      if c != nil:
        pool.lock.acquire
        pool.work.addLast c
        pool.lock.release

  var threads = newSeq[Thread[Pool]](nThreads)
  for i in 0..<nThreads:
    createThread(threads[i], aux, pool)
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
    while j < 100:
      j += 0.01
      b += sin(i) + cos(j)
    jield()

  echo id, ": ", b

proc ticker() {.cps:Cont.} =
  while true:
    echo readFile("/proc/self/stat")
    jield()

for i in 1..1_000_000:
  pool.work.addLast slow(i, 100)

pool.work.addLast ticker()

work(countProcessors())

