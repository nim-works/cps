
#
# Simple test of a naive threadpool for scheduling CPS threads. This demo
# creates 1 million 'threads' scheduled on all available CPUs in the system
#
# nim r --gc:arc --threads:on --threadanalysis:off stash/threadpool.nim 
#

import cps, math, std/locks, deques, cpuinfo, os, heapqueue, times, sets, posix

###########################################################################
# Implementation of the thread pool
###########################################################################

type

  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

  TimerHandle = ref object
    c: Cont
    tWhen: float

  FdHandle = ref object
    c: Cont
    fd: int
    events: int

  Pool = ref object
    tNow: float
    work: Deque[Cont]
    timers: HeapQueue[TimerHandle]
    fds: seq[FdHandle]
    lock: Lock

proc newPool(): Pool =
  new result
  initLock(result.lock)

proc pushWork(pool: Pool, c: Cont) =
  ## Push continuation to the end of the work queue
  if c.state == Running:
    withLock pool.lock:
      pool.work.addLast c

proc pushWorkFront(pool: Pool, c: Cont) =
  ## Push continuation to the front of the work queue
  if c.state == Running:
    withLock pool.lock:
      pool.work.addFirst c

proc popWork(pool: Pool): Cont =
  ## Pop one continuation of the work pool
  withLock pool.lock:
    if pool.work.len > 0:
      result = pool.work.popFirst

proc addTimer(pool: Pool, c: Cont, dt: float) =
  ## Register a timer to expire in dt seconds
  withLock pool.lock:
    let t = TimerHandle(c: c, tWhen: pool.tNow + dt)
    echo "addtimer ", pool.tNow, " ", t.tWhen
    pool.timers.push t

proc addFd(pool: Pool, c: Cont, fd: int, events: int) =
  ## Register fd for the given events
  withLock pool.lock:
    let fh = FdHandle(c: c, fd: fd, events: events)
    pool.fds.add fh

   
proc checkTimers(pool: Pool) =
  ## Move continuations for all expired timers to the work queue
  ## The workers will then pick then up and execute.

  while true:
    # Find the next expired timer
    var t: TimerHandle
    withLock pool.lock:
      if pool.timers.len > 0:
        if pool.tNow > pool.timers[0].tWhen:
          t = pool.timers.pop()

    # No timers expired, done for now
    if t == nil:
      break

    # Move the epxired cont to the work queue
    if pool.tNow > t.tWhen:
      pool.pushWorkFront(t.c)

proc checkFds(pool: Pool) =
  ## Move continuations for all active file descriptors to the work queue.
  ## The workers will then pick then up and execute.

  if pool.fds.len == 0:
    return

  var pfds: seq[TPollfd]
  for id, fdh in pool.fds:
    pfds.add TPollfd(fd: fdh.fd.cint, events: fdh.events.cshort)
  let r = posix.poll(pfds[0].addr, pfds.len.Tnfds, 100)
  
  if r == 0:
    return

  for pfd in pfds:
    if pfd.revents != 0:
      for id, fdh in pool.fds:
        if fdh.fd == pfd.fd and (pfd.revents and fdh.events) != 0:
          pool.pushWorkFront(fdh.c)

proc worker(pool: Pool) {.thread.} =
  ## Worker thread: check the queue for work, execute it and put it back
  while true:
    var c = pool.popWork()
    if c != nil:
      c = c.fn(c) # TODO: trampoline for a limited time slice instead of once
      if c.state == Running:
        pool.pushWork(c)
    else:
      os.sleep(1)

# eventqueue public API

proc jield*(c: Cont, pool: Pool): Cont {.cpsMagic.} =
  pool.pushWork(c)

proc sleep*(c: Cont, pool: Pool, dt: float): Cont {.cpsMagic.} =
  pool.addTimer(c, dt)

proc wait(c: Cont, pool: Pool, fd: int, event: int): Cont {.cpsMagic.} =
  pool.addFd(c, fd, event)

proc run*(pool: Pool) =

  let nThreads = countProcessors()

  var workerThreads = newSeq[Thread[Pool]](nThreads)
  for i in 0..<nThreads:
    createThread(workerThreads[i], worker, pool)

  # Main loop
  while true:
    pool.tNow = epochTime()
    pool.checkTimers()
    pool.checkFds()
    os.sleep(1)

  joinThreads(workerThreads)



###########################################################################
# Main code
###########################################################################

var pool = newPool()

proc slow(id: int, n: float) {.cps:Cont.} =
  var i, j, b: float
  while true:
    i += 0.01
    j = 0
    while j < n:
      j += 0.01
      b += sin(i) + cos(j)
    pool.jield()
  echo id, ": ", b

proc ticker() {.cps:Cont.} =
  while true:
    echo "tick"
    pool.sleep(1.0)

proc reader() {.cps:Cont.} =
  while true:
    echo "waiting"
    pool.wait(0, POLLIN)
    var buf = newString(128)
    let r = posix.read(0, buf[0].addr, buf.len)
    echo "r: ", r, " buf: ", buf

for id in 1..99:
  pool.pushWork slow(id, 1_000)

pool.pushWork ticker()

pool.pushWork reader()

pool.run()

