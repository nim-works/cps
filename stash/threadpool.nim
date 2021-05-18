
#
# Simple test of a naive threadpool for scheduling CPS threads. This demo
# creates 1 million 'threads' scheduled on all available CPUs in the system
#
# nim r --gc:arc --threads:on --threadanalysis:off stash/threadpool.nim 
#

import cps, math, std/locks, deques, cpuinfo, os, heapqueue, times, posix, strutils, sets, tables

###########################################################################
# Implementation of the thread pool
###########################################################################

type

  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    lock: Lock

  TimerHandle = object
    c: Cont
    tWhen: float

  FdHandle = object
    c: Cont
    fd: cint
    events: cshort

  Pool = ref object
    lock: Lock                       ## lock protecting everything below
    tNow: float                      ## timestamp of the latest iteration of the pool
    work: Deque[Cont]                ## dequeue of "work to be done"
    timers: HeapQueue[TimerHandle]   ## registered timers with their continuations
    fdhs: seq[FdHandle]              ## registered fds with their continuations

proc `<`(a, b: TimerHandle): bool = a.tWhen < b.tWhen


proc newPool(): Pool =
  new result
  initLock(result.lock)


proc pushWork(pool: Pool, c: Cont) =
  ## Push continuation to the end of the work queue: low prio
  if c.state == Running:
    withLock pool.lock:
      pool.work.addLast c


proc pushWorkFront(pool: Pool, c: Cont) =
  ## Push continuation to the front of the work queue: high prio
  if c.state == Running:
    withLock pool.lock:
      pool.work.addFirst c


proc popWork(pool: Pool): Cont =
  ## Pop one continuation of the work queue
  withLock pool.lock:
    if pool.work.len > 0:
      result = pool.work.popFirst


proc addTimer(pool: Pool, c: Cont, dt: float) =
  ## Register a timer to expire in dt seconds
  withLock pool.lock:
    let t = TimerHandle(c: c, tWhen: pool.tNow + dt)
    pool.timers.push t


proc addFd(pool: Pool, c: Cont, fd: cint, events: cshort) =
  ## Register fd for the given events
  withLock pool.lock:
    pool.fdhs.add FdHandle(c: c, fd: fd, events: events)


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
    if t.tWhen == 0:
      break

    # Move the epxired cont to the work queue
    if pool.tNow > t.tWhen:
      pool.pushWorkFront(t.c)


proc checkFds(pool: Pool) =
  ## Move continuations for all active file descriptors to the work queue.
  ## The workers will then pick then up and execute.
 
  # Populate the pollfd set with all registered file descriptors
  var pfds: seq[TPollfd]
  withLock pool.lock:
    if pool.fdhs.len == 0:
      return
    for fdh in pool.fdhs:
      pfds.add TPollfd(fd: fdh.fd, events: fdh.events)

  # Perform the poll
  let r = posix.poll(pfds[0].addr, pfds.len.Tnfds, 0)
  if r == 0:
    return

  # See which file descriptors are ready and move their continuations
  # to the work queue # TODO O(n^2)
  withLock pool.lock:
    for pfd in pfds:
      for i in 0..<pool.fdhs.len:
        let fdh = pool.fdhs[i]
        if fdh.fd == pfd.fd:
          if (pfd.revents and fdh.events) != 0:
            pool.work.addFirst fdh.c
            pool.fdhs.del(i)
            break


proc worker(pool: Pool) {.thread.} =
  ## Worker thread: check the queue for work, execute it and put it back
  while true:
    var c = pool.popWork()
    if c != nil:
      var lock = c.lock
      lock.acquire
      c = c.fn(c) # TODO: trampoline for a limited time slice instead of once
      lock.release
      if c.state == Running:
        pool.pushWork(c)
    else:
      os.sleep(1)

###########################################################################
# eventqueue public API
###########################################################################

proc jield*(c: Cont, pool: Pool): Cont {.cpsMagic.} =
  pool.pushWork(c)

proc sleep*(c: Cont, pool: Pool, dt: float): Cont {.cpsMagic.} =
  pool.addTimer(c, dt)

proc wait(c: Cont, pool: Pool, fd: cint, events: cshort): Cont {.cpsMagic.} =
  pool.addFd(c, fd, events)

proc run*(pool: Pool) =

  # Create worker threads
  let nThreads = 1#countProcessors()
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


proc writer(id: int, fd: cint) {.cps:Cont.} =
  var i = 0
  while i < 100:
    echo id, " write wait"
    pool.wait(fd, POLLOUT)
    var buf = alignLeft(($id & " " & $i & " "), 80, '=')
    let r = posix.write(fd, buf[0].addr, 80)
    doAssert r == 80
    echo id, " write r: ", r
    inc i
  echo id, " write done"
  discard close(fd)


proc reader(id: int, fd: cint) {.cps:Cont.} =
  var buf = newString(80)
  while true:
    echo id, " read wait"
    pool.wait(fd, POLLIN)
    let r = posix.read(fd, buf[0].addr, 80)
    if r == 0:
      break
    echo id, " read r: ", r, " buf: ", buf
  echo "read done"
  discard close(fd)


#for id in 1..99:
#  pool.pushWork slow(id, 1_000)

for id in 1..128:
  var fd: array[2, cint]
  discard posix.socketpair(AF_UNIX, SOCK_STREAM, 0, fd)
  #discard posix.pipe(fd)
  pool.pushWork reader(id, fd[0])
  pool.pushWork writer(id, fd[1])

pool.pushWork ticker()

pool.run()

