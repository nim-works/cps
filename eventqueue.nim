
# Basic poll based event loop

import posix, tables

export POLLIN, POLLOUT, POLLERR

type

  HandlerId = int

  FdHandler = ref object
    id: HandlerId
    fd: int
    events: int
    cont: Cont
    deleted: bool

  TimerHandler* = ref object
    id: HandlerId
    interval: float
    tWhen: float
    cont: Cont
    deleted: bool

  Evq* = object
    stop: bool
    tNow: float
    fdHandlers: Table[HandlerId, FdHandler]
    timerHandlers: Table[HandlerId, TimerHandler]
    nextHandlerId: HandlerId

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont


# Continuation trampoline

proc run*(c: Cont) =
  var c = c
  while c.fn != nil:
    c = c.fn(c)


var evq {.threadvar.}: Evq

proc now(): float =
  var ts: Timespec
  discard clock_gettime(CLOCK_MONOTONIC, ts)
  return ts.tv_sec.float + ts.tv_nsec.float * 1.0e-9

proc nextId(): HandlerId =
  inc evq.nextHandlerId
  return evq.nextHandlerId

# Register/unregister a file descriptor to the loop

proc addFd*(fd: int, events: int, cont: Cont): HandlerId =
  let id = nextId()
  evq.fdHandlers[id] = FdHandler(id: id, fd: fd, events: events, cont: cont)
  return id

proc delFd*(id: HandlerId) =
  evq.fdHandlers[id].deleted = true
  evq.fdHandlers.del id

# Register/unregister timers

proc addTimer*(interval: float, cont: Cont): HandlerId =
  let id = nextId()
  evq.timerHandlers[id] = TimerHandler(id: id, tWhen: now()+interval, interval: interval, cont: cont)
  return id

proc delTimer*(id: HandlerId) =
  evq.timerHandlers[id].deleted = true
  evq.timerHandlers.del id

# Run one iteration

proc poll() =

  # Calculate sleep time
  
  evq.tNow = now()
  var tSleep = 100.0
  for id, th in evq.timerHandlers:
    if not th.deleted:
      let dt = th.tWhen - evq.tNow
      tSleep = min(tSleep, dt)

  # Collect file descriptors for poll set

  var pfds: seq[TPollfd]
  for id, fdh in evq.fdHandlers:
    if not fdh.deleted:
      pfds.add TPollfd(fd: fdh.fd.cint, events: fdh.events.cshort)

  let r = posix.poll(if pfds.len > 0: pfds[0].addr else: nil, pfds.len.Tnfds, int(tSleep * 1000.0))

  # Call expired timer handlers
  
  evq.tNow = now()
  var ths: seq[TimerHandler]

  for id, th in evq.timerHandlers:
    if not th.deleted:
      if evq.tNow > th.tWhen:
        ths.add th

  for th in ths:
    if not th.deleted:
      th.cont.run()
      let repeat = false
      if repeat:
        th.tWhen += th.interval
      else:
        delTimer(th.id)

  # Call fd handlers with events

  if r == 0:
    return

  var fdhs: seq[FdHandler]

  for pfd in pfds:
    if pfd.revents != 0:
      for id, fdh in evq.fdHandlers:
        if not fdh.deleted and fdh.fd == pfd.fd and pfd.revents == fdh.events:
          fdhs.add fdh

  for fdh in fdhs:
    if not fdh.deleted:
      fdh.cont.run()


proc stop*() =
  evq.stop = true

# Run forever

proc run*() =
  while not evq.stop:
    poll()

