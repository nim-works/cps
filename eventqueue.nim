import std/monotimes
import std/nativesockets
import std/tables
import std/times

## Basic poll based event loop

type
  Id = int

  Ticks = int64

  Timer* = ref object   ## timer handler
    id: Id
    interval: Duration
    tWhen: MonoTime
    cont: Cont
    deleted: bool

  Socker = ref object   ## socket handler
    id: Id
    sock: SocketHandle
    cont: Cont
    deleted: bool

  Evq* = object
    stop: bool
    tNow: MonoTime
    sockers: Table[Id, Socker]
    timers: Table[Id, Timer]
    nextId: Id

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}


# Continuation trampoline

proc run*(c: Cont) =
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)

var evq {.threadvar.}: Evq

template now(): MonoTime = getMonoTime()

proc nextId(): Id =
  inc evq.nextId
  return evq.nextId

# Register/unregister a file descriptor to the loop

proc addFd*(sock: SocketHandle, cont: Cont): Id =
  let id = nextId()
  evq.sockers[id] = Socker(id: id, sock: sock, cont: cont)
  return id

proc delFd*(id: Id) =
  evq.sockers[id].deleted = true
  evq.sockers.del id

# Register/unregister timers

proc addTimer*(cont: Cont; interval: Duration): Id =
  result = nextId()
  evq.timers[result] = Timer(id: result, tWhen: now() + interval,
                             interval: interval, cont: cont)

proc addTimer*(cont: Cont; ticks: int64): Id =
  result = addTimer(cont, initDuration(nanoseconds = ticks))

proc addTimer*(cont: Cont; time: float): Id =
  result = addTimer(cont, initDuration(nanoseconds = (int64) time * 1_000_000))

proc delTimer*(id: Id) =
  evq.timers[id].deleted = true
  evq.timers.del id

# Run one iteration

proc poll() =

  # Calculate sleep time

  evq.tNow = now()
  var tSleep = initDuration(milliseconds = 1)
  for id, th in evq.timers:
    if not th.deleted:
      let dt = th.tWhen - evq.tNow
      if dt > DurationZero:
        tSleep = min(tSleep, dt)

  # Collect sockets for select

  var readSocks: seq[SocketHandle]
  for id, sh in evq.sockers:
    if not sh.deleted:
      readSocks.add sh.sock

  let r = selectRead(readSocks, tSleep.inMilliseconds.int)

  # Call expired timer handlers. Don't call while iterating because callbacks
  # might mutate the list

  evq.tNow = now()
  var ths: seq[Timer]

  for id, th in evq.timers:
    if not th.deleted:
      if evq.tNow > th.tWhen:
        ths.add th

  for th in ths:
    if not th.deleted:
      th.cont.run()
      let repeat = false
      if repeat:
        th.tWhen = th.tWhen + th.interval
      else:
        delTimer(th.id)

  doAssert r != -1

  if r > 0:

    # Call sock handlers with events. Don't call while iterating because
    # callbacks might mutate the list

    var shs: seq[Socker]

    for s in readSocks:
      for id, sh in evq.sockers:
        if not sh.deleted and sh.sock == s:
          shs.add sh

    for sh in shs:
      if not sh.deleted:
        sh.cont.run()
        delFd(sh.id)


proc stop*() =
  evq.stop = true

# Run forever

proc run*() =
  while not evq.stop:
    poll()
