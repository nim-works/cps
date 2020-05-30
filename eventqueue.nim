
# Basic poll based event loop

import nativesockets, tables, times

type

  HandlerId = int

  SockHandler = ref object
    id: HandlerId
    sock: SocketHandle
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
    sockHandlers: Table[HandlerId, SockHandler]
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
  getTime().toUnixFloat()

proc nextId(): HandlerId =
  inc evq.nextHandlerId
  return evq.nextHandlerId

# Register/unregister a file descriptor to the loop

proc addFd*(sock: SocketHandle, cont: Cont): HandlerId =
  let id = nextId()
  evq.sockHandlers[id] = SockHandler(id: id, sock: sock, cont: cont)
  return id

proc delFd*(id: HandlerId) =
  evq.sockHandlers[id].deleted = true
  evq.sockHandlers.del id

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

  # Collect sockets for select

  var readSocks: seq[SocketHandle]
  for id, sh in evq.sockHandlers:
    if not sh.deleted:
      readSocks.add sh.sock

  let r = selectRead(readSocks, int(tSleep * 1000))

  # Call expired timer handlers. Don't call while iterating because callbacks
  # might mutate the list
  
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

  if r > 0:

    # Call sock handlers with events. Don't call while iterating because
    # callbacks might mutate the list

    var shs: seq[SockHandler]

    for s in readSocks:
      for id, sh in evq.sockHandlers:
        if not sh.deleted and sh.sock == s:
          shs.add sh

    for sh in shs:
      if not sh.deleted:
        sh.cont.run()


proc stop*() =
  evq.stop = true

# Run forever

proc run*() =
  while not evq.stop:
    poll()

