import std/locks
import std/selectors
import std/monotimes
import std/nativesockets
import std/tables
import std/times
import std/deques

## Basic poll based event loop

type
  Id = int

  Ticks = int64
  Clock = MonoTime
  Fd = int32

  HandlerKind = enum  ## handlers come in different flavors
    Time = "trigger after a duration or at a specific time"
    Sock = "trigger in response to read/write status on a socket"
    Poll = "trigger according to various events on a polled selector"
    File = "trigger via a simple select() on a file descriptor"
    Ring = "trigger when async i/o is available for a linux fd"
    #When = "trigger when a condition variable is signalled"

  Handler = object
    id: Id
    cont: Cont
    deleted: bool

    case kind: HandlerKind
    of Time:
      interval: Duration
      hence: Clock
    of Sock:
      sock: SocketHandle
    of Poll:
      selector: Selector[Fd]
    of Ring, File:
      fd: Fd
    #of When:
    #  cond: Cond

  Evq* = object                     ## event queue
    stop: bool                      ## stop the dispatcher
    clock: Clock                    ## current time
    sockers: Table[Id, Handler]     ## socket handlers
    timers: Table[Id, Handler]      ## timer handlers
    selectors: Deque[Handler]       ## selector handlers
    lastId: Id                      ## id of last-issued handler

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}


# Continuation trampoline

proc run*(c: Cont) =
  var c = c
  while c != nil and c.fn != nil:
    c = c.fn(c)

var evq {.threadvar.}: Evq
evq.selectors = initDeque[Handler](0)

template now(): Clock = getMonoTime()

proc nextId(): Id =
  inc evq.lastId
  result = evq.lastId

# Register/unregister a file descriptor to the loop

proc newHandler(kind: HandlerKind; cont: Cont): Handler =
  ## create a new handler for the given continuation
  result = Handler(kind: kind, cont: cont, id: nextId())

template add(tab: var Table[Id, Handler]; handler: Handler) =
  tab.add(handler.id, handler)

proc addFd*(sock: SocketHandle, cont: Cont) =
  var handler = newHandler(Sock, cont)
  handler.sock = sock
  evq.sockers.add handler

proc delFd*(id: Id) =
  evq.sockers[id].deleted = true
  evq.sockers.del id

# Register/unregister timers

proc addTimer*(cont: Cont; interval: Duration) =
  var handler = newHandler(Time, cont)
  handler.interval = interval
  handler.hence = now() + interval
  evq.timers.add handler

proc addTimer*(cont: Cont; ticks: int64) =
  let interval = initDuration(nanoseconds = ticks)
  addTimer(cont, interval)

proc addTimer*(cont: Cont; time: float) =
  let interval = initDuration(nanoseconds = (time * 1_000_000).int64)
  addTimer(cont, interval)

proc delTimer*(id: Id) =
  evq.timers[id].deleted = true
  evq.timers.del id

proc pollTimers() =
  ## Call expired timer handlers. Don't call while iterating because
  ## callbacks might mutate the list

  if len(evq.timers) > 0:
    evq.clock = now()
    var timers: seq[Handler]

    for timer in values(evq.timers):
      if not timer.deleted:
        if evq.clock > timer.hence:
          timers.add timer

    # FIXME: sort the list here

    for timer in items(timers):
      if not timer.deleted:
        run timer.cont
        delTimer timer.id

proc soonestTimer(sleepy: var Duration) =
  ## the sleep duration may be reduced by a pending timer
  if len(evq.timers) > 0:
    for timer in values(evq.timers):
      if not timer.deleted:
        let until = timer.hence - evq.clock
        if until > DurationZero:
          sleepy = min(sleepy, until)
        else:
          sleepy = DurationZero
          break

proc pollSelectors() =

proc poll() =
  ## Run one iteration of the dispatcher

  # Calculate sleep time

  evq.clock = now()
  var sleepy = initDuration(seconds = 1)

  # perhaps we should wait less than `sleepy`?
  soonestTimer(sleepy)

  # Collect sockets for select
  var ready: seq[SocketHandle]
  for socker in values(evq.sockers):
    if not socker.deleted:
      ready.add socker.sock

  let r = selectRead(ready, sleepy.inMilliseconds.int)
  assert r != -1

  # run any timers that are ready
  pollTimers()

  pollSelectors()

  # if sockets are ready, run them
  if r > 0:

    # Call sock handlers with events. Don't call while iterating because
    # callbacks might mutate the list

    var sockers: seq[Handler]

    for socket in items(ready):
      for socker in values(evq.sockers):
        if not socker.deleted and socker.sock == socket:
          sockers.add socker

    for socker in items(sockers):
      if not socker.deleted:
        run socker.cont
        delFd socker.id


proc stop*() =
  ## tell the dispatcher to stop
  evq.stop = true

proc run*() =
  ## Run forever
  while not evq.stop:
    poll()
