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

  Handler[C] = object
    id: Id
    cont: C
    fn: Fn[C]
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

  Evq*[C] = object                  ## event queue
    stop: bool                      ## stop the dispatcher
    clock: Clock                    ## current time
    sockers: Table[Id, Handler[C]]  ## socket handlers
    timers: Table[Id, Handler[C]]   ## timer handlers
    selectors: Deque[Handler[C]]    ## selector handlers
    lastId: Id                      ## id of last-issued handler

  Fn*[C] = proc(c: var C): pointer {.nimcall.}
  Cont* = object
    i*: int
  Continuation = concept c, type C
    C is object

# Continuation trampoline

proc run*[C: Continuation](c: var C; fn: Fn[C]) =
  var fn = fn
  while fn != nil:
    fn = cast[Fn[C]](fn(c))

proc run*[C: Continuation](c: C; fn: Fn[C]) =
  var c = c
  run(c, fn)

proc init[C](evq: var Evq[C]) =
  evq.selectors = initDeque[Handler[C]](4)

var evq {.threadvar.}: Evq[Cont]
init evq

template rightNow(): Clock = getMonoTime()

proc nextId(): Id =
  inc evq.lastId
  result = evq.lastId

# Register/unregister a file descriptor to the loop

proc initHandler[C](kind: HandlerKind; cont: C; fn: Fn): Handler[C] =
  ## create a new handler for the given continuation
  result = Handler[C](kind: kind, cont: cont, fn: fn, id: nextId())

template add(tab: var Table[Id, Handler]; handler: Handler) =
  tab.add(handler.id, handler)

proc addFd*(sock: SocketHandle; cont: Continuation; fn: Fn) =
  var handler = initHandler(Sock, cont, fn)
  handler.sock = sock
  evq.sockers.add handler

proc delFd*(id: Id) =
  evq.sockers[id].deleted = true
  evq.sockers.del id

# Register/unregister timers

proc addTimer*[C](cont: C; fn: Fn[C]; interval: Duration) =
  var handler = initHandler(Time, cont, fn)
  handler.interval = interval
  handler.hence = rightNow() + interval
  evq.timers.add handler

proc addTimer*[C](cont: C; fn: Fn[C]; ticks: int64) =
  let interval = initDuration(nanoseconds = ticks)
  addTimer(cont, fn, interval)

proc addTimer*[C](cont: C; fn: Fn[C]; time: float) =
  let interval = initDuration(nanoseconds = (time * 1_000_000).int64)
  addTimer(cont, fn, interval)

proc delTimer*(id: Id) =
  evq.timers[id].deleted = true
  evq.timers.del id

proc pollTimers[C](evq: var Evq[C]) =
  ## Call expired timer handlers. Don't call while iterating because
  ## callbacks might mutate the list

  if len(evq.timers) > 0:
    evq.clock = rightNow()
    var timers: seq[Handler[C]]

    for timer in values(evq.timers):
      if not timer.deleted:
        if evq.clock > timer.hence:
          timers.add timer

    # FIXME: sort the list here

    for timer in items(timers):
      if not timer.deleted:
        run(timer.cont, timer.fn)
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

proc poll[C](evq: var Evq[C]) =
  ## Run one iteration of the dispatcher

  # Calculate sleep time

  evq.clock = rightNow()
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
  pollTimers evq

  # if sockets are ready, run them
  if r > 0:

    # Call sock handlers with events. Don't call while iterating because
    # callbacks might mutate the list

    var sockers: seq[Handler[C]]

    for socket in items(ready):
      for socker in values(evq.sockers):
        if not socker.deleted and socker.sock == socket:
          sockers.add socker

    for socker in items(sockers):
      if not socker.deleted:
        run(socker.cont, socker.fn)
        delFd socker.id


proc stop*() =
  ## tell the dispatcher to stop
  evq.stop = true

proc forever*() =
  ## Run forever
  while not evq.stop:
    poll evq
    if evq.timers.len == 0:
      break
