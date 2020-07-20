import std/os
import std/selectors
import std/monotimes
import std/nativesockets
import std/tables
import std/times
import std/deques

const
  cpsDebug {.booldefine.} = false

type
  Id = distinct int

  State = enum
    Unready = "the default state, pre-initialized"
    Stopped = "we are outside an event loop but available for queuing events"
    Running = "we're in a loop polling for events and running continuations"
    Stopping = "we're tearing down the dispatcher and it will shortly stop"

  Clock = MonoTime
  Fd = distinct int

  EventQueue = object
    state: State                    ## dispatcher readiness
    goto: Table[Id, Cont]           ## where to go from here!
    lastId: Id                      ## id of last-issued registration
    selector: Selector[Id]
    manager: Selector[Clock]
    timer: Fd
    wake: SelectEvent
    yields: Deque[Cont]

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    when cpsDebug:
      clock: Clock                  ## time of latest poll loop
      delay: Duration               ## polling overhead

const
  InvalidId = Id(0)
  InvalidFd = Fd(-1)

var eq {.threadvar.}: EventQueue

template now(): Clock = getMonoTime()

proc `$`(id: Id): string = "{" & system.`$`(id.int) & "}"
proc `$`(fd: Fd): string = "[" & system.`$`(fd.int) & "]"

proc `==`(a, b: Id): bool {.borrow.}
proc `==`(a, b: Fd): bool {.borrow.}

proc init() {.inline.} =
  ## initialize the event queue to prepare it for requests
  if eq.state == Unready:
    # create a new manager
    eq.timer = InvalidFd
    eq.manager = newSelector[Clock]()
    eq.wake = newSelectEvent()
    eq.selector = newSelector[Id]()
    # the manager wakes up when triggered to do so
    registerEvent(eq.manager, eq.wake, now())
    # so does the main selector
    registerEvent(eq.selector, eq.wake, InvalidId)
    eq.lastId = InvalidId
    eq.yields = initDeque[Cont]()
    eq.state = Stopped

proc nextId(): Id =
  ## generate a new registration identifier
  init()
  inc eq.lastId
  result = eq.lastId

proc wakeUp() =
  case eq.state
  of Unready:
    init()
  of Stopped:
    discard "ignored wake-up to stopped dispatcher"
  of Running:
    trigger eq.wake
  of Stopping:
    discard "ignored wake-up request; dispatcher is stopping"

template wakeAfter(body: untyped): untyped =
  ## wake up the dispatcher after performing the following block
  init()
  try:
    body
  finally:
    wakeUp()

proc len*(eq: EventQueue): int =
  ## the number of pending continuations
  result = len(eq.goto) + len(eq.yields)

proc `[]=`(eq: var EventQueue; id: Id; cont: Cont) =
  ## put a continuation into the queue according to its registration
  assert id != InvalidId
  assert not cont.isNil
  assert not cont.fn.isNil
  assert id notin eq.goto
  eq.goto[id] = cont

proc add*(eq: var EventQueue; cont: Cont): Id =
  ## add a continuation to the queue; returns a registration
  result = nextId()
  eq[result] = cont
  when cpsDebug:
    echo "🤞queue ", $result

proc addTimer*(cont: Cont; interval: Duration) =
  ## run a continuation after an interval
  wakeAfter:
    let fd = registerTimer(eq.selector,
      timeout = interval.inMilliseconds.int,
      oneshot = true, data = eq.add(cont)).Fd
    when cpsDebug:
      echo "⏰timer ", fd

proc addTimer*(cont: Cont; ms: int) =
  ## run a continuation after some milliseconds have passed
  let interval = initDuration(milliseconds = ms)
  addTimer(cont, interval)

proc addTimer*(cont: Cont; seconds: float) =
  ## run a continuation after some seconds have passed
  addTimer(cont, (1_000 * seconds).int)

proc addYield*(cont: Cont) =
  wakeAfter:
    addLast(eq.yields, cont)

proc stop*() =
  ## tell the dispatcher to stop
  if eq.state == Running:
    eq.state = Stopping

    # tear down the manager
    assert not eq.manager.isNil
    eq.manager.unregister eq.wake
    if eq.timer != InvalidFd:
      eq.manager.unregister eq.timer.int
      eq.timer = InvalidFd
    close(eq.manager)

    # discard the current selector to dismiss any pending events
    eq.selector.unregister eq.wake
    close(eq.selector)

    # re-initialize the queue
    eq.state = Unready
    init()

proc trampoline*(c: Cont) =
  ## trampoline
  var c = c
  while not c.isNil and not c.fn.isNil:
    when cpsDebug:
      echo "🎪tramp ", cast[uint](c.fn), " at ", c.clock
    c = c.fn(c)

proc poll*() =
  ## see what needs doing and do it
  if eq.state != Running: return

  #[

  what i want here is a way to measure the length of the selector,
  or to simply confirm that the only remaining "listener" is the
  wake-up event.

  unfortunately, isEmpty() will always be false in that case, so
  instead, we measure the number of pending continuations, which
  should be the same.

  ]#

  if len(eq) > 0:
    when cpsDebug:
      let clock = now()
    let ready = select(eq.selector, -1)

    # ready holds the ready file descriptors and their events.

    for event in items(ready):
      # get the registration of the pending continuation
      let id = getData(eq.selector, event.fd)
      # the id will be InvalidId if it's a wake-up event
      if id != InvalidId:
        var cont: Cont
        if pop(eq.goto, id, cont):
          when cpsDebug:
            cont.clock = clock
            cont.delay = now() - clock
            echo "💈delay ", id, " ", cont.delay
          trampoline cont
        else:
          raise newException(KeyError, "missing registration " & $id)

    # at this point, we've handled all timers and i/o so we can simply
    # iterate over the yields and run them.  to make sure we don't run
    # any newly-added yields in this poll, we'll process no more than
    # the current number of queued yields...

    for index in 1 .. len(eq.yields):
      let cont = popFirst eq.yields
      when cpsDebug:
        echo "🔻yield #", index
      trampoline cont

  elif eq.timer == InvalidFd:
    # if there's no timer and we have no pending continuations,
    stop()
  else:
    echo "💈"
    # else wait until the next polling interval or signal
    for ready in eq.manager.select(-1):
      # if we get any kind of error, all we can reasonably do is stop
      if ready.errorCode.int != 0:
        stop()
        raiseOSError(ready.errorCode, "cps eventqueue error")

proc run*(interval: Duration = DurationZero) =
  ## the dispatcher runs with a maximal polling interval; an interval of
  ## `DurationZero` causes the dispatcher to return when the queue is empty.

  # make sure the eventqueue is ready to run
  init()
  assert eq.state == Stopped
  if interval.inMilliseconds == 0:
    discard "the dispatcher returns after emptying the queue"
  else:
    # the manager wakes up repeatedly, according to the provided interval
    eq.timer = registerTimer(eq.manager,
                             timeout = interval.inMilliseconds.int,
                             oneshot = false, data = now()).Fd
  # the dispatcher is now running
  eq.state = Running
  while eq.state == Running:
    poll()
