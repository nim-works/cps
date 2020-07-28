import std/strutils
import std/macros
import std/os
import std/selectors
import std/monotimes
import std/nativesockets
import std/tables
import std/times
import std/deques

import sorta

import cps
import cps/semaphore

export Semaphore, semaphore.`==`, semaphore.`<`, semaphore.hash, semaphore.wait, isReady, withReady
export Event

const
  cpsDebug {.booldefine, used.} = false    ## produce gratuitous output
  cpsPoolSize {.intdefine, used.} = 64     ## expected pending continuations
  cpsTrace {.booldefine, used.} = false    ## store "stack" traces
  cpsTraceSize {.intdefine, used.} = 1000  ## limit the traceback

type
  State = enum
    Unready = "the default state, pre-initialized"
    Stopped = "we are outside an event loop but available for queuing events"
    Running = "we're in a loop polling for events and running continuations"
    Stopping = "we're tearing down the dispatcher and it will shortly stop"

  Clock = MonoTime
  Id = distinct int
  Fd = distinct int

  WaitingIds = seq[Id]
  PendingIds = Table[Semaphore, Id]

  EventQueue = object
    state: State                  ## dispatcher readiness
    pending: PendingIds           ## maps pending semaphores to Ids
    waiting: WaitingIds           ## maps waiting selector Fds to Ids
    goto: SortedTable[Id, Cont]   ## where to go from here!
    lastId: Id                    ## id of last-issued registration
    selector: Selector[Id]        ## watches selectable stuff
    yields: Deque[Cont]           ## continuations ready to run
    waiters: int                  ## a count of selector listeners

    manager: Selector[Clock]      ## monitor polling, wake-ups
    timer: Fd                     ## file-descriptor of polling timer
    wake: SelectEvent             ## wake-up event for queue actions

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}
    when cpsDebug:
      clock: Clock                  ## time of latest poll loop
      delay: Duration               ## polling overhead
      id: Id                        ## our last registration
      fd: Fd                        ## our last file-descriptor
    when cpsTrace:
      filename: string
      line: int
      column: int
      identity: string

when cpsTrace:
  type
    Frame = object
      c: Cont
      e: ref Exception
    Stack = Deque[Frame]

const
  wakeupId = Id(-1)
  invalidId = Id(0)
  invalidFd = Fd(-1)
  oneMs = initDuration(milliseconds = 1)

var eq {.threadvar.}: EventQueue

template now(): Clock = getMonoTime()

proc `$`(id: Id): string {.used.} = "{" & system.`$`(id.int) & "}"
proc `$`(fd: Fd): string {.used.} = "[" & system.`$`(fd.int) & "]"
proc `$`(c: Cont): string {.used.} =
  when cpsTrace:
    # quality poor!
    #"$1($2) $3" % [ c.filename, $c.line, c.identity ]
    c.identity
  else:
    "&" & $cast[uint](c)

proc `<`(a, b: Id): bool {.borrow, used.}
proc `<`(a, b: Fd): bool {.borrow, used.}
proc `==`(a, b: Id): bool {.borrow, used.}
proc `==`(a, b: Fd): bool {.borrow, used.}

proc put(w: var WaitingIds; fd: int | Fd; id: Id) =
  while fd.int > len(w):
    setLen(w, len(w) * 2)
  system.`[]=`(w, fd.int, id)
  case id
  of wakeupId, invalidId:             # don't count invalid ids
    discard
  else:
    inc eq.waiters
    assert eq.waiters > 0

proc get(w: var WaitingIds; fd: int | Fd): Id =
  result = w[fd.int]
  if result != wakeupId:              # don't zap our wakeup id
    if result != invalidId:           # don't count invalid ids
      dec eq.waiters
    w[fd.int] = invalidId

method clone[T: Continuation](c: T): T {.base.} =
  ## copy the continuation for the purposes of, eg. fork
  result = new T
  result[] = c[]

proc init() {.inline.} =
  ## initialize the event queue to prepare it for requests
  if eq.state == Unready:
    # create a new manager
    eq.timer = invalidFd
    eq.manager = newSelector[Clock]()
    eq.wake = newSelectEvent()
    eq.selector = newSelector[Id]()
    eq.waiters = 0

    # make sure we have a decent amount of space for registrations
    if len(eq.waiting) < cpsPoolSize:
      eq.waiting = newSeq[Id](cpsPoolSize).WaitingIds

    # the manager wakes up when triggered to do so
    registerEvent(eq.manager, eq.wake, now())

    # so does the main selector
    registerEvent(eq.selector, eq.wake, wakeupId)

    # XXX: this seems to be the only reasonable way to get our wakeup fd
    # we want to get the fd used for the wakeup event
    trigger eq.wake
    for ready in select(eq.selector, -1):
      assert User in ready.events
      eq.waiting.put(ready.fd, wakeupId)

    eq.lastId = invalidId
    eq.yields = initDeque[Cont]()
    eq.state = Stopped

proc nextId(): Id {.inline.} =
  ## generate a new registration identifier
  init()
  # rollover is pretty unlikely, right?
  when sizeof(eq.lastId) < 8:
    if (unlikely) eq.lastId == high(eq.lastId):
      eq.lastId = succ(invalidId)
    else:
      inc eq.lastId
  else:
    inc eq.lastId
  result = eq.lastId

proc newSemaphore*(): Semaphore =
  ## Create a new Semaphore.
  result.init nextId().int

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
  ## The number of pending continuations.
  result = len(eq.goto) + len(eq.yields) + len(eq.pending)

proc `[]=`(eq: var EventQueue; s: var Semaphore; id: Id) =
  ## put a semaphore into the queue with its registration
  assert id != invalidId
  assert id != wakeupId
  assert not s.isReady
  assert s.id.Id != invalidId
  eq.pending[s] = id

proc `[]=`(eq: var EventQueue; id: Id; cont: Cont) =
  ## put a continuation into the queue according to its registration
  assert id != invalidId
  assert id != wakeupId
  assert not cont.isNil
  assert not cont.fn.isNil
  assert id notin eq.goto
  eq.goto[id] = cont

proc add(eq: var EventQueue; cont: Cont): Id =
  ## Add a continuation to the queue; returns a registration.
  result = nextId()
  eq[result] = cont
  when cpsDebug:
    echo "🤞queue ", $result, " now ", len(eq), " items"

proc stop*() =
  ## Tell the dispatcher to stop, discarding all pending continuations.
  if eq.state == Running:
    eq.state = Stopping

    # tear down the manager
    assert not eq.manager.isNil
    eq.manager.unregister eq.wake
    if eq.timer != invalidFd:
      eq.manager.unregister eq.timer.int
      eq.timer = invalidFd
    close(eq.manager)

    # shutdown the wake-up trigger
    eq.selector.unregister eq.wake
    close(eq.wake)

    # discard the current selector to dismiss any pending events
    close(eq.selector)

    # discard the contents of the semaphore cache
    eq.pending = initTable[Semaphore, Id](cpsPoolSize)

    # discard the contents of the continuation cache
    eq.goto = initSortedTable[Id, Cont]()

    # re-initialize the queue
    eq.state = Unready
    init()

proc init*(c: Cont): Cont =
  result = c

when cpsTrace:
  import std/strformat

  proc init*(c: Cont; identity: static[string];
             file: static[string]; row, col: static[int]): Cont =
    result = init(c)
    result.identity = identity
    result.filename = file
    result.line = row
    result.column = col

  proc addFrame(stack: var Stack; c: Cont) =
    stack.addLast Frame(c: c)
    while len(stack) > cpsTraceSize:
      popFirst(stack)

  proc formatDuration*(d: Duration): string =
    ## format a duration to a nice string
    let
      n = d.inNanoseconds
      ss = (n div 1_000_000_000) mod 1_000
      ms = (n div 1_000_000) mod 1_000
      us = (n div 1_000) mod 1_000
      ns = (n div 1) mod 1_000
    try:
      result = fmt"{ss:>3}s {ms:>3}ms {us:>3}μs {ns:>3}ns"
    except:
      result = [$ss, $ms, $us, $ns].join(" ")

  proc `$`(f: Frame): string =
    result = $f.c
    when cpsDebug:
      let (i, took) = ("", formatDuration(f.c.delay))
      result.add "\n"
      result.add took.align(20) & " delay"

  proc writeStackTrace*(stack: Stack) =
    if len(stack) == 0:
      writeLine(stderr, "no stack recorded")
    else:
      writeLine(stderr, "noroutine stack: (most recent call last)")
      when cpsDebug:
        var prior = stack[0].c.clock
      for i, frame in pairs(stack):
        when cpsDebug:
          let took = formatDuration(frame.c.clock - prior)
          prior = frame.c.clock
          writeLine(stderr, $frame)
          writeLine(stderr, took.align(20) & " total")
        else:
          writeLine(stderr, $frame)

else:
  proc writeStackTrace*(): Cont {.cpsMagic.} =
    when declaredInScope(result):
      result = c
    warning "--define:cpsTrace:on to output traces"

proc trampoline*(c: Cont) =
  ## Run the supplied continuation until it is complete.
  var c = c
  when cpsTrace:
    var stack = initDeque[Frame](cpsTraceSize)
  while not c.isNil and not c.fn.isNil:
    when cpsDebug:
      echo "🎪tramp ", c, " at ", c.clock
    try:
      c = c.fn(c)
      when cpsTrace:
        if not c.isNil:
          addFrame(stack, c)
    except Exception:
      when cpsTrace:
        writeStackTrace(stack)
      raise

proc poll*() =
  ## See what continuations need running and run them.
  if eq.state != Running: return

  if eq.waiters > 0:
    when cpsDebug:
      let clock = now()

    # ready holds the ready file descriptors and their events.
    let ready = select(eq.selector, -1)
    for event in items(ready):
      # get the registration of the pending continuation
      let id = eq.waiting.get(event.fd)
      assert getData(eq.selector, event.fd) == id
      # the id will be wakeupId if it's a wake-up event
      assert id != invalidId
      if id == wakeupId:
        discard
      else:
        # stop listening on this fd
        unregister(eq.selector, event.fd)
        var cont: Cont
        if take(eq.goto, id, cont):
          when cpsDebug:
            cont.clock = clock
            cont.delay = now() - clock
            cont.id = id
            cont.fd = event.fd.Fd
            echo "💈delay ", id, " ", cont.delay
          trampoline cont
        else:
          raise newException(KeyError, "missing registration " & $id)

  if len(eq.yields) > 0:
    # run no more than the current number of ready continuations
    for index in 1 .. len(eq.yields):
      let cont = popFirst eq.yields
      trampoline cont

  # if there are no pending continuations,
  if len(eq) == 0:
    # and there is no polling timer setup,
    if eq.timer == invalidFd:
      # then we'll stop the dispatcher now.
      stop()
    else:
      when cpsDebug:
        echo "💈"
      # else wait until the next polling interval or signal
      for ready in eq.manager.select(-1):
        # if we get any kind of error, all we can reasonably do is stop
        if ready.errorCode.int != 0:
          stop()
          raiseOSError(ready.errorCode, "cps eventqueue error")
        break

proc run*(interval: Duration = DurationZero) =
  ## The dispatcher runs with a maximal polling interval; an `interval` of
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

proc jield*(): Cont {.cpsMagic.} =
  ## Yield to pending continuations in the dispatcher before continuing.
  wakeAfter:
    addLast(eq.yields, c)

proc sleep*(interval: Duration): Cont {.cpsMagic.} =
  ## Sleep for `interval` before continuing.
  if interval < oneMs:
    raise newException(ValueError, "intervals < 1ms unsupported")
  else:
    wakeAfter:
      let id = eq.add(c)
      let fd = registerTimer(eq.selector,
        timeout = interval.inMilliseconds.int,
        oneshot = true, data = id)
      eq.waiting.put(fd, id)
      when cpsDebug:
        echo "⏰timer ", fd.Fd

proc sleep*(ms: int): Cont {.cpsMagic.} =
  ## Sleep for `ms` milliseconds before continuing.
  let interval = initDuration(milliseconds = ms)
  sleep(c, interval)

proc sleep*(secs: float): Cont {.cpsMagic.} =
  ## Sleep for `secs` seconds before continuing.
  sleep(c, (1_000 * secs).int)

proc discart*(): Cont {.cpsMagic.} =
  ## Discard the current continuation.
  discard

proc noop*(): Cont {.cpsMagic.} =
  ## A primitive that merely sheds scope.
  result = c

template signalImpl(s: Semaphore; body: untyped): untyped =
  ## run the body when when semaphore is NOT found in the queue
  var trigger = false
  var id = invalidId
  try:
    if take(eq.pending, s, id):
      var c: Cont
      if take(eq.goto, id, c):
        addLast(eq.yields, c)
        trigger = true
    else:
      body
  finally:
    if trigger:
      wakeUp()

proc signal*(s: var Semaphore) =
  ## Signal the given Semaphore `s`, causing the first waiting continuation
  ## to be queued for execution in the dispatcher; control remains in
  ## the calling procedure.
  semaphore.signal s
  withReady s:
    init()
    signalImpl s:
      discard

proc signalAll*(s: var Semaphore) =
  ## Signal the given Semaphore `s`, causing all waiting continuations
  ## to be queued for execution in the dispatcher; control remains in
  ## the calling procedure.
  semaphore.signal s
  if s.isReady:
    init()
    while true:
      signalImpl s:
        break

proc wait*(s: var Semaphore): Cont {.cpsMagic.} =
  ## Queue the current continuation pending readiness of the given
  ## Semaphore `s`.
  let id = nextId()
  if s.isReady:
    addLast(eq.yields, c)
    wakeUp()
  else:
    eq[s] = id
    eq[id] = c

proc fork*(): Cont {.cpsMagic.} =
  ## Duplicate the current continuation.
  result = c
  wakeAfter:
    addLast(eq.yields, clone(c))

proc spawn*(c: Cont) =
  ## Queue the supplied continuation `c`; control remains in the calling
  ## procedure.
  wakeAfter:
    addLast(eq.yields, c)

proc io*(file: int | SocketHandle; events: set[Event]): Cont {.cpsMagic.} =
  ## Continue upon any of `events` on the given file-descriptor or
  ## SocketHandle.
  if len(events) == 0:
    raise newException(ValueError, "no events supplied")
  else:
    wakeAfter:
      let id = eq.add(c)
      registerHandle(eq.selector, file, events = events, data = id)
      eq.waiting.put(file.int, id)
      when cpsDebug:
        echo "📂file ", $Fd(file)
