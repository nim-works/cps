import std/selectors
import std/monotimes
import std/nativesockets
import std/tables
import std/times

## Basic poll based event loop

type
  Id = int

  State = enum
    Stopped
    Running
    Stopping

  Clock = MonoTime
  Fd = int

  EventQueue = object
    state: State                    ## dispatcher readiness
    clock: Clock                    ## time of latest poll loop
    goto: Table[Id, Cont]           ## where to go from here!
    lastId: Id                      ## id of last-issued registration
    selector: Selector[Id]
    manager: Selector[Clock]
    timer: Fd

  Cont* = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

var eq {.threadvar.}: EventQueue
eq.selector = newSelector[Id]()
let WakeUp = newSelectEvent()

template now(): Clock = getMonoTime()

proc nextId(): Id =
  inc eq.lastId
  result = eq.lastId

template wakeAfter(body: untyped): untyped =
  try:
    body
  finally:
    if eq.state == Running:
      trigger WakeUp

proc `[]=`(eq: var EventQueue; id: Id; cont: Cont) =
  assert id != 0
  assert not cont.isNil
  assert not cont.fn.isNil
  assert id notin eq.goto
  eq.goto[id] = cont

proc add(eq: var EventQueue; cont: Cont): Id =
  result = nextId()
  eq[result] = cont

proc addTimer*(cont: Cont; interval: Duration) =
  wakeAfter:
    let fd = eq.selector.registerTimer(
      timeout = interval.inMilliseconds.int,
      oneshot = true, data = eq.add(cont))
    echo "added timer ", fd

proc addTimer*(cont: Cont; ms: int) =
  let interval = initDuration(milliseconds = ms)
  addTimer(cont, interval)

proc addTimer*(cont: Cont; seconds: float) =
  let interval = initDuration(milliseconds = (1_000 * seconds).int)
  addTimer(cont, interval)

proc stop*() =
  ## tell the dispatcher to stop
  if eq.state == Running:
    eq.state = Stopping

    # tear down the manager
    assert not eq.manager.isNil
    eq.manager.unregister WakeUp
    assert eq.timer != -1
    eq.manager.unregister eq.timer
    eq.timer = -1
    close(eq.manager)

    # discard the current selector to dismiss any pending events
    close(eq.selector)
    # open a new one so we can attach events while stopped
    eq.selector = newSelector[Id]()

    # the dispatcher is now stopped
    eq.state = Stopped

proc run*(c: Cont) =
  ## trampoline
  var c = c
  while not c.isNil and not c.fn.isNil:
    c = c.fn(c)

proc poll() =
  ## see what needs doing and do it
  if eq.state != Running: return

  if isEmpty(eq.selector):
    stop()
  else:
    let clock = now()
    let ready = eq.selector.select(-1)
    if len(ready) > 0:
      for event in items(ready):
        echo event.fd, " is ready"

    if eq.state == Running:
      # wait until the next polling interval or signal
      discard eq.manager.select(-1)

proc run*(interval: Duration = initDuration(seconds = 1)) =
  ## the dispatcher runs with a maximal polling interval
  let began = now()
  assert eq.state == Stopped
  # create a new manager
  eq.manager = newSelector[Clock]()
  # the manager wakes up repeatedly
  eq.timer = registerTimer(eq.manager,
                           timeout = interval.inMilliseconds.int,
                           oneshot = false, data = began)
  # the manager wakes up when triggered to do so
  eq.manager.registerEvent(WakeUp, began)
  # the dispatcher is now running
  eq.state = Running
  while eq.state == Running:
    poll()
