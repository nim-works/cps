
import frosty
import cps
import tables
import epoll
import heapqueue
import posix
import std/monotimes
import deques


type
  C = ref object of Continuation
    evq: Evq

  Io = object
    fd: SocketHandle
    c: C

  Timer = object
    time: float
    c: C

  Evq = ref object
    now: float
    epfd: cint
    work: Deque[C]
    timers: HeapQueue[Timer]
    ios: Table[uint64, Io]
    ioIdx: uint64

  Conn = ref object
    evq: Evq
    fd: SocketHandle
    s: string


proc pass(cFrom, cTo: C): C =
  cTo.evq = cFrom.evq
  cTo

template checkSyscall(e: typed) =
  let r = e
  if r == -1:
    raise newException(OSError, "boom r=" & $r & ": " & $strerror(errno))


############################################################################
# Minimal event queue implementation supporting a work queue, I/O and timers
############################################################################

proc newEvq(): Evq =
  Evq(
    now: getMonoTime().ticks.float / 1.0e9,
    epfd: epoll_create(1),
  )

template `<`(a, b: Timer): bool = a.time < b.time

proc push(evq: Evq, c: C) =
  ## Push work to the back of the work queue
  c.evq = evq
  evq.work.addLast c

proc iowait(c: C, conn: Conn, events: int): C {.cpsMagic.} =
  ## Suspend continuation until I/O event triggered
  inc c.evq.ioIdx
  c.evq.ios[c.evq.ioIdx] = Io(fd: conn.fd, c: c)
  var ee = EpollEvent(events: events.uint32, data: EpollData(u64: c.evq.ioIdx))
  checkSyscall epoll_ctl(c.evq.epfd, EPOLL_CTL_ADD, conn.fd.cint, ee.addr)

proc sleep(c: C, delay: float): C {.cpsMagic.} =
  ## Suspend continuation until timer expires
  c.evq.timers.push Timer(c: c, time: c.evq.now + delay)

proc run(evq: Evq) =
  ## Run the event queue

  while true:

    # Trampoline all work
    while evq.work.len > 0:
      discard trampoline(evq.work.popFirst)

    # Calculate timeout until first timer
    evq.now = getMonoTime().ticks.float / 1.0e9
    var timeout: cint = -1
    if evq.timers.len > 0:
      let timer = evq.timers[0]
      timeout = cint(1000 * (timer.time - evq.now))

    # Wait for timers or I/O
    var es: array[8, EpollEvent]
    let n = epoll_wait(evq.epfd, es[0].addr, es.len.cint, timeout)

    # Move expired timer continuations to the work queue
    evq.now = getMonoTime().ticks.float / 1.0e9
    while evq.timers.len > 0 and evq.timers[0].time <= evq.now:
      evq.push evq.timers.pop.c

    # Move triggered I/O continuations to the work queue
    for i in 0..<n:
      let idx = es[i].data.u64
      let io = evq.ios[idx]
      checkSyscall epoll_ctl(evq.epfd, EPOLL_CTL_DEL, io.fd.cint, nil)
      evq.ios.del idx
      evq.push io.c


############################################################################
# Little async socket library
############################################################################

proc listen(evq: Evq, port: int): Conn =
  var sa: Sockaddr_in6
  sa.sin6_family = AF_INET6.uint16
  sa.sin6_port = htons(port.uint16)
  sa.sin6_addr = in6addr_any
  var yes: int = 1
  let fd = socket(AF_INET6, SOCK_STREAM, 0);
  checkSyscall setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, yes.addr, sizeof(yes).SockLen)
  checkSyscall bindSocket(fd, cast[ptr SockAddr](sa.addr), sizeof(sa).SockLen)
  checkSyscall listen(fd, SOMAXCONN)
  return Conn(evq: evq, fd: fd)

proc dial(evq: Evq, ip: string, port: int): Conn =
  var res: ptr AddrInfo
  var hints: AddrInfo
  hints.ai_family = AF_UNSPEC
  hints.ai_socktype = SOCK_STREAM
  let r = getaddrinfo(ip, $port, hints.addr, res)
  if r == 0:
    let fd = socket(res.ai_family, res.ai_socktype, 0)
    checkSyscall connect(fd, res.ai_addr, res.ai_addrlen)
    freeaddrinfo(res)
    Conn(evq: evq, fd: fd)
  else:
    raise newException(OSError, "dial: " & $gai_strerror(r))

proc send(conn: Conn, s: string): int {.cps:C.} =
  iowait(conn, POLLOUT)
  let r = posix.send(conn.fd, s[0].unsafeAddr, s.len, 0)
  return r

proc recv(conn: Conn, n: int) {.cps:C.} =
  var s = newString(n)
  iowait(conn, POLLIN)
  let r = posix.recv(conn.fd, s[0].addr, n, 0)
  s.setLen if r > 0: r else: 0
  conn.s = s

proc sendFull(conn: Conn, s: string) {.cps:C.} =
  var done = 0
  var todo = s.len
  while todo > 0:
    iowait(conn, POLLOUT)
    let r = posix.send(conn.fd, s[done].unsafeAddr, todo, 0)
    if r <= 0:
      break
    done += r
    todo -= r

proc recvFull(conn: Conn, n: int) {.cps:C.} =
  var todo = n
  var s = newString(n)
  while todo > 0:
    conn.recv(todo)
    if conn.s.len == 0:
      break
    s.add conn.s
    todo -= conn.s.len
  conn.s = s

proc close(conn: Conn) =
  checkSyscall posix.close(conn.fd)


############################################################################
# Main program
############################################################################

proc doClient(conn: Conn) {.cps:C.} =
  echo "connected"

  while true:
    conn.recv(1024 * 1024)
    let s = conn.s
    if s.len == 0:
      break
    conn.sendFull(s)

  echo "disconnected"
  conn.close()


proc doServer(evq: Evq, port: int) {.cps:C.} =
  let connServer = listen(evq, port)

  while true:
    iowait(connServer, POLLIN)

    var sa: Sockaddr_in
    var saLen: SockLen
    let fd = posix.accept4(connServer.fd, cast[ptr SockAddr](sa.addr), saLen.addr, O_NONBLOCK)

    let conn = Conn(fd: fd, evq: evq)
    evq.push whelp doClient(conn)


proc ticker(evq: Evq) {.cps:C.} =
  let conn = dial(evq, "localhost", 8080)
  while true:
    echo "tick"
    sleep(1.0)


var evq = newEvq()

evq.push whelp doServer(evq, 8080)
evq.push whelp ticker(evq)

evq.run()
