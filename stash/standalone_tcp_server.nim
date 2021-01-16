

#
# Tine stand alone CPS http server. Goal is to get this to absolute minimal
# system call overhead to get maximum performance serving static HTTP content
#
# Todo, in more or less this order:
#
# - Get CPS to accept the code flow
# - Implement basic HTTP protocol for serving a static html page
# - Only do epoll_ctl when needed, instead of adding/removing for every iteration
# - Add IO scheduling for send, with proper send buffer handling using writev()
# - Make multi threaded with a single event queue per thread, using EPOLLEXCLUSIVE
#

import cps
import epoll
import posix
import tables
import deques


type

  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

  Evq = object
    work: Deque[Cont]
    readers: Table[SocketHandle, Cont]


var evq: Evq
let epfd = epoll_create(1)



# CPS 'io' primitive, registers the fd with epoll and stashes the continuation.
# Will be revived later by the main loop when the fd gets ready

proc io(c: Cont, fd: SocketHandle, event: int): Cont {.cpsMagic.} =
  var epv = EpollEvent(events: event.uint32)
  epv.data.u64 = fd.uint
  discard epoll_ctl(epfd, EPOLL_CTL_ADD, fd.cint, epv.addr)
  evq.readers[fd] = c


# Some convenience functions to hide the dirty socket stuff, this keeps the CPS
# functions as clean and readable as possible

proc sockBind(port: int): SocketHandle =
  result = posix.socket(AF_INET, SOCK_STREAM, 0)
  var sas: Sockaddr_in
  sas.sin_family = AF_INET.uint16
  sas.sin_port = htons(port.uint16)
  sas.sin_addr.s_addr = INADDR_ANY
  var yes: int = 1
  doAssert setsockopt(result, SOL_SOCKET, SO_REUSEADDR, yes.addr, sizeof(yes).SockLen) != -1
  doAssert bindSocket(result, cast[ptr SockAddr](sas.addr), sizeof(sas).SockLen) != -1
  doAssert listen(result, SOMAXCONN) != -1

proc sockAccept(fds: SocketHandle): SocketHandle =
  var sac: Sockaddr_in
  var sacLen: SockLen
  posix.accept(fds, cast[ptr SockAddr](sac.addr), sacLen.addr)

proc sockRecv(fd: SocketHandle): string =
  result = newString(1024)
  let n = posix.recv(fd, result[0].addr, result.len, 0)
  if n >= 0:
    result.setlen(n)
  else:
    result.setlen(0)

proc sockSend(fd: SocketHandle, s: string) =
  let n = posix.send(fd, s[0].unsafeAddr, s.len, 0)
  assert(n == s.len)


const response = """
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 14
Connection: keep-alive

Hello, there!
"""


# CPS client hander

var clients = 0
var requests = 0

proc doClient(fdc: SocketHandle) {.cps:Cont} =

  echo "Serving client ", clients
  inc clients

  while true:
    io(fdc, POLLIN)
    echo "  Serving request ", requests
    let s: string = sockRecv(fdc)
    if s.len == 0:
      return
    io(fdc, POLLOUT)
    sockSend(fdc, response)
    inc requests
  echo "remove this line and the while breaks"


# CPS server handler

proc doServer(port: int) {.cps:Cont} =
  let fds: SocketHandle = sockBind(port)
  while true:
    io(fds, 1.int16)
    let fdc: SocketHandle = sockAccept(fds)
    # Create new client and add to work queue
    evq.work.addLast doClient(fdc)
  echo "remove this line and the while breaks"


# Create new http server and add to work queue

evq.work.addLast doServer(8000)


# Main event queue worker

while true:

  # Pump the queue until empty

  while evq.work.len > 0:
    let c = evq.work.popFirst
    let c2 = c.fn(c)
    if c2 != nil:
      evq.work.addLast c2

  # Wait for all registered file descriptors

  var events: array[8, EpollEvent]
  let n = epoll_wait(epfd, events[0].addr, events.len.cint, 1000)

  # Put continuations for all ready fds back into the queue

  for i in 0..<n:
    let fd = events[i].data.u64.SocketHandle
    evq.work.addLast evq.readers[fd]
    evq.readers.del(fd)
    discard epoll_ctl(epfd, EPOLL_CTL_DEL, fd.cint, nil)

