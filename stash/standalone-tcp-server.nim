

#
# Minimal stand alone CPS http server
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

proc io(c: Cont, fd: SocketHandle, event: int): Cont =
  var epv = EpollEvent(events: EPOLLIN)
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


# CPS client hander

proc doClient(fdc: SocketHandle): Cont {.cps.} =

  while true:
    cps io(fdc, POLLIN)
    let s: string = sockRecv(fdc)
    if s.len > 0:
      echo "recv> ", s
    else:
      return


# CPS server handler

proc doServer(port: int): Cont {.cps.} =
  let fds: SocketHandle = sockBind(port)
  while true:
    cps io(fds, POLLIN)
    let fdc: SocketHandle = sockAccept(fds)
    # Create new client and add to work queue
    evq.work.addLast doClient(fdc)


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

