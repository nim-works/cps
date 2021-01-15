import balls

import cps
import cps/eventqueue

import epoll
import posix
import tables
import deques
import os

suite "echo service":
  block:
    ## zevv's echo service
    when true:
      skip"off for now"
    else:
      proc timerfd_create(clock_id: ClockId, flags: cint): cint
         {.cdecl, importc: "timerfd_create", header: "<sys/timerfd.h>".}

      proc timerfd_settime(ufd: cint, flags: cint,
                            utmr: ptr Itimerspec, otmr: ptr Itimerspec): cint
         {.cdecl, importc: "timerfd_settime", header: "<sys/timerfd.h>".}

      type

        Cont = ref object of RootObj
          fn*: proc(c: Cont): Cont {.nimcall.}

        Evq = ref object
          epfd: cint
          work: Deque[Cont]
          fds: Table[cint, Cont]
          running: bool

        Timer = proc()

      ## Event queue implementation

      proc newEvq(): Evq =
        new result
        result.epfd = epoll_create(1)

      proc stop(evq: Evq) =
        evq.running = false

      proc addWork(evq: Evq, cont: Cont) =
        evq.work.addLast cont

      proc addFd(evq: Evq, fd: SocketHandle | cint, cont: Cont) =
        evq.fds[fd.cint] = cont

      proc delFd(evq: Evq, fd: SocketHandle | cint) =
        evq.fds.del(fd.cint)

      proc io(evq: Evq, c: Cont, fd: SocketHandle | cint, event: int): Cont =
        var epv = EpollEvent(events: event.uint32)
        epv.data.u64 = fd.uint
        discard epoll_ctl(evq.epfd, EPOLL_CTL_ADD, fd.cint, epv.addr)
        evq.addFd(fd, c)

      proc sleep(evq: Evq, c: Cont, timeout: int): Cont =
        let fd = timerfd_create(CLOCK_MONOTONIC, 0)
        var ts: Itimerspec
        ts.it_interval.tv_sec = Time(timeout div 1_000)
        ts.it_interval.tv_nsec = (timeout %% 1_000) * 1_000_000
        ts.it_value.tv_sec = ts.it_interval.tv_sec
        ts.it_value.tv_nsec = ts.it_interval.tv_nsec
        check timerfd_settime(fd.cint, 0.cint, ts.addr, nil) != -1
        evq.io(c, fd, POLLIN)

      proc run(evq: Evq) =
        evq.running = true
        while true:

          # Pump the queue until empty
          while evq.work.len > 0:
            let c = evq.work.popFirst
            let c2 = c.fn(c)
            if c2 != nil:
              evq.addWork c2

          if not evq.running:
            break

          # Wait for all registered file descriptors
          var events: array[8, EpollEvent]
          let n = epoll_wait(evq.epfd, events[0].addr, events.len.cint, 1000)

          # Put continuations for all ready fds back into the queue
          for i in 0..<n:
            let fd = events[i].data.u64.cint
            evq.addWork evq.fds[fd]
            evq.delFd(fd)
            discard epoll_ctl(evq.epfd, EPOLL_CTL_DEL, fd.cint, nil)

      ## Some convenience functions to hide the dirty socket stuff, this
      ## keeps the CPS functions as clean and readable as possible

      proc sockBind(port: int): SocketHandle =
        let fds = posix.socket(AF_INET, SOCK_STREAM, 0)
        var sas: Sockaddr_in
        sas.sin_family = AF_INET.uint16
        sas.sin_port = htons(port.uint16)
        sas.sin_addr.s_addr = INADDR_ANY
        var yes: int = 1
        check setsockopt(fds, SOL_SOCKET, SO_REUSEADDR, yes.addr, sizeof(yes).SockLen) != -1
        check bindSocket(fds, cast[ptr SockAddr](sas.addr), sizeof(sas).SockLen) != -1
        check listen(fds, SOMAXCONN) != -1
        return fds

      proc sockAccept(fds: SocketHandle): SocketHandle =
        var sac: Sockaddr_in
        var sacLen: SockLen
        let fdc = posix.accept(fds, cast[ptr SockAddr](sac.addr), sacLen.addr)
        check fcntl(fdc, F_SETFL, fcntl(fdc, F_GETFL, 0) or O_NONBLOCK) != -1
        return fdc

      proc sockRecv(fd: SocketHandle): string =
        result = newString(1024)
        let n = posix.recv(fd, result[0].addr, result.len, 0)
        if n >= 0:
          result.setlen(n)
        else:
          result.setlen(0)

      proc sockSend(fd: SocketHandle, s: string) =
        let n = posix.send(fd, s[0].unsafeAddr, s.len, 0)
        check(n == s.len)

      proc sockConnect(address: string, port: int): SocketHandle =
        discard
        let fd = posix.socket(AF_INET, SOCK_STREAM, 0)
        var sas: Sockaddr_in
        sas.sin_family = AF_INET.uint16
        sas.sin_port = htons(port.uint16)
        sas.sin_addr.s_addr = inet_addr(address)
        var yes: int = 1
        check connect(fd, cast[ptr SockAddr](sas.addr), sizeof(sas).SockLen) != -1
        return fd

      var evq = newEvq()
      var count = 0
      var clients = 0

      ## CPS server session hander
      proc handleClient(fdc: SocketHandle) {.cps: Cont.} =

        inc clients

        while true:
          evq.io(fdc, POLLIN)
          let s: string = sockRecv(fdc)
          if s.len == 0: break
          inc count
          evq.io(fdc, POLLOUT)
          sockSend(fdc, s)

        dec clients
        discard fdc.close()


      ## CPS server listener handler
      proc doEchoServer(port: int) {.cps: Cont.} =
        let fds: SocketHandle = sockBind(port)
        checkpoint "listening fd: ", fds.int
        while true:
          evq.io(fds, POLLIN)
          let fdc: SocketHandle = sockAccept(fds)
          #checkpoint "accepted fd:", fdc.int
          # Create new client and add to work queue
          evq.addWork handleClient(fdc)


      ## CPS client handler
      proc doEchoClient(address: string, port: int, n: int, msg: string) {.cps: Cont.} =
        let fd: SocketHandle = sockConnect(address, port)
        #checkpoint "connected fd: ", fd.int

        var i: int = 0
        while i < n:
          evq.io(fd, POLLOUT)
          sockSend(fd, msg)
          evq.io(fd, POLLIN)
          let msg2: string = sockRecv(fd)
          check msg2 == msg
          inc i

        discard fd.close()
        #checkpoint "disconnected fd: ", fd.int

      ## Progress reporting
      proc doTicker() {.cps: Cont.} =
        while true:
          evq.sleep(1000)
          checkpoint "tick. clients: ", clients, " echoed ", count, " messages"
          if clients == 0:
            evq.stop()

      ## Spawn workers
      evq.addWork doTicker()
      evq.addWork doEchoServer(8000)
      for i in 1..100:
        evq.addWork doEchoClient("127.0.0.1", 8000,
                                 2000, "The quick brown fox jumped over the lazy dog")

      ## Forever run the event queue
      evq.run()
