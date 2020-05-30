
import eventqueue
import nativesockets
import os

const port = 9000


# All procs happen to use the same continuation type, so I made
# only 1 here

type SocketCont = ref object of Cont
  sock: SocketHandle


# Some helper procs

proc waitForSock(s: SocketHandle, c: Cont) =
  discard addFd(s, c)

proc write(s: SocketHandle, buf: string) =
  discard send(s, buf[0].unsafeAddr, buf.len.cint, 0)

proc read(sock: SocketHandle, len=256): string =
  result = newString(len)
  let r = recv(sock, result[0].addr, len.cint, 0)
  result.setlen(r)

# proc doClient(sock: SocketHandle) =
#   sock.write("Hello! Please type something.\n")
#   while true:
#     waitForSock(sock)
#     var buf = sock.read()
#     echo "buf ", buf, " ", buf.len
#     if buf.len > 0:
#       sock.write("You sent " & $buf.len & " characters\n")
#       discard
#     else:
#       echo "Client went away"
#       break

proc doClient2(cont: Cont): Cont

proc doClient1(cont: Cont): Cont =
  var sock = cont.SocketCont.sock
  var buf = sock.read()
  echo "buf ", buf, " ", buf.len
  if buf.len > 0:
    sock.write("You sent " & $buf.len & " characters\n")
    return SocketCont(fn: doClient2, sock: sock)
  else:
    echo "Client went away"
  
proc doClient2(cont: Cont): Cont =
  var sock = cont.SocketCont.sock
  waitForSock sock, SocketCont(fn: doClient1, sock: sock)

proc doClient(cont: Cont): Cont =
  var sock = cont.SocketCont.sock
  sock.write("Hello! Please type something.\n")
  SocketCont(fn: doClient2, sock: sock)


# proc doServer(sock: SocketHandle) =
#   while true:
#     waitForSock(sock)
#     var sa: Sockaddr_in
#     var saLen = sizeof(sa).SockLen
#     let sockc = accept(sock, cast[ptr SockAddr](sa.addr), saLen.addr)
#     echo "Accepted new client"
#     doClient(sockc)

proc doServer2(c: Cont): Cont

proc doServer1(c: Cont): Cont =
  var sock = c.SocketCont.sock
  var sa: Sockaddr_in
  var saLen = sizeof(sa).SockLen
  let sockc = accept(sock, cast[ptr SockAddr](sa.addr), saLen.addr)
  echo "Accepted new client"
  SocketCont(fn: doClient, sock: sockc).run()
  SocketCont(fn: doServer2, sock: sock)
  
proc doServer2(c: Cont): Cont =
  var sock = c.SocketCont.sock
  waitForSock sock, SocketCont(fn: doServer1, sock: sock)

proc doServer(c: Cont): Cont =
  var sock = c.SocketCont.sock
  SocketCont(fn: doServer2, sock: sock)


# Create TCP server socket and coroutine

let sock = createNativeSocket()
var sa: Sockaddr_in
sa.sin_family = AF_INET.uint16
sa.sin_port = ntohs(port)
sa.sin_addr.s_addr = INADDR_ANY
setSockOptInt(sock, SOL_SOCKET, SO_REUSEADDR, 1)
discard bindAddr(sock, cast[ptr SockAddr](sa.addr), sizeof(sa).SockLen)
discard listen(sock, SOMAXCONN)

SocketCont(fn: doServer, sock: sock).run()

echo "TCP server ready on port ", port

# Run eventqueue main loop

run()

