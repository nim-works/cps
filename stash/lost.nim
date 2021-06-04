
import cps

type
  C = ref object of RootObj
    fn*: proc(c: C): C {.nimcall.}
    mom: C
    val: CData

  CData = ref object
    v1: int
    v2: string

proc `$`(c: C): string = 
  $cast[int](c)

proc pass(cFrom, cTo: C): C =
  echo "pass ", $cFrom, " -> ", $cTo
  cTo.val = move cfrom.val
  cTo
    
proc boot(c: C): C =
  echo "boot ", c
  new c.val
  c

proc jield(c: C): C {.cpsMagic.} =
  echo "jield ", c
  c

proc send(c: C, v: int) {.cpsVoodoo.} =
  echo "send ",c 
  c.val.v1 = v
  c.val.v2 = "hello"

proc recv(c: C): int {.cpsVoodoo.} =
  echo "recv ", c
  c.val.v1

proc level_two() {.cps:C.} =
  echo " two in"
  send(42)
  jield()
  echo " two recv: ", recv()

proc level_one() {.cps:C.} =
  echo "one in"
  level_two()
  echo "one recv"            # <--- we never get here
  let v = recv()
  echo "one recv: ", recv()

var a = whelp level_one()

while a.running:
  echo "tramp ", a
  a = a.fn(a)
