
import cps, options, deques

type
  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

  Coro = ref object
    c: Cont
    sIn: string

  CoroFn = proc(coro: Coro): Cont


proc jield(c: Cont, coro: Coro): Cont {.cpsMagic.} =
  coro.c = c

proc send(coro: Coro, s: string) =
  coro.sIn = s

proc recv(coro: Coro): string =
  coro.sIn
 
proc resume(coro: Coro) =
  var c = coro.c
  while c.running:
    c = c.fn(c)

proc newCoro(fn: CoroFn): Coro =
  let coro = Coro()
  coro.c = fn(coro)
  coro.resume()
  coro


var coro1: Coro
var coro2: Coro


# This coro receives strings, accumulates and splits by newline,
# and resumes coro 2
proc fn_coro1(coro: Coro) {.cps:Cont} =
  var buf = ""
  while true:
    coro.jield()
    let s = coro.recv()
    for c in s:
      if c == '\n':
        echo "tx ", buf
        coro2.send(buf)
        coro2.resume()
        buf = ""
      else:
        buf = buf & c

# This coro receives strings from coro1 and prints them
proc fn_coro2(coro: Coro) {.cps:Cont} =
  while true:
    coro.jield()
    let s = coro.recv()
    echo "rx ", s


coro1 = newCoro(fn_coro1)
coro1.resume()

coro2 = newCoro(fn_coro2)
coro2.resume()

while true:
  coro1.send("line ")
  coro1.resume()
  coro1.send("one\nline ")
  coro1.resume()
  coro1.send("two\n")
  coro1.resume()

