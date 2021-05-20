
import cps, options, deques

type
  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}

  Coro = ref object
    c: Cont
    sIn: string

  CoroFn = proc(app: App, coro: Coro): Cont

  App = ref object
    coro1: Coro
    coro2: Coro


proc jield(c: Cont, coro: Coro): Cont {.cpsMagic.} =
  coro.c = c
 
proc resume(coro: Coro) =
  var c = coro.c
  while c.running:
    c = c.fn(c)

proc send(coro: Coro, s: string) =
  coro.sIn = s
  coro.resume()

proc recv(coro: Coro): string =
  coro.sIn

proc newCoro(app: App, fn: CoroFn): Coro =
  let coro = Coro()
  coro.c = fn(app, coro)
  coro.resume()
  coro



# This coro receives strings, accumulates and splits by newline,
# and send the lines to coro2
proc fn_coro1(app: App, coro: Coro) {.cps:Cont} =
  var buf = ""
  while true:
    coro.jield()
    let s = coro.recv()
    for c in s:
      if c == '\n':
        echo "tx ", buf
        app.coro2.send(buf)
        buf = ""
      else:
        buf = buf & c

# This coro receives strings from coro1 and prints them
proc fn_coro2(app: App, coro: Coro) {.cps:Cont} =
  while true:
    coro.jield()
    let s = coro.recv()
    echo "rx ", s


let app = App()

app.coro1 = app.newCoro(fn_coro1)
app.coro1.resume()

app.coro2 = app.newCoro(fn_coro2)
app.coro2.resume()

while true:
  app.coro1.send("line ")
  app.coro1.send("one\nline ")
  app.coro1.send("two\n")

