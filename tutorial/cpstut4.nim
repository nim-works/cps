
# Growing your own continuations

import cps
import deques

type

  Work = ref object
    queue: Deque[Continuation]

  MyCont = ref object of Continuation
    work: Work

proc pass(cFrom, cTo: MyCont): MyCont =
  cTo.work = cFrom.work
  return cTo

proc schedule(c: MyCont): MyCont {.cpsMagic.} =
  c.work.queue.addLast c
  return nil

proc push(work: Work, c: MyCont) =
  work.queue.addLast c
  c.work = work

proc work(work: Work) =
  while work.queue.len > 0:
    discard trampoline work.queue.popFirst()

proc sayHi(name: string, i: int) {.cps:MyCont.} =
  echo "Hi ", name, " ", i
  schedule()

proc runner(name: string) {.cps:MyCont.}=
  var i = 0
  while i < 4:
    inc i
    sayHi(name, i)
  echo ""

var mywork = Work()
mywork.push whelp runner("donkey")
mywork.push whelp runner("tiger")
mywork.work()
