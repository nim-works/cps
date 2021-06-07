

import cps
import options
import macros

###########################################################################
# Channels implementation
###########################################################################

# Basic empty continuation type. The rest of the bookkeeping is done in the
# Channel type below

type Cont = ref object of Continuation

# A channel connects a sender and a receiver CPS proc; it holds a continuation
# for each of them, the pump will run either one, depending on the existance of
# a value

type Channel = ref object
  cSend: Cont
  cRecv: Cont
  val: Option[int]

# Receive only stores the continuation in the channel, the
# value is taken out "by hand" for now

proc recv(c: Cont, ch: Channel): Cont {.cpsMagic.} =
  ch.cRecv = c

# Send stores the sender continuation in the channel and sets a value

proc send(c: Cont, ch: Channel, val: int): Cont {.cpsMagic.} =
  ch.val = some(val)
  ch.cSend = c

# Helper to get the value out of a channel. We need this because we
# can not return values from continuations yet

proc getval(ch: Channel): int =
  result = ch.val.get
  ch.val = none(int)

# The lady is a

proc tramp(cont: var Cont) =
  var c = Continuation: cont
  cont = nil
  while c.running:
    c = c.fn(c)

# Pump all channels in the pool until there is nothing to be done

proc pump(chs: seq[Channel]) =

  while true:
    var worked: bool
    for ch in chs:
      if isNone(ch.val) and ch.cSend != nil and ch.cSend.fn != nil:
        tramp(ch.cSend)
        worked = true
      elif isSome(ch.val) and ch.cRecv != nil and ch.cRecv.fn != nil:
        tramp(ch.cRecv)
        worked = true
    if not worked:
      break


###########################################################################
# Main program
#
# source --> sink
#         ^
#         |
#      channel
#
###########################################################################

# This is a simple source, generating numbers

proc source(ch: Channel; lo: int; hi: int) {.cps:Cont.} =
  var i = lo
  while i <= hi:
    ch.send(i)
    inc i

# This is a simple sink, echoing what is received

proc sink(ch: Channel) {.cps:Cont.} =
  while true:
    ch.recv()
    echo ch.getVal()

# Create a channel and hook it up to a source and a sink

block:
  var ch = Channel()
  ch.cSend = whelp source(ch, 10, 12)
  ch.cRecv = whelp sink(ch)

  pump(@[ch])
  echo ""

###########################################################################
# Main program two.
#
# source --> filter --> sink
#         ^          ^
#         |          |
#        ch1        ch2
#
###########################################################################

# Here is a filter, connected to two channels: it reads from one, calculates
# the running sum and sends it to the second

proc filter(chIn: Channel; chOut: Channel) {.cps:Cont.} =
  var total: int
  while true:
    recv(chIn)
    let v = chIn.getVal()
    inc total, v
    chOut.send(total)

# Create two channels.
# - ch1 connects source and filter
# - ch2 connects filter and sink

block:
  var ch1 = Channel()
  var ch2 = Channel()

  let cSource = whelp source(ch1, 10, 20)
  let cFilter = whelp filter(ch1, ch2)
  let cSink = whelp sink(ch2)

  ch1.cSend = cSource
  ch1.cRecv = cFilter
  ch2.cRecv = cFilter
  ch2.cSend = cSink

  pump(@[ch1, ch2])
  echo ""

