import std/deques
import std/macros
import std/options
import std/strutils

import pkg/cps

###########################################################################
# Pipes implementation
###########################################################################

type
  Pipe = ref object  ## A pipe connects sender and receiver CPS procs.
    sender: Continuation
    receiver: Continuation
    value: Option[int]  ## Pipes carry integer values.

  State = enum
    Empty
    Full

proc state(pipe: Pipe): State =
  ## Pipes can be Empty or Full.
  case pipe.value.isNone
  of true: Empty
  of false: Full

## Conveniences.
template isEmpty(pipe: Pipe): bool = pipe.state == Empty
template isFull(pipe: Pipe): bool = pipe.state == Full

template isSending(pipe: Pipe): bool = pipe.sender.state == Running
template isReceiving(pipe: Pipe): bool = pipe.receiver.state == Running

proc `$`(pipe: Pipe): string =
  "<$# pipe $# $#>" % [ $pipe.state, cast[int](pipe).toHex(2), $pipe.value ]

proc wait(c: Continuation; pipe: Pipe): Continuation {.cpsMagic.} =
  ## Waits until the `pipe` is ready to be read.  Raises a ValueError
  ## if the writing side of the pipe has terminated.
  case pipe.state
  of Empty:                  # the pipe is empty, and
    if pipe.isSending:
      echo "stall"           # nothing is available to receive;
      pipe.receiver = c      # store ourselves in the pipe, and
      result = nil           # rely on pump() to resume us later.
    else:
      echo "hang-up"         # the sender is no longer running!
      raise ValueError.newException "unexpected hang-up"
  of Full:
    result = c               # no need to wait on a full pipe!

proc recv(pipe: Pipe): int {.cps: Continuation.} =
  ## Read a value from the `pipe`.
  wait pipe                  # wait until the pipe is ready.
  result = get pipe.value    # recover a result from the pipe,
  reset pipe.value           # the pipe is now empty.
  echo "recv ", result

proc isComplete(pipe: Pipe): bool =
  ## Truthy if the `pipe` has ceased.
  case pipe.state
  of Empty:
    not pipe.isSending       # it's empty and there's no sender.
  of Full:
    false                    # if it's Full, it's not complete.

proc send(c: Continuation; pipe: Pipe; value: int): Continuation {.cpsMagic.} =
  ## Send a `value` into the `pipe`.
  case pipe.state
  of Full:                   # we cannot send into a Full pipe;
    pipe.sender = nil        # rely on pump() to resume us later.
    echo "block"
  of Empty:
    pipe.value = some value  # deposit a value in the pipe.
    echo "send ", value

proc pump(pool: openArray[Pipe]) =
  ## Run all pipes to completion.
  var pool = pool.toDeque
  while pool.len > 0:
    let pipe = pool.popFirst()
    echo "\n-----", pipe, "-----"
    case pipe.state
    of Empty:
      echo "    ", pipe.sender.state, " sender"
      if pipe.isSending:
        discard trampoline pipe.sender
        pool.addLast pipe
    of Full:
      echo "    ", pipe.receiver.state, " receiver"
      if pipe.isReceiving:
        discard trampoline pipe.receiver
        pool.addLast pipe


###########################################################################
# Main program
#
# source --> speaker
#         ^
#         |
#      pipe
#
###########################################################################

proc source(pipe: Pipe; lo: int; hi: int) {.cps: Continuation.} =
  ## A simple source, generating numbers.
  var i = lo
  while i <= hi:
    pipe.send(i)
    inc i

proc speaker(pipe: Pipe) {.cps: Continuation.} =
  ## A simple sink, echoing what is received.
  var saying: int
  while not pipe.isComplete:
    echo recv(pipe), ", sayeth the speaker"

block:
  ## Create a pipe and hook it up to a source and a speaker.
  var pipe = new Pipe
  pipe.sender = whelp source(pipe, 10, 12)
  pipe.receiver = whelp speaker(pipe)

  pump [pipe]
  echo "    (end of program)\n\n"

###########################################################################
# Main program two.
#
# source --> filter --> speaker
#         ^          ^
#         |          |
#        one        two
#
###########################################################################

type
  BinaryOp = proc(x: int; y: int): int {.cps: Continuation.}

proc filter(inputs, outputs: Pipe; mutate: BinaryOp) {.cps: Continuation.} =
  ## The filter connects two pipes; it reads from one,
  ## applies a binary operation to a running value,
  ## and sends that value to the second pipe.
  var total: int
  while not inputs.isComplete:
    echo "filter value $# awaits mutation" % [ $total ]
    try:
      let value = recv inputs
      total = mutate(total, value)
      echo "filter value $# post mutation by $#" % [ $total, $value ]
    except ValueError:
      echo "broken pipe"  # exception handling
      break
    outputs.send(total)  # no error control here


proc addition(running: int; value: int): int {.cps: Continuation.} =
  ## An example mutating continuation.
  running + value


block:
  ## Create two pipes.
  ## - one connects source and filter
  ## - two connects filter and speaker

  const
    Adder {.used.} = whelp addition
  var one, two = new Pipe
  let transformer = whelp filter(one, two, Adder)

  one.sender = whelp source(one, 10, 20)
  one.receiver = transformer

  two.sender = transformer
  two.receiver = whelp speaker(two)

  pump [one, two]
  echo "    (end of program)\n\n"
