
# Continuation Passing Style (CPS) in Nim

This is a test bench to see what needs to be done for implementing CPS with Nim
macros. The goal is to provide a method for transforming any Nim proc into a
series of procs passing continuations, and using this as building blocks to
implement non-linear control flow like ultra-light coroutines and iterators.

There is no macro yet, The example code in main.nim has two procs that are
manually translated to CPS form, and are run concurrently with the help of
simple event loop.

If this all works out right, the next step would be to see if the
transformation can be moved into the Nim compiler itself.

This project is releated to https://github.com/zevv/nimcoro/, which uses
ucontext based coroutines with all the overhead and complications.


## Implementation notes

Continuations are implemented as objects inheriting from `Cont`, and consist of
a function pointer and the inherited `Cont` type that holds the lifted
arguments for this function

The proc is split up into separate procs by the rules given in [1] and [2].
Each proc contains a small preamble that extracts the lifted arguments from its
own continuation object and places these in local variables. Each proc creates
a new continuation of the specific type for the next proc to be called and
places the arguments to that proc in it.

Continuations are run by a trampoline that executes each proc until an empty
continuation is returned.

The `sleep()` proc takes the passed continuation and puts it on the event
queue, which will resume the continuation after the given delay.

1. https://www.irif.fr/~jch/cpc.pdf
2. https://arxiv.org/pdf/1011.4558.pdf


## Conversion example

### Original proc:

```
proc tocker(start: int) =
  var j = start
  while true:
    sleep()
    echo "tock"
    dec j
```

### Splitting with 'virual' gotos

This step makes sure all CPS calls are followed by a goto, and control flow is
made explicit with gotos as well. (pseudo-Nim, can not be implemented directly
in Nim AST, as there is no way to represent the gotos.)

```
proc tocker(start: int) =
  var j = start
  label2:
    sleep()
    goto label1:
    label1:
    echo "tock"
    dec j
    goto label2:
```

### Converting gotos to nested procs

This step replaces the goto blocks by procs, which will always have a tail cail
to another block.

```
proc tocker(start: int) =
  var j = start
  tocker2()
  proc tocker2() =
    sleep()
    tocker1()
    proc tocker1() =
      echo "tock"
      dec j
      tocker2()
```

### Lambda lifting

This step takes out variables that are used in nested procs and makes
them explicit as proc arguments.

```
proc tocker(start: int) =
  var j = start
  tocker2(j)
  proc tocker2(j: int) =
    sleep()
    tocker1(j)
    proc tocker1(j: int) =
      echo "tock"
      dec j
      tocker2(j)
```

### Moving procs to top level

There is no need to keep the procs nested anymore, the can all be taken
out and moved to top level:

```
proc tocker1(j: int) =
  echo "tock"
  dec j
  tocker2(j)

proc tocker2(j: int) =
  sleep()
  tocker1(j)

proc tocker(start: int) =
  var j = start
  tocker2(j)
```

### Continuations

Moving arguments into continuation types for each proc and returning the
continuation instead of doing a tail call. Each proc gets a preamble to
retreive the original arguments from the continuation object.

```
type
  tocker1Cont = object of Cont
    j: int

  tocker2Cont = object of Cont
    j: int
  
  tockerCont = object of Cont
    start: int

proc tocker1(c: Cont): Cont =
  var j = c.Tocker1Cont.j
  echo "tock"
  dec j
  tocker2Cont(fn: tocker2, j: j)

proc tocker2(c: Con): Cont =
  var j = c.tocker2Cont.j
  sleep(tocker1Cont(fn: tocker1, j))

proc tocker(c: Cont): Cont =
  var j = c.TockerCont.start
  tocker2Cont(fn: tocker2, j: j)
```
