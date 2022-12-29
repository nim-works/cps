# Continuation-Passing Style

[![Test Matrix](https://github.com/nim-works/cps/workflows/CI/badge.svg)](https://github.com/nim-works/cps/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/nim-works/cps?style=flat)](https://github.com/nim-works/cps/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.5.1-informational?style=flat&logo=nim)
![Recommended Nim version](https://img.shields.io/badge/nim-1.6.11-informational?style=flat&logo=nim)
![Maximum supported Nim version](https://img.shields.io/badge/nim-1.9.1-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/nim-works/cps?style=flat)](#license)
[![Matrix](https://img.shields.io/matrix/cps:matrix.org?style=flat&logo=matrix)](https://matrix.to/#/#cps:matrix.org)
[![IRC](https://img.shields.io/badge/chat-%23cps%20on%20libera.chat-brightgreen?style=flat)](https://web.libera.chat/#cps)

This project provides a `cps` pragma which you can add to a procedure to
automatically rewrite it to use continuations for control-flow. This provides
the benefits of CPS while abstracting away the verbosity of continuations.

The `cps` pragma performs only the control-flow rewrite; you implement or
import a dispatcher to define both the type and behavior of your continuations,
so there is virtually no API and no limitations on composition.

A substantial effort to demystify this style of programming, and what it may
enable, lives [in the docs/ subdirectory](https://github.com/nim-works/cps/tree/master/docs).

We also have [a tutorial to help new users on their way to get starting with
CPS](https://github.com/nim-works/cps/blob/master/tutorial/README.md).

For a description of the origins of our approach, see the included papers and
https://github.com/nim-lang/RFCs/issues/295, where we write in more depth about
why the implementation exists, goals for future development, etc.

## What Are These Continuations Good For?

The continuations produced by this macro…

- compose efficient and idiomatic asynchronous code
- are over a thousand times lighter than threads
- are leak-free under Nim's ARC/ORC memory management
- may be based upon your own custom `ref object` type
- may be dispatched using your own custom dispatcher
- may be moved between threads to parallelize execution
- are faster and lighter than async/await futures
- are _now_ *2.5x slower* than native closure iterators
- exploit no unsafe features of the language (`cast`, `ptr`, `addr`, `emit`)

## This is Work In Progress!

The macro itself should be considered beta quality. Corner-cases are being
nailed down and the API is being adjusted as demonstration applications are
built and rough edges are identified.

## Uh, So Should I Use it?

Yes. We need more people building toys and small projects and giving feedback
on what works well and what doesn't.

The `Continuation` type may be changed into a generic soon. If that doesn't
scare you, then you are as safe to use CPS in larger projects as the quality of
your tests; the API won't break you badly.

#### CPS has no runtime library requirements

## Cool, How Do I Use It?

### Architecture

The implementation is comprised of two concepts:

1. an *environment* is a bespoke type made to carry all locals in a procedure,
plus a pointer `fn` to a continuation leg, and a `mom` pointer to any parent
continuation:

```nim
type
  Continuation* = ref object of RootObj
    fn*: proc(c: Continuation): Continuation {.nimcall.}
    mom*: Continuation
```

2. a *trampoline* is a while loop that looks like this:
```nim
result = input_continuation
while result != nil and result.fn != nil:
  result = result.fn(result)
```

We call the instantiated environment with its `fn` pointer a _continuation_,
and we call anything that invokes a continuation's `fn` pointer a _dispatcher_.

### Application

The `cps` macro is applied to procedure definitions, and it takes a single
argument: the type you wish your continuations to inherit from. This
parent type must be a `ref object of Continuation`. You can simply use the
`Continuation` type itself, if you prefer.

```nim
type
  Whoosh = ref object of Continuation
```

We use a procedure definition to define the continuation. The type we want to
base the continuation upon is supplied as the only argument to our `cps` pragma
macro.

```nim
type
  Whoosh = ref object of Continuation

proc zoom() {.cps: Whoosh.} =
  ## a very fast continuation
  discard
```

Calling the procedure (with arguments, as required) runs the continuation to
completion.

```nim
type
  Whoosh = ref object of Continuation

proc zoom() {.cps: Whoosh.} =
  ## a very fast continuation
  discard

zoom()
echo "that was fast!"
```

You can extend the `Continuation` type to carry state during the execution of
your continuation.

```nim
type
  Count = ref object of Continuation
    labels: Table[string, Continuation.fn]
```

Here we've introduced a table that maps strings to a continuation "leg", or
slice of control-flow that is executed as a unit.

Now we'll introduce two magical procedures for manipulating the table.

```nim
proc label(c: Count; name: string): Count {.cpsMagic.} =
  c.labels[name] = c.fn
  result = c

proc goto(c: Count; name: string): Count {.cpsMagic.} =
  c.fn = c.labels[name]
  result = c
```

These pragma'd procedures act as continuation legs and we can use them in our
continuations without supplying the initial `Count` argument.

```nim
proc count(upto: int): int {.cps: Count.} =
  ## deploy the Count to make counting fun again;
  ## this continuation returns the number of trips through the goto
  var number = 0
  label: "again!"
  inc number
  echo number, "!"
  echo number, " loops, ah ah ah!"
  if number < upto:
    goto "again!"
  echo "whew!"
  return number

const many = 1_000_000_000
assert many + 1 == count(many)  # (this might take awhile to finish)
```

Sometimes you don't want to do a lot of counting right away, but, y'know, maybe
a bit later, after your nap. In that case, you can use `whelp` to instantiate
your continuation with arguments, without actually running it.

```nim
var later = whelp count(1_000_000)
echo "nap time!"
sleep 30*60*1000
```

When you're ready, the `trampoline` will run your continuation to completion
and bounce it back to you.

```nim
var later = whelp count(1_000_000)
sleep 30*60*1000
echo "it's later!  time to count!"
later = trampoline later
```

Continuations have a simple `state(c: Continuation): State` enum that is helped
into `running()`, `finished()`, and `dismissed()` boolean predicates.

```nim
var later = whelp count(1_000_000)
sleep 30*60*1000
echo "it's later!  time to count!"
later = trampoline later
assert later.finished, "laws of physics lost their sway"
```

Continuations can themselves be called in order to retrieve their return value.

```nim
var later = whelp count(1_000_000)
sleep 30*60*1000
echo "it's later!  time to count!"
later = trampoline later
assert later.finished, "laws of physics lost their sway"
echo "i counted ", later(), " trips through the goto"
```

In fact, such a call will run the continuation on your behalf, as well.

```nim
var later = whelp count(1_000_000)
sleep 30*60*1000
echo "it's later!  time to count!"
echo "i counted ", later(), " trips through the goto"
```

### Walkthrough for the `coroutine.nim` example

[examples/coroutine.nim](https://github.com/nim-works/cps/blob/master/examples/coroutine.nim)
shows a basic implementation of coroutines communicating with each other on top
of CPS.

We're going to walk through the execution of this example looking into the
details as we go, but first let's introduce the `Coroutine` type:

```nim
type
  Coroutine = ref object of Continuation
    data: int
    next: Coroutine
```

This is a `Continuation` extended with the `data` on which we're going to perform
a computation and a reference to some other `Coroutine` object. This will allow
us to resume the execution of one coroutine from the other.

The execution starts with instantiating our coroutines, which is necessary for
continuations. The order is reversed because we need to pass a specific instance
of the consumer to the filter. `filter` also takes an anonymous function as its
second argument; in our case it's a simple doubling written in a short form
provided by the `std/sugar` module.

```nim
let coro2 = whelp consumer()
let coro1 = whelp filter(coro2, x => x * 2)
```

Both coroutines are already "running" (`coroX.running == true`) but nothing is
executed yet. For this we need to actually launch them:

```nim
discard coro1.resume()
discard coro2.resume()
```

The `resume` proc is our dispatcher for the coroutines and it's what actually
runs the continuation via the call to the function pointer stored in the
continuation:

```nim
proc resume(c: Coroutine): Coroutine {.discardable.} =
  var c = Continuation c
  while c.running:
    c = c.fn(c)
  result = Coroutine c
```

Actually, this is such a basic pattern for CPS code that the library provides a
[trampoline](https://nim-works.github.io/cps/cps/spec.html#trampoline%2CsinkT)
template for this, and it would be preferable to just use it here. Also, notice
how before invoking the function pointer we needed to convert a `Coroutine` to
its base type so we could set `c` to `fn(c)`, which returns a `Continuation`.

So, `resume` launches the `filter` coroutine:

```nim
proc filter(dest: Coroutine, f: proc(x: int): int) {.cps:Coroutine.} =
  while true:
    jield()
    let n = f(recv())
    dest.send(n)
```

As the first coroutine launches, it yields (suspends execution) by calling
`jield` immediately (*yield* is a keyword in Nim). This is necessary because we
haven't actually sent any data to process, yet we need the Coroutines launched
and waiting for it. We could move the suspension points later, but we don't want
the coroutines processing the initialized-by-default value prior to the data we
send them (try moving "jields" around and inspect the results).

```nim
proc jield(c: Coroutine): Coroutine {.cpsMagic.} =
  c.next = c
  return nil
```

Our `jield` proc is a special function, as signified by the `{.cpsMagic.}`
pragma. It allows controlling the execution flow of the continuation by
returning the continuation leg to run next. Usually, it's the same as the proc's
argument, but in our case, we store the passed continuation in the `next`.
This way our coroutine could be resumed later.

Since the `cpsMagic` `jield` returns `nil` the coroutine gets suspended. Notice,
how CPS manages the control flow for us and hides the implementation details
behind ~~a veil of magic~~ code transformations.

`coro2` is launched next by calling `resume` and in the same manner `consumer`
starts running and yields immediately.

Next we start feeding our coroutines the data in a loop:

```nim
for i in 1..10:
  coro1.send(i)
```

The `send` proc just sets the data the coroutine holds to the supplied number
and calls `resume`, which takes us back to the previous `jield`, currently in
the `filter` continuation. There we receive the data and try to process it with
the passed function:

```nim
let n = f(recv())
```

The `recv` proc is another special one. `{.cpsVoodoo.}` allows returning the
contents of the concrete instance of the running continuation (which is not
accessible directly from the coroutine code of `filter` and `consumer`).
`cpsVoodoo` functions are very similar to `cpsMagic` ones, as they both can
access the contents of continuation and can only be called from the CPS
functions (`filter` and `consumer`). While `cpsMagic` procs return the
continuation (see `jield` description above), `cpsVoodoo` procs return
arbitrary data.

```nim
proc recv(c: Coroutine): int {.cpsVoodoo.} =
  c.data
```

Then we pass the processed data to the destination `consumer` coroutine:

```nim
dest.send(n)
```

Again, the *sending* is just updating the state of the continuation and resuming
it from `dest.next`. *Receiving*, then done by `consumer`, is just getting that
data from the continuation.

After setting the `value` to the received integer, we're finally able to print
it:

```nim
let value = recv()
echo value
```

Since we're at the end of the code of both coroutines, they loop and yield
again, now in the opposite order: first the `consumer` and then the `filter`
suspends, which returns us to the main loop, ready to go again.

## Dispatchers

### Notes on the Example Dispatcher

An example dispatcher was included in the past, but demonstrating dispatch
conflated the purpose of the `cps` macro and made misconceptions about the role
of continuation-versus-dispatcher common. The reference dispatcher can now be
found at https://github.com/disruptek/eventqueue and you can also jump directly to
[the documentation](https://disruptek.github.io/eventqueue/eventqueue.html).

### Other Available Dispatchers

- https://github.com/alaviss/nim-sys -- next generation OS services
- https://github.com/disruptek/passenger -- create a graph of continuation runtime
- https://github.com/disruptek/supervisors -- simple dispatch patterns for composition

## Documentation

See [the documentation for the cps module](https://nim-works.github.io/cps/cps.html) as generated directly from the source.

## Examples

A small collection of examples provides good demonstration of multiple patterns
of CPS composition. Each example runs independently, with no other requirements,
yet demonstrates different exploits of `cps`.

| Example | Description |
|     --: | :--         |
|[Channels](https://github.com/nim-works/cps/blob/master/examples/channels.nim)|A channel connects sender and receiver continuations|
|[Goto](https://github.com/nim-works/cps/blob/master/examples/goto.nim)|Implementation of `label` and `goto` statements using CPS|
|[Iterator](https://github.com/nim-works/cps/blob/master/examples/iterator.nim)|A simple demonstration of a CPS-based iterator|
|[Coroutines](https://github.com/nim-works/cps/blob/master/examples/coroutine.nim)|A pair of continuations communicate as coroutines|
|[Lazy](https://github.com/nim-works/cps/blob/master/examples/lazy.nim)|Lazy streams are composed by continuations in a functional style|
|[TryCatch](https://github.com/nim-works/cps/blob/master/examples/trycatch.nim)|Exception handling is reimplemented using only CPS|
|[CpsCps](https://github.com/nim-works/cps/blob/master/examples/cpscps.nim)|Continuations can efficiently call other continuations|
|[Work](https://github.com/nim-works/cps/blob/master/examples/work.nim)|Implementation of a simple continuation scheduler|
|[LuaCoroutines](https://github.com/nim-works/cps/blob/master/examples/lua_coroutines.nim)|Coroutines implemented in the style of Lua|
|[ThreadPool](https://github.com/nim-works/cps/blob/master/examples/threadpool.nim)|1,000,000 continuations run across all your CPU cores|
|[WebServer](https://github.com/zevv/cpstest)|Zevv's "Real World Test" WebServer And More|
|[Background](https://github.com/disruptek/background)|Run any function on a background thread|

## Debugging

### Expectations

See [this list of open Nim issues surfaced by CPS
development](https://github.com/nim-lang/Nim/issues?q=is%3Aopen+is%3Aissue+label%3ACPS); some repercussions include the following:

- Exceptions are evaluated differently under `panics:on` and `panics:off`, so
  you may need to use `panics:on` in order to produce correct code.

- Expressions are evaluated differently under `gc:[ao]rc`, so you may need to
  use those memory managers in order to produce correct code.

- The `cpp` backend often doesn't work, particularly due to faulty codegen but
  also, perhaps, due to `exceptions:goto` assumptions that we rely upon.

- `var`/`openArray`/`varargs` parameters to procedures with the `cps` pragma
  are not supported.

- Nim's `for` loops work, but you cannot perform any CPS control-flow inside of
  them; if in doubt, use a `while` loop instead.

- Generic continuations such as the following won't work without changes to
the compiler.

```nim
type
  MyContinuation[T] = ref object of Continuation
    something: T
```

### Call Syntax

Use `--define:cpsNoCallOperator` to explicitly disable the `()` operator.

### Performance

If you are not running with `define:danger` and `gc:arc` and `panics:on` then
performance considerations really aren't your primary consideration, right?

### Tracing

There are two tracing systems that are enabled by default via Nim's built-in
`stackTrace:on` compiler option, which defaults to `on` outside of `release`
and `danger` builds.

#### Stack Frames

Stack semantics are implemented by a single `TraceFrame` object stored on each
continuation and these serve to capture the progress of control-flow through
multiple levels of CPS calls just as they do so for normal stack-based code.

The `renderStackFrames()` and `writeStackFrames()` procedures return a sequence
of strings or write them to the standard message stream, respectively. You can
run these procedures without arguments in any continuation.

You can extend the `Stack` hook to alter its behavior.

Force-enable the stack frame support with `--define:cpsStackFrames=on`.

#### Trace Deque

The trace deque system stores the last _N_ hooks executed by the continuation,
where _N_ defaults to `4096` (see below). A single continuation has multiple
hooks executed during its lifecycle, so these traces can be very detailed.

The `renderTraceDeque()` and `writeTraceDeque()` procedures return a sequence
of strings or write them to the standard message stream, respectively. You can
run these procedures without arguments in any continuation.

You can extend the `Trace` hook to alter its behavior.

Force-enable the trace deque support with `--define:cpsTraceDeque=on` and
alter the size of the deque with e.g. `--define:traceDequeSize=100`.

### Using `cpsDebug`

Add `--define:cpsDebug=SomePass` where `SomePass` matches one of the CPS
transformation passes; this will output Nim codegen corresponding to the
rewrite phase. Interesting places to start include the following:

- `cpsTransform`
- `cpsResolver`
- `cpsJump`
- `cpsContinuationJump`
- `cpsManageException`
- `cpsTryFinally`
- etc.

Note that only one `--define:cpsDebug=...` can be enabled at a time - multiple defines will use only the final one.

### Using `trace`

Implement `trace` and it will be called at each continuation leg; [see the documentation for details](https://nim-works.github.io/cps/cps.html#trace.m%2Cstatic%5BHook%5D%2Ctyped%2Ctyped%2Cstring%2CLineInfo%2Ctyped).

Use `--define:cpsNoTrace` to disable the `trace` code generation.

## License
MIT
