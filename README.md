# Continuation-Passing Style

[![Test Matrix](https://github.com/nim-works/cps/workflows/CI/badge.svg)](https://github.com/nim-works/cps/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/nim-works/cps?style=flat)](https://github.com/nim-works/cps/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.9.3-informational?style=flat&logo=nim)
![Recommended Nim version](https://img.shields.io/badge/nim-1.9.3-informational?style=flat&logo=nim)
![Maximum supported Nim version](https://img.shields.io/badge/nim-2.1.1-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/nim-works/cps?style=flat)](#license)
[![Matrix](https://img.shields.io/matrix/cps:matrix.org?style=flat&logo=matrix)](https://matrix.to/#/#cps:matrix.org)
[![IRC](https://img.shields.io/badge/chat-%23cps%20on%20libera.chat-brightgreen?style=flat)](https://web.libera.chat/#cps)

This project provides a `cps` macro which you can add to a procedure
declaration to magically rewrite it as a continuation with suspend/resume
points injected automatically at suitable control-flow transitions.

This provides the benefits of CPS while hiding all the verbosity and spaghetti
code paths of hand-written continuation passing.

The macro performs only the control-flow rewrite; you can implement or import a
dispatcher to manage your continuations, and you can extend your continuation
type for nearly limitless composition.

As the macro merely rewrites your code as an equivalent continuation, you can
use CPS with C, C++, or even JavaScript backends, on any platform Nim supports.

<details>
  <summary>Click here for an awesome infographic explaining the technique.
  </summary>
  <img alt="The Nim CPS Transform" src="https://github.com/nim-works/cps/blob/master/docs/cps.svg"/>
</details>

## What Are These Continuations Good For?

The continuations produced by this macro are _tiny_ -- as little as 32 bytes
each -- and run at the speed of any native function call; they…

- compose efficient and idiomatic asynchronous code
- are over a thousand times lighter than threads
- are leak-free under Nim's modern memory management
- may be extended with custom inheritance-based types
- may be managed using custom dispatchers (executors)
- may be moved between threads to parallelize execution
- have no backend or runtime library requirements
- exploit no unsafe features of the language (`cast`, `ptr`, `addr`, `emit`)

## Is This Project Dead?

No, it just hasn't needed much development. We have a few small improvements we
want to make before a 1.0 major release, but the general consensus is that the
current implementation accomplishes our original goals quite well.

Major future development will revolve around implementing CPS directly in [the
Nimskull compiler](https://github.com/nim-works/nimskull).

## How Does It Work?

A substantial effort to demystify this style of programming, and what it may
enable, lives [in the docs/ subdirectory](https://github.com/nim-works/cps/tree/master/docs).

We also have [a tutorial to help new users on their way to get starting with
CPS](https://github.com/nim-works/cps/blob/master/tutorial/README.md).

For a description of the origins of our approach, see [the included
papers](https://github.com/nim-works/cps/blob/master/papers) and
https://github.com/nim-lang/RFCs/issues/295, where we write in more depth about
why the implementation exists, goals for future development, etc.

### Architecture

The implementation is comprised of two moving parts:

1. an *environment* is a bespoke type made to carry all locals in a procedure,
plus a pointer `fn` to a continuation leg, and a `mom` pointer to any parent
continuation:

```nim
type
  # instantiated continuations inherit from this type
  Continuation* = ref object of RootObj
    fn*: proc(c: Continuation): Continuation {.nimcall.}
    mom*: Continuation    # a reference to a parent
```

2. a *trampoline* is a procedure that looks like this:
```nim
var c = myContinuationInstance
while not c.isNil and not c.fn.isNil:
  # assign to the reference the result of running the stored
  # function pointer with the continuation itself as input
  c = c.fn(c)
```

We call the instantiated environment with its `fn` pointer a _continuation_,
and we call anything that invokes a continuation's `fn` pointer a _dispatcher_.

### Application

We use a procedure definition to define the continuation. The type we want to
base the continuation upon is supplied as the only argument to our `cps` pragma
macro.  You can simply use the `Continuation` type itself, if you prefer.

```nim
proc zoom() {.cps: Continuation.} =
  ## a very fast continuation
  discard
```

Calling the procedure (with arguments, if required) runs the continuation to
completion.

```nim
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
  ## Record a label by `name` which we can later goto().
  c.labels[name] = c.fn
  return c  # control resumes in the continuation

proc goto(c: Count; name: string): Count {.cpsMagic.} =
  ## Resume execution at the `name` label in the code.
  c.fn = c.labels[name]
  return c  # control resumes in the continuation
```

These pragma'd procedures act as continuation legs and we can use them in our
continuations without supplying the initial `Count` continuation argument.

Some people call this "colorless" syntax, since calls look the same whether
made inside or outside of a continuation.

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
your continuation with arguments, without actually invoking it.

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
assert later.finished, "counting incomplete!"
```

Continuations can themselves be called in order to retrieve their result.

```nim
echo "i counted ", later(), " trips through the goto"
```

Such a call will run the continuation on your behalf if it has not already run
to completion.

```nim
var later = whelp count(1_000_000)
echo "we counted ", later(), " trips through the goto"
```

### Ready For More?

[examples/coroutine.nim](https://github.com/nim-works/cps/blob/master/examples/coroutine.nim)
shows a simple implementation of coroutines communicating with each other on top
of CPS.  [Here's a walkthrough of how the example works.](https://github.com/nim-works/cps/docs/coroutines.md)

## Dispatchers

An example dispatcher with support for yields, I/O, and timers was included in
the past, but demonstrating dispatch conflated the roles of _continuation_ and
_dispatcher_, confusing newcomers.

For a robust dispatcher implementation targeting broad OS support and modern
async features, take a look at https://github.com/alaviss/nim-sys.

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
|[Actors](https://github.com/zevv/actors)|Zevv's experimental project to create a threaded, share-nothing actor based framework on top of CPS|
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
