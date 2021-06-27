# Continuation-Passing Style

[![Test Matrix](https://github.com/disruptek/cps/workflows/CI/badge.svg)](https://github.com/disruptek/cps/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/cps?style=flat)](https://github.com/disruptek/cps/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.5.1%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/cps?style=flat)](#license)
[![Matrix](https://img.shields.io/matrix/cps:matrix.org?style=flat&logo=matrix)](https://matrix.to/#/#cps:matrix.org)
[![IRC](https://img.shields.io/badge/chat-%23cps%20on%20libera.chat-brightgreen?style=flat)](https://web.libera.chat/#cps)

This project provides a `cps` pragma which you can add to a procedure to
automatically rewrite it to use continuations for control-flow. This provides
the benefits of CPS while abstracting away the verbosity of continuations.

The `cps` pragma performs only the control-flow rewrite; you implement or
import a dispatcher to define both the type and behavior of your continuations,
so there is virtually no API and no limitations on composition.

A substantial effort to demystify this style of programming, and what it may
enable, lives at https://github.com/zevv/cpsdoc.

For a description of the origins of our approach, see the included papers and
https://github.com/nim-lang/RFCs/issues/295, where we write in more depth about
why the implementation exists, goals for future development, etc.

## What Are These Continuations Good For?

The continuations produced by this macro...

- compose efficient and idiomatic asynchronous code
- are over a thousand times lighter than threads
- are leak-free under Nim's ARC/ORC memory management
- may be based upon your own custom `ref object` type
- may be dispatched using your own custom dispatcher
- may be moved between threads to parallelize execution
- are faster and lighter than async/await futures
- are 5-15% faster than native closure iterators
- exploit no unsafe features of the language (`cast`, `ptr`, `addr`, `emit`)

## This is Work In Progress!

The macro itself should be considered beta quality.  Corner-cases are being
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
a bit later, after your nap.  In that case, you can use `whelp` to instantiate
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

### TBD

We'll talk about voodoo here and walk through the coroutine demo, since it pulls together prior concepts and adds voodoo and multiple continuations.

## Dispatchers

### Notes on the Example Dispatcher

An example dispatcher was included in the past, but demonstrating dispatch
conflated the purpose of the `cps` macro and made misconceptions about the role
of continuation-versus-dispatcher common. The reference dispatcher can now be
found at https://github.com/disruptek/eventqueue and you can also jump directly
to [the documentation](https://disruptek.github.io/eventqueue/eventqueue.html).

### Other Available Dispatchers

- https://github.com/alaviss/nim-sys -- next generation OS services
- https://github.com/disruptek/passenger -- create a graph of continuation runtime
- https://github.com/disruptek/supervisors -- simple dispatch patterns for composition

## Documentation

See [the documentation for the cps module](https://disruptek.github.io/cps/cps.html) as generated directly from the source.

## Examples

A small collection of examples provides good demonstration of multiple patterns
of CPS composition. Each example runs independently, with no other requirements,
yet demonstrates different exploits of `cps`.

| Example | Description |
|     --: | :--         |
|[Channels](https://github.com/disruptek/cps/blob/master/examples/channels.nim)|A channel connects sender and receiver continuations|
|[Goto](https://github.com/disruptek/cps/blob/master/examples/goto.nim)|Implementation of `label` and `goto` statements using CPS|
|[Iterator](https://github.com/disruptek/cps/blob/master/examples/iterator.nim)|A simple demonstration of a CPS-based iterator|
|[Coroutines](https://github.com/disruptek/cps/blob/master/examples/coroutines.nim)|A pair of continuations communicate as coroutines|
|[Lazy](https://github.com/disruptek/cps/blob/master/examples/lazy.nim)|Lazy streams are composed by continuations in a functional style|
|[TryCatch](https://github.com/disruptek/cps/blob/master/examples/trycatch.nim)|Exception handling is reimplemented using only CPS|
|[CpsCps](https://github.com/disruptek/cps/blob/master/examples/cpscps.nim)|Continuations can efficiently call other continuations|
|[Work](https://github.com/disruptek/cps/blob/master/examples/work.nim)|Implementation of a simple continuation scheduler|
|[LuaCoroutines](https://github.com/disruptek/cps/blob/master/examples/lua_coroutines.nim)|Coroutines implemented in the style of Lua|
|[ThreadPool](https://github.com/disruptek/cps/blob/master/examples/threadpool.nim)|1,000,000 continuations run across all your CPU cores|

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

- `var` parameters to procedures with the `cps` pragma are not supported.

- Generic continuations such as the following won't work without changes to
the compiler.

```nim
type
  MyContinuation[T] = ref object of Continuation
    something: T
```

### Performance

If you are not running with `define:danger` and `gc:arc` and `panics:on` then
performance considerations really aren't your primary consideration, right?

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

### Using `trace`

Implement `trace` and it will be called at each continuation leg; [see the documentation for details](https://disruptek.github.io/cps/cps.html#trace.t%2CContinuation%2Cstring%2CLineInfo).

## License
MIT
