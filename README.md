# Continuation-Passing Style [![Build Status](https://travis-ci.org/disruptek/cps.svg?branch=master)](https://travis-ci.org/disruptek/cps)

This project provides a macro `cps` which you can apply to a procedure to
rewrite it to use continuations for control flow.

All runtime functionality is implemented in a dispatcher which you can replace
to completely change the type and behavior of your continuations.

For a description of the origins of this concept, see the included papers
and https://github.com/zevv/nimcsp.

## Why

These continuations...

- compose efficient and idiomatic asynchronous code
- are over a thousand times lighter than threads
- are leak-free under Nim's ARC memory management
- may be based upon your own custom `ref object`
- may be dispatched using your own custom dispatcher
- may be moved between threads to parallelize execution
- are faster and lighter than async/await futures

## Work In Progress

The macro itself should be considered alpha quality. It is expected to
fail frequently, but it is in a state where we can begin to add tests and
refinement.

The included dispatcher is based upon
[selectors](https://nim-lang.org/docs/selectors.html), so you can see how the
features of that module will map quite easily to the following list.

Key primitives and their implementation status, in order of priority:

- [x] sleep
- [x] yield
- [x] discard
- [x] signal
- [x] wait
- [x] I/O
- [x] fork
- [ ] thread

Windows is not supported by the included dispatcher yet due to the lack of
native timer support in `selectors`, but this is a solved problem; feel free to
submit a pull request!

## Usage

The provided `selectors`-based event queue is imported as `cps/eventqueue`. The
`cps` macro will use whatever return value you specify to determine the type of
your continuations.

```nim
import std/times       # Duration

import cps             # .cps. macro
import cps/eventqueue  # sleep(), trampoline, run(), Cont

# a procedure that starts off synchronous and becomes asynchronous
proc tock(name: string; interval: Duration) {.cps: Cont.} =
  var count: int = 0
  while true:
    inc count
    # this primitive sends the continuation to the dispatcher
    yield sleep(interval)
    # this is executed from the dispatcher
    echo name, " ", count

# the trampoline repeatedly invokes continuations...
trampoline tock("tick", initDuration(milliseconds = 300))
# ...until they complete or are queued in the dispatcher
trampoline tock("tock", initDuration(milliseconds = 700))

# run the dispatcher to invoke pending continuations
run()
```
...is rewritten during compilation to something like...

```nim
type
  env_16451076 = ref object of Cont
    name: string
    interval: Duration

  env_16451209 = ref object of env_16451076
    count: int

proc after_16451243(locals_16451244: Cont): Cont =
  let interval: Duration = env_16451209(locals_16451244).interval
  let name: string = env_16451209(locals_16451244).name
  var count: int = env_16451209(locals_16451244).count
  echo name, " ", count
  return env_16451209(fn: loop_16451121, count: count, name: name, interval: interval).Cont

proc loop_16451121(locals_16451228: Cont): Cont =
  let interval: Duration = env_16451209(locals_16451228).interval
  let name: string = env_16451209(locals_16451228).name
  var count: int = env_16451209(locals_16451228).count
  if true:
    inc count
    return sleep env_16451209(fn: after_16451243, count: count, name: name,
                                interval: interval).Cont, interval

proc tock(name: string; interval: Duration): Cont =
  var count: int = 0
  return env_16451209(fn: loop_16451121, count: count, name: name, interval: interval).Cont
```

## Hacking

- use `--define:cpsDebug` to get extra debugging output
- use `--define:cpsTrace` to get continuation tracing from the trampoline
- use `--define:cpsCast` to `cast` continuations (versus type conversion)
- use `--define:cpsTree` to dump AST via `treeRepr` in `cpsDebug` mode
- use `--define:cpsExcept` catch exceptions and stash them in the continuation

## Documentation
See [the documentation for the cps module](https://disruptek.github.io/cps/cps.html) as generated directly from the source.
You can also jump to [the documentation for the included dispatcher](https://disruptek.github.io/cps/cps/eventqueue.html).

## Tests

The tests provide the best examples of usage and are a great starting point for
your experiments.

## License
MIT
