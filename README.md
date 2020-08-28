# Continuation-Passing Style

[![Test Matrix](https://github.com/disruptek/cps/workflows/CI/badge.svg)](https://github.com/disruptek/cps/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/cps?style=flat)](https://github.com/disruptek/cps/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.3.5%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/cps?style=flat)](#license)

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
- require no `{.gcsafe.}` for global/local accesses
- are only 10-15% slower than Nim's native iterators
- are faster and lighter than async/await futures

## Work In Progress

The macro itself should be considered alpha quality. It is expected to
fail frequently, but it is in a state where we can begin to add tests and
refinement.

The included dispatcher is based upon
[selectors](https://nim-lang.org/docs/selectors.html), so you can see how the
features of that module will map quite easily to the following list.

Windows is not supported by the included dispatcher yet due to the lack of
native timer support in `selectors`, but [an ioselectors package that supports
Windows](https://github.com/xflywind/ioselectors) is in an advanced stage of
development.

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
  var count: int = 10
  while count > 0:
    dec count
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
  env0_18406164 = ref object of Cont
    name2_18406181: string
    ms3_18406189: int
    count4_18406302: int

proc loop_18406303(continuation: Cont): Cont
proc after_18406343(continuation: Cont): Cont
proc after_18406343(continuation: Cont): Cont =
  template name = env0_18406164(continuation).name2_18406181
  template ms = env0_18406164(continuation).ms3_18406189
  template count = env0_18406164(continuation).count4_18406302
  echo name, " ", count
  return Cont:
    continuation.fn = loop_18406303
    continuation

proc loop_18406303(continuation: Cont): Cont =
  template name = env0_18406164(continuation).name2_18406181
  template ms = env0_18406164(continuation).ms3_18406189
  template count = env0_18406164(continuation).count4_18406302
  if count > 0:
    dec count
    return sleep(Cont:
      continuation.fn = after_18406343
      continuation, ms)
  return nil

proc tock(name: string; ms: int): Cont =
  result = env0_18406164(fn: nil, ms3_18406189: ms, name2_18406181: name)
  template continuation = result
  template name = env0_18406164(continuation).name2_18406181
  template ms = env0_18406164(continuation).ms3_18406189
  template count = env0_18406164(continuation).count4_18406302
  count = 10
  return Cont:
    continuation.fn = loop_18406303
    continuation
```
...and when built with `--define:cpsDebug`, outputs something like...

![tick-tock demonstration](docs/demo.svg "tick-tock demonstration")

## Hacking

- use `--define:cpsDebug` to get extra debugging output
- use `--define:cpsTrace` to get continuation tracing from the trampoline
- use `--define:cpsCast` to `cast` continuations (versus type conversion)
- use `--define:cpsTree` to dump AST via `treeRepr` in `cpsDebug` mode
- use `--define:cpsExcept` catch exceptions and stash them in the continuation
- use `--define:cpsMutant` toggle mutating continuations (default: _off_)

## Documentation

See [the documentation for the cps module](https://disruptek.github.io/cps/cps.html) as generated directly from the source.
You can also jump to [the documentation for the included dispatcher](https://disruptek.github.io/cps/cps/eventqueue.html).

## Tests

The tests provide the best examples of usage and are a great starting point for
your experiments.

Here are some tests that Zevv prepared:

![zevv tests](docs/tzevv.svg "zevv tests")

Here are the simpler tests of AST rewrites:

![taste tests](docs/taste.svg "taste tests")

## License
MIT
