# Continuation-Passing Style

[![Test Matrix](https://github.com/disruptek/cps/workflows/CI/badge.svg)](https://github.com/disruptek/cps/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/cps?style=flat)](https://github.com/disruptek/cps/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.5.1%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/cps?style=flat)](#license)

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

## How Do I Use It?

The `cps` macro will use whatever return value you specify to determine the
type of your continuations. This type is typically a `ref object of RootObj` so
you can use it with inheritance.

```nim
# all cps programs need the cps macro to perform the transformation
import cps

# but each usage of the .cps. macro can have its own dispatcher
# implementation and continuation type, allowing you to implement
# custom forms of async or use an existing library implementation
#
# here we use a reference dispatcher (see below)
from eventqueue import sleep, run, spawn, trampoline, Cont

# this procedure is written in a simple synchronous style, but when
# the .cps. is applied during compilation, it is rewritten to use
# the Cont type in a series of asynchronous continuations

proc tock(name: string; ms: int) {.cps: Cont.} =
  ## echo the `name` at `ms` millisecond intervals, ten times

  # a recent change to cps allows us to use type inference
  var count = 10

  # `for` loops are not supported yet
  while count > 0:

    dec count

    # the dispatcher supplied this primitive which receives the
    # continuation and returns control to the caller immediately
    sleep ms

    # subsequent control-flow is continues from the dispatcher
    # when it elects to resume the continuation
    echo name, " ", count

# NOTE: all the subsequent code is supplied by the chosen dispatcher

# the trampoline repeatedly invokes continuations until they
# complete or are queued in the dispatcher; this call does not block
trampoline tock("tick", ms = 300)

# you can also send a continuation directly to the dispatcher;
# this call does not block
spawn tock("tock", ms = 700)

# run the dispatcher to invoke its pending continuations from the queue;
# this is a blocking call that completes when the queue is empty
run()
```

[The source to the tick-tock test.](https://github.com/disruptek/eventqueue/blob/master/tests/tock.nim)

## Notes on the Example Dispatcher

An example dispatcher was included in the past, but demonstrating dispatch
conflated the purpose of the `cps` macro and made misconceptions about the role
of continuation-versus-dispatcher common. The reference dispatcher can now be
found at https://github.com/disruptek/eventqueue and you can also jump directly
to [the documentation](https://disruptek.github.io/eventqueue/eventqueue.html).

## Hacking

- use `--define:cpsDebug` to get extra debugging output
- use `--define:cpsTree` to dump AST via `treeRepr` in `cpsDebug` mode

## Documentation

See [the documentation for the cps module](https://disruptek.github.io/cps/cps.html) as generated directly from the source.

## Tests

The tests provide the best examples of usage and are a great starting point for
your experiments.

[Here are some tests that Zevv prepared:](https://github.com/disruptek/cps/blob/master/tests/tzevv.nim)

![zevv tests](docs/tzevv.svg "zevv tests")

[Here are more contrived tests of AST rewrites:](https://github.com/disruptek/cps/blob/master/tests/taste.nim)

![taste tests](docs/taste.svg "taste tests")

## License
MIT
