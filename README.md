# Continuation Passing Style [![Build Status](https://travis-ci.org/disruptek/cps.svg?branch=master)](https://travis-ci.org/disruptek/cps)

This project provides a macro `cps` which you can apply to a procedure to
rewrite it to use continuations for control flow.

For a description of the origins of this concept, see the included papers
and https://github.com/zevv/nimcsp.


## Usage
```nim
import std/times

import cps
import cps/eventqueue

proc tock(name: var string; interval: Duration = DurationZero): Cont {.cps.} =
  var count: int = 0
  while true:
    inc count
    cps_sleep interval
    echo name, " ", count

trampoline tock("tick", initDuration(milliseconds = 300))
trampoline tock("tock", initDuration(milliseconds = 700))

run()
```

## Documentation
See [the documentation for the cps module](https://disruptek.github.io/cps/cps.html) as generated directly from the source.

## Tests
See the `tests/` subdirectory.

## License
MIT
