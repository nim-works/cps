
# CPS - also known as Continuation-Passing Style - for Nim.

People watching the Nim community have likely heard the term "CPS" over the last
few months. It seems there is some confusion about CPS. Specifically, what CPS
_is_, what it _is not_, what it can _do_ and what it can _not do_.

This short writeup attempts to answer the above questions and describes a few
technicalities involved with CPS, and how this could fit in the Nim ecosystem.

NB: This document uses the term **CPS** for the programming style using
continuations, and **Nim-CPS** when referring to the particular implementation
that is currently under development.

## TL;DR -- minute summary

*Nim-CPS* is a macro that transforms procedures.  This transformation
takes a single function definition as input via a pragma, and produces
the following outputs:

- a **continuation** type, typically an `object`, which contains a function
  pointer and any local variables from the original function

- a new **continuation leg** function for each list of statements between
  control-flow changes in the original procedure; these leg functions
  are modified to use the **continuation** fields in lieu of local
  variables, and at exit of each leg, the function pointer in the
  **continuation** is updated to point to the next leg in the control-flow
  path

- a new convenience function having the same signature as the original
  function, which instantiates a new instance of the **continuation** type

The stack is not needed after this transformation, which allows for some
interesting possibilities:

- Transformed functions _may_ now be interrupted, either cooperatively or
  pre-emptively, at each continuation leg.

- Continuation legs _may_ be called in arbitrary order, or not at all.

- Some or all of the continuation legs _may_ be run in different threads.

Note that _may_ was often used; this is where new libraries may provide
additional functionality:

- Interrupting and resuming function execution, while running another
  function, is the basis of `yield` (coroutines! iterators! generators!),
  or one can wait for I/O to become available (async I/O!).

- Varying the invocation order of continuation legs allows you to build
  custom control-flow primitives (goto! exceptions!)
  _without needing support from the language_.

- Moving continuation legs that are known to block (calculations,
  DNS lookups, etc.) to another thread while keeping the main program
  responsive (background processing!).

- [TODO talk about Nim's threading support]

*Nim-CPS is not*:

- an alternative implementation of `async`. As a matter of fact, Nim-CPS does
  not know anything about sockets, I/O, system calls, etc. That said, it is
  perfectly possible to implement something like `async` using CPS as a building
  block

- [TODO What more is Nim-CPS not]

# What Is This and Why Should I Care?

![CPS](/docs/cps.svg)

## Control-flow on the Stack

Computer programs written in *procedural* programming languages are typically
composed of functions calling other functions, making up a kind of tree-shaped
form.

As an example of this control-flow, consider the following Nim program:

```nim
proc func3() =
  echo "three"

proc func2() =
  echo "two"
  func3()

proc func1() =
  echo "one"

proc main() =
  echo "entry"
  func1()
  func2()
  echo "exit"

main()
```

This is a rendering of that program's control-flow to show the tree structure:

```
 ----[main..] - - [..main..] - - - - - - - - - - [..main]---> end
            |     ^        |                     ^
            v     |        v                     |
            [func1] - - -  [func2..] - - [..func2]
                                   |     ^
                                   v     |
                                   [func3]
```

As control-flow is introduced, the tree -- also known as a **stack** -- grows
and shrinks; it must do so in order to keep track of variables the programmer
defined and the functions the programmer called so that it can *resume*
control-flow when those functions complete.

## Stack Growth

When a function *calls* another function, it will store the *return address* on
the stack; when the *called* function is done, the program will continue the
*calling* function from that return address.

This way of programming is structured and easy to reason about, but the
consequence is that there is only "one way" your program can flow: it goes into
functions, and only leaves them when they return. When your function calls
another function, the stack grows, consuming a limited memory resource that is
often inaccessible and of little use to the new function context.

If your program decides that it doesn't need some memory it consumed on the
stack in a parent function context, there's no easy way to recover that
resource. You might imagine that this is particularly problematic with
recursive functions, and you have perhaps run into a *stack overflow* error in
these cases.

## Enter CPS

A different approach to control-flow is _**Continuation-Passing Style**_, or
**CPS**. CPS sounds a bit abstract if you're not knee-deep in computer science,
but the concept is actually very simple and as the name implies, it is merely a
*style* of programming control-flow which you can trivially adopt in almost any
program.

This document will demonstrate both how to write or modify programs using
CPS, and why you might want to do so in your own programs. We use Nim in the
examples, but CPS is applicable to almost any language with functions.

## A little history of CPS

First, CPS is nothing new; in the Lisp world people have been doing CPS since
the '70s, and in some languages (mainly those of the functional-programming
style) CPS is a common programming paradigm.

## Simplifying Control-flow with CPS

Using CPS, a completed function will not return to its caller using the return
address on the stack; instead it will directly call another function and
execution will continue from there.

Let's rewrite our example in CPS.

```nim
proc func3() =
  echo "three"

proc func2() =
  echo "two"
  func3()

proc func1() =
  echo "one"
  func2()

proc main() =
  echo "entry"
  func1()
  echo "exit"

main()
```

Now let's see what our tree looks like.

```
 ----[main]                             [main] ---> end
          |                             ^
          v                             |
          [func1]                 [func1]
                |                 ^
                v                 |
                [func2]     [func2]
                      |     ^
                      v     |
                      [func3]
```

## There's Just One Problem

This is silly! After leaving each of these functions, `main`, `func1`, `func2`,
and even `func3`, we never return to the caller's function context, yet we
still pay the penalty of having visited in the first place.

However, when you think about it, the program is already simpler to follow and
we've managed to constrain this problem of optimizing or eliminating stack
growth to a single scenario: a function call at the tail of another function.

## Tail-Call Optimization

Indeed, `gcc` and `clang` may be able to recognize that control-flow need
never revisit the calling function and thus unwind the stack before making
the function call at the tail. This is appropriately-termed **Tail-Call
Optimization** or **Tail-Call Elimination**.

Unfortunately, this optimization is not guaranteed by Nim, and as a result, we
need to address stack growth directly in Nim.

## Enter the Trampoline

What we really want is to run the first function, then run the next function,
then run the next function, and so on until there are no more functions to run.

To achieve this, we have each function in the chain tell the program where to
go next using a simple loop that executes each function until the chain is
terminated.

```nim
proc done(): auto =
  return done

proc func3(): auto =
  echo "three"
  return done

proc func2(): auto =
  echo "two"
  return func3

proc func1(): auto =
  echo "one"
  return func2

proc main() =
  echo "entry"
  var next = func1
  while next != done:
    next = next()
  echo "exit"

main()
```

Now our stack tree looks like this:

```
 ----[main]     [main]     [main]     [main] ---> end
          |     ^    |     ^    |     ^
          v     |    v     |    v     |
          [func1]    [func2]    [func3]
```

Perfect! We performed the same control-flow with no inefficient stack growth.

## You Call That 'Simple'?

Hang in there; it's going to get worse before it gets better.  😁

Let's modify our original program to add some more complexity.

```nim
import times

proc spin() =
  if getTime().toUnix mod 2 == 0:
    echo "come back later"
    spin()
  else:
    echo "okay, it's time"

proc main() =
  echo "entry"
  spin()
  echo "exit"

main()
```

Now the program prints `come back later` until the epoch time is odd, at which
point it prints `okay, it's time` and then completes, printing `exit`. This is
a very elegant control-flow design, but there's a bug in this program because
sometimes we spin so much that we exhaust the stack just by bookkeeping the
entry and exit of the `spin` procedure.

Let's see how this is solved in CPS.

```nim
import times

proc okay(): auto =
  echo "okay, it's time"
  okay

proc spin(): auto =
  if getTime().toUnix mod 2 == 0:
    echo "come back later"
    spin
  else:
    okay

proc main() =
  echo "entry"
  var next = spin
  while next != okay:
    next = next()
  echo "exit"

main()
```

Actually, that wasn't so bad. In fact, it's similar in size to the original
version, but the version using CPS doesn't suffer the stack-overflow bug.

Hopefully, you can start to see some advantages to this method of structuring
control-flow, but perhaps you've also noticed a new issue, which brings us
to...

## Local Variables

Remember when I said things would get worse before they get better? This is
that. 😈  But trust me, if you can get through this little section, you're
home free.

First, let's introduce some local state in our program.

```nim
import times, strutils

proc spin() =
  var now = getTime()
  if now.toUnix mod 2 == 0:
    echo "come back later"
    spin()
  else:
    echo "okay, it's $#" % [$now]

proc main() =
  echo "entry"
  spin()
  echo "exit"

main()
```

Our CPS version of the _previous_ program does the `okay...` echo in a separate
function, but that's going to be a problem here because that function doesn't
have access to the `now` variable. We cannot supply it as an argument to the
`okay` procedure because changing the signature of the procedure from that of
`spin` will cause it to be unusable in our trampoline.

We could change both signatures thusly:
```nim
proc spin(now: Time)
proc okay(now: Time)
```
But this will clearly be burdensome as the complexity of our program grows.

A more general solution is to share variables of the functions throughout the
CPS call chain; each procedure can receive the same object as input and mutate
that _environment_ before directing the trampoline to the next function in the
chain.

We begin our `OddTimer` object definition with the `next` function target for
the trampoline, and then we add the `now` variable to the environment.

```nim
import times, strutils

type OddTimer = ref object
  next: proc (c: OddTimer): OddTimer
  now: Time

proc okay(c: OddTimer): OddTimer =
  echo "okay, it's $#" % [$c.now]
  return nil

proc spin(c: OddTimer): OddTimer =
  c.now = getTime()
  if c.now.toUnix mod 2 == 0:
    echo "come back later"
  else:
    c.next = okay
  return c

proc main() =
  echo "entry"
  var c = OddTimer(next: spin)
  while c != nil:
    c = c.next(c)
  echo "exit"

main()
```

You probably noticed that we're using a `ref object` to hold these values; this
neatly moves the allocations with a longer lifetime from the stack to the heap.

## But You Said It Would Be Simple!

Use of CPS in procedural code is rare for the reasons you've seen in these
examples: it may be easier to reason about the functions in your program
individually, but harder to digest a greater perspective that shows the more
complex control-flow path.

What you _really_ want is to write your program so it is easiest to read, and
then pass it off to an accountant who will rewrite it for maximum efficiency,
using CPS.

This is where we segue into Nim's metaprogramming contribution...

# Nim-CPS

## A little history of Nim-CPS

Somewhere in the summer of '20, a few people started to think about what an
implementation of CPS could bring to Nim, and what this would look like. First
inspiration for in implementation was suggested by @Araq, who pointed to a
paper describing a rudimentary CPS implementation for the C language.

https://github.com/disruptek/cps/blob/master/papers/1011.4558.pdf

Using the algorithms in this paper as inspiration, a first implementation of
Nim-CPS was built using Nim's powerful metaprogramming. This allowed CPS to be
available as a library only, without needing support from the compiler or the
language.

[Nim-CPS repository on GitHub](https://www.github.com/disruptek/cps)

## What Nim-CPS can do for you, today

[The repository links to a series of examples @Zevv prepared to showcase CPS](https://github.com/disruptek/cps#examples)

[The repository also includes a walkthrough that shows the implementation of `GOTO`
from scratch using CPS.](https://github.com/disruptek/cps#application)

## The future of Nim-CPS

_Your contribution here!_
