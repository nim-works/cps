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

Both coroutines are already "running" (`coroX.running == true`) but no work
has been performed yet. Let's add a `resume` proc as our dispatcher for the
coroutines.

```nim
proc resume(c: Coroutine): Coroutine {.discardable.} =
  var c = Continuation c
  while c.running:
    c = c.fn(c)
  result = Coroutine c
```

Now we can `resume()` each coroutine.

```nim
coro1.resume()
coro2.resume()
```

Actually, this is such a basic pattern for CPS code that the library provides a
[trampoline](https://nim-works.github.io/cps/cps/spec.html#trampoline%2CsinkT)
template for this, and it would be preferable to just use it here. Also, notice
how before invoking the function pointer we needed to convert a `Coroutine` to
its base type so we could set `c` to `fn(c)`, which returns a `Continuation`.

When invoked, `resume` launches the `filter` coroutine:

```nim
proc filter(dest: Coroutine, f: proc(x: int): int) {.cps: Coroutine.} =
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

