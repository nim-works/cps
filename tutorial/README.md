

# Introduction

What you are reading is a little tutorial to get started with Nim CPS. This
document will introduce the essential parts of the CPS API to get you started
writing your own CPS programs.

The latest greatest CPS can be found at https://github.com/nim-works/cps/

If you are not familiar with the concept of CPS I recomment first reading up
a bit on the background: https://github.com/zevv/cpsdoc

## Table of contents

- [Introduction](#introduction)
  - [Table of contents](#table-of-contents)
  - [Baby steps: my first cps program](#baby-steps-my-first-cps-program)
  - [The CPS transform macro](#the-cps-transform-macro)
  - [A more elaborate example: cooperative scheduling](#a-more-elaborate-example-cooperative-scheduling)
  - [Growing your own continuations](#growing-your-own-continuations)
  - [Going deeper: calling CPS from CPS](#going-deeper-calling-cps-from-cps)
  - [Todo](#todo)

## Baby steps: my first cps program

The complete code for this chapter can be found at
https://github.com/nim-works/cps/blob/master/tutorial/cpstut1.nim

CPS is available as a regular nim library that you must import before CPS is
available in your program. The module offers a number of macros and templates,
for details refer to the module documentation at
https://nim-works.github.io/cps/cps.html

So we start with the import:

```nim
import cps
```

At the heart of CPS lies the `Continuation` type. In our implementation, this
is just a regular Nim object that is inheritable. This is what the type looks like:

```nim
Continuation = ref object of RootObj
   fn*: proc (c: Continuation): Continuation {.nimcall.}
   ...
```

The object has a few more fields which are used for the CPS implementation
internally, but one of the fields is very important for the users of CPS,
which is `fn`, the function pointer that makes CPS continuations
tick. We'll get back to its use later.

To start with CPS, you would typically define your own object, inherited from
the CPS Continuation type, like so

```nim
type
  MyCont = ref object of Continuation
```

At a later time we will add our own fields to the derived Continuation
objects, but for now we'll start out simple.


## The CPS transform macro

Next to the continuation type, the `.cps.` macro is the other essential part of
writing CPS programs, this is the macro that will be applied to any Nim
function we want to transform to CPS style. This macro does two jobs:

- It splits the Nim function into a number of separate functions that we
  can independently; each of these functions is what we call a "Leg".
- It creates a new object type that is derived from our `MyCont`, on which it
  will store all function arguments and local variables. This type is opaque
  to us and is only used by CPS internally.

The `.cps.` macro is a bit special, as it is typed: when calling the macro, the
user needs to specify the type on which the macro should operate, and this
type needs to be a derivative of the Continuation root object. This is what
the notation looks like:

```nim
proc hello() {.cps:MyCont.} =
  echo "Hello, world!"
```

Congratulations! We have now written our very first CPS program. Nim will now
know all that is needed to do the transformation of our procedure at compile
time so it will run our code in CPS style.

The next thing to do would be to run our CPS transformed function. This
involves a few steps we'll go through below.

We start with instantiating the continuation: this means CPS will allocate a
continuation object and prepare it so that it will point to the first leg of
our function. Creating this instance is done with the `whelp` macro, and
looks like this:

```nim
var c: Continuation = whelp hello()
```

For technical reasons, the whelp macro returns a derived type, which we need to
convert back to the `Continuation` type to be able to work with it.

Our continuation is now ready to be run; in fact, it has already started!
There is a little function to check the state of a continuation, and the one
above is now in the state called `Running`. You can inspect the current state
of a continuation like this:

```nim
doAssert c.state == Running
```

or, shorter:

```nim
doAssert c.running()
```

Now, to run the rest of our function (_continue_ it!), we need to do a little
function call dance, which in the world of CPS is called `trampolining`: we
call the `fn()` proc that is in the object, and pass the object itself to it.
The result of this function call is again a continuation. Calling the `fn()`
function once will run exactly one leg of our function:

```nim
c = c.fn(c)
```

The effect of the call above is printing "Hello, world!" to your terminal!

Our original function was not very exciting and did not do much; after printing
the text, it is done and finished - all the work could be done in one single leg.
This means the continuation is now done and complete:

```nim
doAssert c.state == Finished
```

or again, the shorthand

```nim
doAssert c.finished()
```

In real life, your CPS functions will have more then one leg. You would
typically want to call the `fn()` proc repeatedly until the continunation
is no longer running. This is a typical CPS idiom, and looks like this:

```nim
while c.running:
  c = c.fn(c)
```

Running the continuation legs sequentially is called "trampolining", look at
the diagram below to see why:

```
whelp >--.     ,---.     ,---.     ,---.     ,---.     ,--> finished
          \   /     v   /     v   /     v   /     v   /
         +-----+   +-----+   +-----+   +-----+   +-----+
         | leg |   | leg |   | leg |   | leg |   | leg |
         +-----+   +-----+   +-----+   +-----+   +-----+
```

Because trampolining is a very common operation, CPS offers a template called
`trampoline()` that does exactly this.

## A more elaborate example: cooperative scheduling

The complete code for this chapter can be found at
https://github.com/nim-works/cps/blob/master/tutorial/cpstut2.nim

The above function was pretty simple and minimal, as it was transformed to
only one single leg; it served the purpose of showing how to instantiate and
run a CPS function.

Let's go a bit deeper now. The essence of CPS is that our functions can be
split into legs that can be run at leisure; one typical example of this would
be cooperative scheduling, where we can run multiple CPS functions
concurrently.

For a simple example, let's write a little function with a loop - just a
normal regular Nim function, which we will change later to run concurrent
using CPS:

```nim
proc animal(name: string) =
  var i = 0
  while i < 4:
    inc i
    echo name, " ", i
```

So let's call the function to see if it works:

```nim
animal("donkey")
```

The output of this function call looks like this:

```
donkey 1
donkey 2
donkey 3
donkey 4
```

Now let's see how we can leverage CPS to run multiple instances of this
function concurrently!

Let's start with a place to store the continuations that we want to run. A
deque is a good fit for this, this is a first-in-first-out queue where we can
add new continuations on one side, and take them off to run them from the
other:

```nim
import deques

var work: Deque[Continuation]
```

Now we need some code to run with this work queue. It will have a pretty simple
job: it takes one continuation of the queue and trampolines it until it is no
longer running, and repeat until there is no more work on the queue. Note that we
also use the `trampoline()` template now, instead of calling `c.fn()` ourselves:

```nim
proc runWork() =
  while work.len > 0:
    discard trampoline work.popFirst()
```

Now we will introduce the last important part for building CPS programs,
which is a special kind of function with the silly name `cpsMagic`. Hold on
to your seat, because this is possibly the most confusing part of CPS:

Let's first describe what a `cpsMagic` function looks like. It:

- is annotated with the `{.cpsMagic.}` pragma;
- takes a continuation type as its first arguments;
- has the same continuation type as its return value;
- can only be called from within a CPS function.

When calling the function, you do not need to provide the first argument, as
this will be injected by the CPS transformation at the call site. Also you do
not need to consume its return value, as that is handled by CPS internally.

Now this is where the magic comes in. `cpsMagic` functions can be used to alter
the program flow of a CPS function: it has access to the current continuation
that is passed as its first argument, and it can return a continuation which
will be used as the next leg in the trampoline.

That sounds complicated, so let's just write our first `cpsMagic` proc:

```nim
proc schedule(c: MyCont): MyCont {.cpsMagic.} =
  work.addLast c
  return nil
```

Let's see what happens when we call this:

- The current continuation of the CPS function will be passed as the first
  argument `c`.
- The continuation `c` is added to `work` - the dequeue of continuations.
- It returns `nil` which means "no continuation". This will cause the
  trampoline that is running the continuation to terminate.

Summarizing the above, the `schedule()` function will move the current
continuation to the work queue, and stop the trampoline. The trampoline
now has lost track of the continuation, as it is stored on the work queue
instead so we can pick it up and suspend it later.

Remember that when calling a `cpsMagic` function from within CPS, we do not
need to provide the first argument, nor handle the return type. To call
the above function, simply do:

```nim
schedule()
```

It is now time to put the above pieces together. Let's take the example
function we wrote before, and make the required changes:

- Add the `{.cps:MyCont.}` pragma to make it into a CPS function.
- Call `schedule()` in the loop to suspend execution of the code by
  the trampoline.

This is what it will look like now:

```nim
proc animal(name: string) {.cps:MyCont.}=
  var i = 0
  while i < 4:
    inc i
    echo name, " ", i
    schedule()
  echo ""
```

And that's it! Now we can instantiate the function into a continuation with
the `whelp` macro. Let's do this twice to create two instances, and add the
resulting continuations to the work queue:

```nim
work.addLast whelp animal("donkey")
work.addLast whelp animal("tiger")
```

Now let's run this beast:

```nim
runWork()
```

And here is the output of our run:

```
donkey 1
tiger 1
donkey 2
tiger 2
donkey 3
tiger 3
donkey 4
tiger 4
```

What we have implemented here is very close to a concept known as "coroutines":
this allows for functions that can suspend their execution (often called `yield`)
at a point where we called `schedule` to be resumed later. In contrast with
normal threads, coroutines are light as a feather: they typically cost only a
handful of bytes per coroutine, and do not require OS context switching.


```

  : = schedule()

            :       :          :       :          :       :
 --[donkey..]       [..donkey..]       [..donkey..]       [..donkey..
            :       :          :       :          :       :
            [tiger..]          [tiger..]          [tiger..]
            :       :          :       :          :       :
```

## Growing your own continuations

The complete code for this chapter can be found at
https://github.com/nim-works/cps/blob/master/tutorial/cpstut3.nim

The example from the chapter above works just fine, but has one ugly drawback:
the work queue is a global variable that is accessed from the `cpsMagic` proc. A
nice way to solve this is to make the work queue a reference object, which is
added to the continuation type itself: this way, every CPS function can access
the work queue from the `cpsMagic` functions, without having to pass it around.

For this we need to make some changes to the code: first we define a reference
type holding the work queue, and add a value of this type to our own
continuation:

```nim
type
  Work = ref object
    queue: Deque[Continuation]

  MyCont = ref object of Continuation
    work: Work
```

When we now whelp new continuations, we need to make sure that the `work`
pointer on the continuation points to a valid work queue. A little convenience
function can be added for this, which we will use later to add our freshly
whelped continuations to the work queue:

```nim
proc push(work: Work, c: MyCont) =
  work.queue.addLast c
  c.work = work
```

The schedule function is now changed not to add the continuation to
the global work queue, but to the queue that is stored on the continuation
instead:

```nim
proc schedule(c: MyCont): MyCont {.cpsMagic.} =
  c.work.queue.addLast c
  return nil
```

The trampolining of the work queue was done in the main code before, let's move
this to a proc instead:

```nim
proc work(work: Work) =
  while work.queue.len > 0:
    var c = work.queue.popFirst()
    while c.running:
      c = c.fn(c)
```

And this completes all the pieces of the puzzle: we can now create one
instance of a work queue, add the fresh continuations to them and run the
work queue like this:

```nim
var mywork = Work()
mywork.push whelp animal("donkey")
mywork.push whelp animal("tiger")
mywork.work()
```

## Going deeper: calling CPS from CPS

The complete code for this chapter can be found at
https://github.com/nim-works/cps/blob/master/tutorial/cpstut4.nim

In the above examples we have seen how to write and run your own CPS functions
using `whelp` and a trampoline. By moving the continuation objects around you
are in full control of the control flow, running multiple functions
concurrently.

Real programs are usually not made of one single function; instead, programs
are composed of functions calling other functions, calling other functions, all
the way down. When running regular function code, this is what your control flow
might look like:

```
 ----[main..] - - [..main..] - - - - - - - - - - [..main]---> end
            |     ^        |                     ^
            v     |        v                     |
            [func1]        [func2..] - - [..func2]
                                   |     ^
                                   v     |
                                   [func3]
```

Conveniently, CPS offers this functional flow when you call a CPS function from
another CPS function: the current continuation (the "parent") will be temporary
suspended, and its continuation will be replaced by a newly whelped one for the
new called function (the "child"). When the child finishes during trampolining,
CPS will automatically restore the continuation for the parent, which can then
be resumed.

Let's extend our earlier example. We will make a CPS-to-CPS function call from
the `animal` proc, and call schedule from there:

```nim
proc sayHi(name: string, i: int) {.cps:MyCont.} =
  echo "Hi ", name, " ", i
  schedule()

proc animal(name: string) {.cps:MyCont.}=
  var i = 0
  while i < 4:
    inc i
    sayHi(name, i)
  echo ""
```

Note that we can just call the `sayHi()` proc as if it were a normal Nim proc:
there is no need use `whelp` there, CPS will automatically do the right thing
for you.

However, there is one more thing we need to add to the program to make this
work:

Remember that when we whelped the initial continuation for the `animal()`
function, we added this to the work queue with the `push()` function. `push()`
does not only add the continuation to the work queue, but also sets the `work`
field of the continuation to point to the work queue itself, so it can later be
used by `schedule()` to suspend execution.

However, when we call `sayHi()` from `animal()`, CPS will implicitly whelp the
new continuation for the `sayHi()` function for us, and this continuation will
not have its `work` field initialized.

Enter CPS "hooks": hooks are procs or templates that get mixed in by CPS which
you can choose to implement to tailor CPS' behaviour to your needs. We will use
the `pass()` hook, which gets called when CPS performs a call or return from a
CPS function, and looks like this:

```
proc pass(cFrom, cTo: Continuation): Continuation
```

It gets passed two continuations as arguments. The first is the current
continuation that is now active, the second is the next continuation that will
be ran:

- on a *call*, `cFrom` will be the parent continuation, and `cTo` the child;
- on a *return*, `cFrom` points to the child, and `cTo` to the parent.

We can use the `pass()` hook to set the `work` field of the new child continuation
that gets whelped by CPS, effectively inheriting it from the parent function:

```nim
proc pass(cFrom, cTo: MyCont): MyCont =
  cTo.work = cFrom.work
  return cTo
```

With the above changes, this is what the control flow will now look like this:

```
 : = schedule()

                   :                         :                              :
 --[donkey..]      :                         :         [..donkey..]         :
            |      :                         :         |          |         :
            [saHi..] - - - - - - - - - - - - [..sayHi..]          [..sayHi..] - - - - - - - -
                   :                         :                              :
                   :                         :                              :
                   :                         :                              :
                   :       [..tiger..]       :                              :       [..tiger..
                   :       |         |       :                              :       |
 - - - - - - - - - [sayHi..]         [sayHi..] - - - - - - - - - - - - - -  [..sayHi]
                   :                         :                              :
```


## Todo

- `{.cpsVoodo.}`


