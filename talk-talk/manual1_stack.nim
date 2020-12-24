

when false:

  # The original proc

  proc foo(n: int) =
    var i = 0
    while i < n:
      # sleep(1)
      var j = 0
      while j < n:
        echo i, " ", j
        # sleep(1)
        inc j
      inc i
    var j = "done"
    # sleep()
    echo j

  foo(3)


when true:

  # CPS'ed

  type

    # Environments for all the split procs. These are analogous to
    # the stack frames in the original proc at the split locations

    # We modify example 1 to store all the environment on the stack
    # this is an optimization when "not supportsCopyMem(T)".
    # This gives the compiler full visibility to optimize
    # the code away.

    Env_foo_0 = object
      n_gensymmed: int
      i_gensymmed: int

    Env_foo_1 = object
      n_gensymmed: int
      i_gensymmed: int

    Env_foo_2 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: int

    Env_foo_3 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: int

    Env_foo_4 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: int

    Env_foo_5 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: int

    HackEnvFoo6 = enum
      # No strings for the stack optimization
      Done = "done"

    Env_foo_6 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: HackEnvFoo6

    Env_foo_7 = object
      n_gensymmed: int
      i_gensymmed: int
      j_gensymmed: HackEnvFoo6

    # This is an object which is large enough to hold any of the above, and is
    # used for the initial allocation.

    Env_foo_storage {.union.} = object
      stor_Env_foo_0: Env_foo_0
      stor_Env_foo_1: Env_foo_1
      stor_Env_foo_2: Env_foo_2
      stor_Env_foo_3: Env_foo_3
      stor_Env_foo_4: Env_foo_4
      stor_Env_foo_5: Env_foo_5
      stor_Env_foo_6: Env_foo_6
      stor_Env_foo_7: Env_foo_7

    C = object
      fn: proc(c: var C) {.nimcall.}
      storage: Env_foo_storage

  proc noop(c: var C) =
    return

  proc sleep(c: var C, seconds: int) =
    c.fn = nil
    return

  import typetraits
  doAssert Env_foo_storage.supportsCopyMem()

  # Split proc forward declarations
  # should be "proc foo_0(c: sink C): C", but crash
  # to allow in-place modification and return
  proc foo_0(c: var C)
  proc foo_1(c: var C)
  proc foo_2(c: var C)
  proc foo_3(c: var C)
  proc foo_4(c: var C)
  proc foo_5(c: var C)
  proc foo_6(c: var C)
  proc foo_7(c: var C)

  # Bootstrap proc to go from Nim to CPS land. Responsible for allocating the
  # continuation and transferring any arguments in

  proc foo(n: int): C =
    var c = C()
    echo sizeof(c.storage)

    c.storage.stor_Env_foo_0.n_gensymmed = n
    c.fn = foo_0
    return c

  # CPS functions

  template injectVar(T, id: untyped) =
    template id(): untyped = (c.storage.`stor _ T`.`id gensymmed`)

  proc foo_0(c: var C) =
    injectVar(Env_foo_0, n)
    injectVar(Env_foo_0, i)
    i = 0
    c.fn = foo_1

  proc foo_1(c: var C) =
    injectVar(Env_foo_1, n)
    injectVar(Env_foo_1, i)
    if i < n:
      c.fn = foo_2
      noop(c)
      return
    c.fn = foo_6

  proc foo_2(c: var C) =
    injectVar(Env_foo_2, n)
    injectVar(Env_foo_2, i)
    injectVar(Env_foo_2, j)
    j = 0
    c.fn = foo_3

  proc foo_3(c: var C) =
    injectVar(Env_foo_3, n)
    injectVar(Env_foo_3, i)
    injectVar(Env_foo_3, j)
    if j < n:
      echo i, " ", j
      c.fn = foo_4
      noop(c)
      return
    c.fn = foo_5

  proc foo_4(c: var C) =
    injectVar(Env_foo_4, n)
    injectVar(Env_foo_4, i)
    injectVar(Env_foo_4, j)
    inc j
    c.fn = foo_3

  proc foo_5(c: var C) =
    injectVar(Env_foo_5, n)
    injectVar(Env_foo_5, i)
    injectVar(Env_foo_5, j)
    inc i
    c.fn = foo_1

  proc foo_6(c: var C) =
    injectVar(Env_foo_6, n)
    injectVar(Env_foo_6, i)
    injectVar(Env_foo_6, j)
    j = Done
    c.fn = foo_7
    noop(c)
    return

  proc foo_7(c: var C) =
    injectVar(Env_foo_7, n)
    injectVar(Env_foo_7, i)
    injectVar(Env_foo_7, j)
    echo j
    c.fn = nil

  # Create initial continuation

  var c = foo(3)

  # Trampoline it

  while c.fn != nil:
    c.fn(c)
