

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
    C = ref object of RootObj
      fn: proc(c: C): C


  proc noop(c: C): C =
    return c

  proc sleep(c: C, seconds: int): C =
    return nil
    

  type 

    # Environments for all the split procs. These are analogous to
    # the stack frames in the original proc at the split locations
     
    Env_foo_0 = ref object of C
      n_gensymmed: int
      i_gensymmed: int

    Env_foo_1 = ref object of Env_foo_0

    Env_foo_2 = ref object of Env_foo_0
      j_gensymmed: int

    Env_foo_3 = ref object of Env_foo_2

    Env_foo_4 = ref object of Env_foo_2

    Env_foo_5 = ref object of Env_foo_2

    Env_foo_6 = ref object of Env_foo_0
      j_gensymmed: string
    
    Env_foo_7 = ref object of Env_foo_6


    # This is an object which is large enough to hold any of the above, and is
    # used for the initial allocation. I want max[sizeof(env_foo_0[]),
    # sizeof(env_foo_1[])), but that does not work

    Env_foo_storage = ref object of C
      storage: array[64, uint8]  # TODO calculate


  # Split proc forward declarations
  
  proc foo_0(c: C): C 
  proc foo_1(c: C): C 
  proc foo_2(c: C): C 
  proc foo_3(c: C): C 
  proc foo_4(c: C): C 
  proc foo_5(c: C): C 
  proc foo_6(c: C): C 
  proc foo_7(c: C): C 
  
  # Bootstrap proc to go from Nim to CPS land. Responsible for allocating the
  # continuation and transferring any arguments in

  proc foo(n: int): C =
    var env_foo = new Env_foo_storage
    echo sizeof(Env_foo_storage.storage)

    var c = cast[Env_foo_0](env_foo)
    c.n_gensymmed = n
    c.fn = foo_0
    return c

  # CPS functions
  
  template injectVar(T, id: untyped) =
    template id(): untyped = (cast[T](c).`id gensymmed`)


  proc foo_0(c: C): C =
    injectVar(Env_foo_0, n)
    injectVar(Env_foo_0, i)
    i = 0
    c.fn = foo_1
    return c
  
  proc foo_1(c: C): C =
    injectVar(Env_foo_1, n)
    injectVar(Env_foo_1, i)
    if i < n:
      c.fn = foo_2 
      return noop(c)
    c.fn = foo_6
    return c
  
  proc foo_2(c: C): C =
    injectVar(Env_foo_2, n)
    injectVar(Env_foo_2, i)
    injectVar(Env_foo_2, j)
    j = 0
    c.fn = foo_3
    return c

  proc foo_3(c: C): C =
    injectVar(Env_foo_3, n)
    injectVar(Env_foo_3, i)
    injectVar(Env_foo_3, j)
    if j < n:
      echo i, " ", j
      c.fn = foo_4
      return noop(c)
    c.fn = foo_5
    return c
  
  proc foo_4(c: C): C =
    injectVar(Env_foo_4, n)
    injectVar(Env_foo_4, i)
    injectVar(Env_foo_4, j)
    inc j
    c.fn = foo_3
    return c

  proc foo_5(c: C): C =
    injectVar(Env_foo_5, n)
    injectVar(Env_foo_5, i)
    injectVar(Env_foo_5, j)
    inc i
    c.fn = foo_1
    return c
  
  proc foo_6(c: C): C =
    injectVar(Env_foo_6, n)
    injectVar(Env_foo_6, i)
    injectVar(Env_foo_6, j)
    j = "done"
    c.fn = foo_7
    return noop(c)
  
  proc foo_7(c: C): C =
    injectVar(Env_foo_7, n)
    injectVar(Env_foo_7, i)
    injectVar(Env_foo_7, j)
    echo j
    c.fn = nil
    return c

  # Create initial continuation
 
  var c = foo(3)

  # Trampoline it

  while c.fn != nil:
    c = c.fn(c)



