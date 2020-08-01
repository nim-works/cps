

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
     
    Env_foo = ref object of C
      n_gensymmed: int
      i_gensymmed: int
      j1_gensymmed: int
      j2_gensymmed: string


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
    var c = new Env_foo
    c.n_gensymmed = n
    c.fn = foo_0
    return c

  # CPS functions
  
  template injectVar(T, name, id: untyped) =
    template name(): untyped = (T(c).`id gensymmed`)


  proc foo_0(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    i = 0
    c.fn = foo_1
    return c
  
  proc foo_1(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    if i < n:
      c.fn = foo_2 
      return noop(c)
    c.fn = foo_6
    return c
  
  proc foo_2(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j1)
    j = 0
    c.fn = foo_3
    return c

  proc foo_3(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j1)
    if j < n:
      echo i, " ", j
      c.fn = foo_4
      return noop(c)
    c.fn = foo_5
    return c
  
  proc foo_4(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j1)
    inc j
    c.fn = foo_3
    return c

  proc foo_5(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j1)
    inc i
    c.fn = foo_1
    return c
  
  proc foo_6(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j2)
    j = "done"
    c.fn = foo_7
    return noop(c)
  
  proc foo_7(c: C): C =
    injectVar(Env_foo, n, n)
    injectVar(Env_foo, i, i)
    injectVar(Env_foo, j, j2)
    echo j
    c.fn = nil
    return c

  # Create initial continuation
 
  var c = foo(3)

  # Trampoline it

  while c.fn != nil:
    c = c.fn(c)



