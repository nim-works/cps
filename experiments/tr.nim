import balls

suite "tr":
  ## define the basic types
  type
    C = ref object of RootObj
      fn: proc (c: C): C {.nimcall.}
      mom: C

    CT[T] = ref object of C
      data: ref T

    CR[T, R] = ref object of CT[T]
      result: R

  ## result procs
  proc res[T; R: void](c: CR[T, R]): R =
    ## an empty result should never be queried.
    raise ValueError.newException: "you're better than this"

  proc res[T; R: not void](c: CR[T, R]): R =
    ## fine, sure.
    c.result

  ## upscaler for result-less continuations
  converter toR[T](c: sink CT[T]): CR[T, void] =
    ## i know, i know
    result = CR[T, void](data: c.data, fn: c.fn, mom: c.mom)

  ## a voodoo or something
  proc bif(c: CT[int]; v: var int) =
    v = 10

  ## a child continuation or whatever
  proc bar[T, R](c: CR[T, R]; v: var T): CR[T, R] =
    result = c
    result.mom = c
    bif(c, c.data[])
    when R is string:    # added guard?  terrible.
      c.result = "it's " & $v

  ## define foo
  proc foo[T, R](c: CR[T, R]; results: static[bool]): CR[T, R] =
    # choose a child type dynamically
    var e =
      when R is void:
        CT[int](data: c.data)
      else:
        CR[int, R](data: c.data, result: default R)
    # run a child with/without result
    var d = bar(e, e.data[])
    when results:
      when R is void:
        raise Defect.newException "this is highly irregular"
      check d.res != ""
    else:
      when R isnot void:
        raise Defect.newException "this is highly irregular"
      expect ValueError:
        # d.res is void
        # d.res should raise
        d.res
    result = d

  type Cont[R] = CR[int, R]

  proc trampoline[T](c: T): T =
    var c = c.C
    while c != nil and c.fn != nil:
      c = c.fn(c)
    result = c.T

  proc newCont(x: ref int): Cont[void] =
    Cont[void](data: x)

  proc newCont(x: ref int; s: string): Cont[string] =
    Cont[string](data: x, result: s)

  var x = new int

  block:
    ## test runtime C
    var o = newCont x
    o = o.foo off
    check o.data[] == 10

  block:
    ## test runtime CR
    var o = newCont(x, "no result")
    o = o.foo on
    check o.data[] == 10
    check o.res == "it's 10"
