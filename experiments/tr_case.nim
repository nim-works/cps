import balls

suite "tr":
  ## define the basic types
  type
    Kind = enum Basic, Result, Data
    C[R, T] = ref object of RootObj
      fn: proc (c: C[R, T]): C[R, T] {.nimcall.}
      mom: C[R, T]
      case kind: Kind
      of Basic:
        discard
      of Result:
        r: R
      of Data:
        tr: R
        data: ref T

  proc newC[R, T](): C[R, T] =
    when R is void and T is void:
      C[R, T](kind: Basic)
    elif T is void:
      C[R, T](kind: Result)
    else:
      C[R, T](kind: Data)

  proc newC[R, T](x: ref T): C[R, T] =
    C[R, T](data: x, kind: Data)

  ## result proc
  proc res[R: void; T](c: C[R, T]) =
    raise ValueError.newException: "just no"

  proc res[R, T](c: C[R, T]): R =
    ## fine, sure.
    case c.kind
    of Result:
      result = c.r
    of Data:
      result = c.tr
    else:
      raise ValueError.newException: "you're better than this"

  ## a voodoo or something
  proc bif(c: C[auto, int]; v: var int) =
    v = 10

  ## a child continuation or whatever
  proc bar[R, T](c: C[R, T]; v: var T): C[R, T] =
    result = c
    result.mom = c
    bif(c, c.data[])
    when R is string:    # added guard?  terrible.
      c.tr = "it's " & $v

  ## define foo
  proc foo[R, T](c: C[R, T]; results: static[bool]): C[R, T] =
    # choose a child type dynamically
    var e =
      when results:
        newC[R, T]()
      else:
        newC[void, T]()
    e.data = c.data
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

  proc trampoline[T](c: T): T =
    var c = c.C
    while c != nil and c.fn != nil:
      c = c.fn(c)
    result = c.T

  type Cont[R] = C[R, int]

  proc newCont(x: ref int): Cont[void] =
    result = newC[void, int](x)

  proc newCont(x: ref int; s: string): Cont[string] =
    result = newC[string, int](x)
    result.tr = s

  var x = new int

  block:
    ## test runtime CT
    var o = newCont x
    o = o.foo off
    check o.data[] == 10

  block:
    ## test runtime CR
    var o = newCont(x, "no result")
    o = o.foo on
    check o.data[] == 10
    check o.res == "it's 10"
