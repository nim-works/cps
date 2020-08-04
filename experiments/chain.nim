import std/strutils
import std/macros
import std/os
import std/deques
import std/options
import std/algorithm

{.experimental: "dotOperators".}
{.experimental: "callOperator".}

type
  ## just a generic continuation concept
  C[T] = concept c
    c.fn is P
    c.data is T
  Pv[C] = proc (c: var C): C {.nimcall.}
  Pl[C] = proc (c: C): C {.nimcall.}
  P[C] = Pv[C] or Pl[C]

proc next(c: C): C =
  echo "next"
  if c.fn.isNil:
    result = c
  else:
    result = c.fn(c)

proc `()`[T](c: var C[T]): T =
  ## run a continuation; returns a result
  while not c.fn.isNil:
    #stdout.write "run "
    c = c.fn(c)
  result = c.data

proc `...`[T](d: T; c: C[T]) =
  # asgn
  echo "asgn"
  c.data = d

proc `...`[T](c: C[T]; d: T): C[T] =
  # asgn
  echo "asgn"
  result = c
  result.data = d

proc `...`[T](a, b: C[T]): C[T] =
  # and
  echo "and"
  result = a(b)

proc `...`[T](c: var C[T]; p: proc (m: var T)): C[T] =
  echo "mutate"
  result = c
  p(result.data)

proc `...`[T](c: var C[T]; p: proc (m: T): T): C[T] =
  echo "replace"
  result = c
  result.data = p(c.data)

proc `...`(c: C; p: P[C]): C =
  # continue
  echo "continue"
  result = c
  result.fn = p


when false:
  type
    ## we'll design our own generic datatype
    Fibber[T] = object
      fn: proc (c: var Fibber[T]): Fibber[T] {.nimcall.}
      data: seq[T]

  static:
    assert Fibber[int] is C[seq[int]]

  proc grow[T](c: var Fibber[T]): Fibber[T] =
    ## "grow" a continuation; whatever that means
    echo "grow()"
    result = c
    let d = c.data[^2].ord + c.data[^1].ord
    if d > high(T).ord:
      echo "except high enough ", d
      result.fn = nil
    else:
      result.data.add T(d)

  proc shrink[T](c: var Fibber[T]): Fibber[T] =
    ## the opposite operation
    echo "shrink()"
    result = c
    if len(c.data) > 0:
      var d = initDeque[T]()
      for n in items(c.data):
        d.addLast n
      echo "shrunk off ", popFirst(d)
      result.data = newSeq[T](len(d))
      for i, n in pairs(d):
        result.data[i] = n
    if len(result.data) <= 8:
      echo "except low enough"
      result.fn = nil

  proc calculate[T: Ordinal](n: T): seq[T] =
    assert grow is P   # grow is a continuation proc

    # make a "computer" using code and data
    var
      c: Fibber[T]

    assert c is C              # c is continuation
    assert c is C[seq[T]]      # c is abstraction
    assert c is Fibber[T]      # c is concrete type

    let a = @[n, n.succ]
    #c = a ... grow[T]
    #c = c ... a                # stuff data in
    #c.data = a
    #c.fn = grow[T]             # stuff code in
    c = Fibber[T](fn: grow[T], data: a)

    # call the continuation to produce a result
    result = c()
    let l = len(c())
    echo c()
    assert l == len(c())

    var d = c ... shrink[T]
    var e = d ... shrink[T]           # uses current value of c!
    assert d is C                     # these are continuations,
    assert e is C                     # what else?

    # c() does not run grow() again
    assert len(e()) == 8   # e has run shrink() once
    assert len(d()) == 8   # d has run shrink() once

    echo "pre-shrunk: ", c()          # noop; yields result
    echo "    shrunk: ", d()          # noop; yields result
    echo "veryshrunk: ", e()          # noop; yields result

    # do it all at once
    var f = e.shrink()                # resolved continuation
    assert f is C                     # of course
    echo "     frunk: ", f()          # noop; yields result

  assert calculate(0'i8) == @[0'i8, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
  assert calculate(0'u8) == @[0'u8, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233]

block:
  type
    Foo = object
      fn: proc (c: var Foo): Foo {.nimcall.}
      data: string
    M = proc (s: string): string

  static:
    assert Foo is C
    assert Foo is C[string]

  proc `<-`(c: var Foo; s: string) =
    c.data = s

  proc `->`(c: var Foo; p: Foo.fn): Foo =
    result = c
    result.fn = p

  macro `->`[T](d: T; c: var Foo): Foo =
    result = newStmtList()
    result.add newAssignment(newDotExpr(c, ident"data"), d)
    result.add c
    echo treeRepr(result)

  macro `->`(c: var Foo; p: M): Foo =
    result = copyNimNode(c)
    var body = newStmtList()
    let data = newDotExpr(c, ident"data")
    body.add newAssignment(newDotExpr(ident"c", ident"data"),
                           newCall(p, data))
    body.add newAssignment(newDotExpr(c, ident"fn"), newNilLit())
    var lambda = newProc(newEmptyNode(), body = body,
                         params = [ident"Foo",
                           newIdentDefs(ident"c",
                                        newTree(nnkVarTy, ident"Foo"),
                                        newEmptyNode())])
    result = newStmtList()
    result.add newAssignment(newDotExpr(ident"c", ident"fn"), lambda)
    result.add ident"c"
    echo treeRepr(result)

  var c: Foo
  expandMacros:
    var upper = "hello" -> c -> toUpperAscii
    echo "c = ", c()
    echo "u = ", upper()
