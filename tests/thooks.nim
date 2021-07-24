import std/genasts
import std/macros
import std/strutils
import std/sequtils

from cps/hooks import Hook

include preamble
import killer

suite "hooks":

  block:
    ## cooperative yield hooks are used automatically
    shouldRun 6:
      proc coop(c: Cont): Cont {.cpsMagic.} =
        ran()
        result = c

      proc foo() {.cps: Cont.} =
        var i = 0
        while i < 3:
          ran()
          noop()
          inc i
          if i == 0:
            continue
          if i > 2:
            break

      foo()

  block:
    ## control-flow tracing hooks are used automatically
    var found: seq[string]
    macro trace[T](hook: static[Hook]; c: typed;
                   fun: string; info: LineInfo; body: T): untyped =
      var body =
        if body.kind == nnkNilLit:
          newEmptyNode()
        else:
          body
      result =
        genAst(c, hook, fun, info, body):
          let sub = fun.split("_", maxsplit=1)[0]
          found.add "$# $#: $# $# $#" % [ $hook, $found.len, $sub,
                                          $info.column, astToStr c ]
          body

    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 3:
        noop()
        inc i
        if i == 0:
          continue
        if i > 2:
          break

    foo()
    let s = found.join("\10")
    const
      expected = """
        alloc 0: cps environment 8 Cont
        head 1: trace 8 nil
        boot 2: C 8 nil
        trace 3: foo 4 continuation
        coop 4: continuation 12 nil
        trace 5: While Loop 12 continuation
        trace 6: Post Call 8 continuation
        coop 7: continuation 12 nil
        trace 8: While Loop 12 continuation
        trace 9: Post Call 8 continuation
        coop 10: continuation 12 nil
        trace 11: While Loop 12 continuation
        trace 12: Post Call 8 continuation
      """.dedent(8).strip()
    check "trace output doesn't match":
      s == expected

  block:
    ## custom continuation allocators are used automatically
    shouldRun 1:
      proc alloc[T](root: typedesc[Cont]; c: typedesc[T]): T =
        ran()
        new c

      proc foo(x: int) {.cps: Cont.} =
        check x == 3
        noop()
        check x == 3

      foo(3)

  block:
    ## custom continuation deallocators can nil the continuation
    shouldRun 4:
      proc dealloc[T: Cont](t: typedesc; c: sink T) =
        ran()
        c = nil

      proc bar() {.cps: Cont.} =
        ran()
        noop()
        ran()

      proc foo(x: int) {.cps: Cont.} =
        check x == 3
        noop()
        check x == 3
        bar()
        ran()

      foo(3)

  block:
    ## custom continuation deallocators work with whelp
    shouldRun 4:
      proc dealloc[T: Cont](t: typedesc; c: sink T) =
        ran()
        c = nil

      proc bar() {.cps: Cont.} =
        ran()
        noop()
        ran()

      proc foo(x: int) {.cps: Cont.} =
        check x == 3
        noop()
        check x == 3
        bar()
        ran()

      let c = whelp foo(3)
      trampoline c

  block:
    ## custom continuation passing hook works
    shouldRun 34:
      proc pass(a: Cont; b: Continuation): Continuation =
        echo "pass to parent"
        for n in 0..9: ran()
        result = b

      proc pass(a: Cont; b: Cont): Continuation =
        echo "pass to child"
        for n in 0..9: ran()
        for n in 0..9: ran()
        result = b

      proc bar() {.cps: Cont.} =
        ran()
        noop()
        ran()

      proc foo() {.cps: Cont.} =
        ran()
        bar()
        ran()

      foo()

  block:
    ## custom continuation bootstrap hook works
    var k = newKiller 1

    proc bar() {.cps: Cont.} =
      noop()

    proc boot(c: Cont): Cont =
      step 1
      result = c

    proc foo() {.cps: Cont.} =
      bar()

    foo()

  block:
    ## custom continuation head/tail setup hooks work
    var h, t = 0

    proc head(c: Cont): Cont =
      inc h
      result = c

    proc tail(mom: Continuation; c: Cont): Continuation =
      inc t
      result = c
      result.mom = mom

    proc bar() {.cps: Cont.} =
      check h == 1, "parent triggered second"
      noop()

    proc foo() {.cps: Cont.} =
      check h == 1, "parent triggered first"
      check t == 0, "child triggered first"
      bar()
      check t == 1, "child triggered second"

    var c = whelp foo()
    c = cps.trampoline c
    check "bzzzt whelped":
      h == t
      t == 1

    h = 0
    t = 0
    foo()
    check "bzzzt bootstrapped":
      h == t
      t == 1

  block:
    ## custom continuation exception handling works
    var k = newKiller 4
    proc unwind(c: Cont; ex: ref Exception): Continuation {.cpsMagic.} =
      inc k
      result = cps.unwind(c, ex)

    proc bar() {.cps: Cont.} =
      step 2
      noop()
      raise IOError.newException "hol' up"

    proc foo() {.cps: Cont.} =
      step 1
      bar()
      step 4

    expect IOError:
      foo()
