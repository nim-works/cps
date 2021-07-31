import std/os
import std/genasts
import std/macros
import std/strutils

from cps/spec import Hook

include preamble
import killer

suite "hooks":

  block:
    ## cooperative yield hooks are used automatically
    shouldRun 6:
      proc coop(c: Cont): Cont {.cpsMagic, used.} =
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
    macro trace[T](hook: static[Hook]; c, n: typed;
                   fun: string; info: LineInfo; body: T): untyped =
      var body =
        if body.kind == nnkNilLit:
          newEmptyNode()
        else:
          body
      result =
        genAst(c, hook, fun, info, body):
          let last = if hook == Dealloc: "😎" else: astToStr c
          let sub = fun.split("_", maxsplit=1)[0]
          var path = info.filename.lastPathPart
          path = if path == "thooks.nim": "👍" else: path
          found.add "$# $#: $# $# $#" % [ $hook, $found.len, $sub, last, path ]
          body

    proc bar() {.cps: Cont.} =
      noop()

    proc foo() {.cps: Cont.} =
      var i = 0
      while i < 3:
        noop()
        inc i
        if i == 0:
          continue
        if i > 2:
          break
        bar()

    foo()
    let s = found.join("\10")
    const
      expected = """
        alloc 0: cps environment Cont normalizedast.nim
        head 1: trace nil normalizedast.nim
        boot 2: C nil normalizedast.nim
        trace 3: foo continuation 👍
        coop 4: continuation nil genasts.nim
        trace 5: While Loop continuation 👍
        trace 6: Post Call continuation 👍
        tail 7: Cont Continuation(continuation) normalizedast.nim
        alloc 8: cps environment Cont normalizedast.nim
        boot 9: result nil normalizedast.nim
        pass 10: cps environment continuation normalizedast.nim
        trace 11: bar continuation 👍
        trace 12: Post Call continuation 👍
        pass 13: continuation.mom continuation normalizedast.nim
        coop 14: result nil normalizedast.nim
        dealloc 15: cps environment 😎 genasts.nim
        trace 16: Post Child continuation normalizedast.nim
        coop 17: continuation nil genasts.nim
        trace 18: While Loop continuation 👍
        trace 19: Post Call continuation 👍
        tail 20: Cont Continuation(continuation) normalizedast.nim
        alloc 21: cps environment Cont normalizedast.nim
        boot 22: result nil normalizedast.nim
        pass 23: cps environment continuation normalizedast.nim
        trace 24: bar continuation 👍
        trace 25: Post Call continuation 👍
        pass 26: continuation.mom continuation normalizedast.nim
        coop 27: result nil normalizedast.nim
        dealloc 28: cps environment 😎 genasts.nim
        trace 29: Post Child continuation normalizedast.nim
        coop 30: continuation nil genasts.nim
        trace 31: While Loop continuation 👍
        trace 32: Post Call continuation 👍
      """.dedent(8).strip()
    if s != expected:
      fail "trace output doesn't match; received:\n" & s

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
      proc dealloc[T: Cont](c: sink T; E: typedesc[T]): E =
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
      proc dealloc[T: Cont](c: sink T; E: typedesc[T]): E =
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
    proc unwind(c: Cont; ex: ref Exception): Continuation {.cpsMagic, used.} =
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
