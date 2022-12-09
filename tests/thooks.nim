import std/genasts
import std/macros
import std/os
import std/sequtils
import std/strutils

from cps/spec import Hook, cpsStackFrames
from cps/hooks import findColonLit

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
    macro trace(hook: static[Hook]; c, n: typed;
                fun: string; info: LineInfo; body: typed): untyped =
      var body =
        if body.kind == nnkNilLit:
          newEmptyNode()
        else:
          body
      let fun =
        if hook == Stack: c.findColonLit("fun", string)
                    else: fun.strVal
      genAst(c, hook, fun, info, body):
        var last =
          case hook
          of Dealloc: "😎"
          of Stack: fun
          else: astToStr c
        let sub = fun.split("_", maxsplit=1)[0]
        last = if hook == Stack: last.split("_", maxsplit=1)[0] else: last
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

    proc normalize(s: string): seq[string] =
      var s = strip s
      result = splitLines s
      result = map(result, proc(x: string): string = strip(x))

    when cpsStackFrames:
      const
        expected = """
          alloc 0: cps:foo() env Cont 👍
          head 1: trace nil 👍
          stack 2: foo foo 👍
          boot 3: c nil 👍
          trace 4: foo continuation 👍
          coop 5: Cont nil environment.nim
          trace 6: cps:foo() loop continuation 👍
          trace 7: cps:foo() jump noop() continuation 👍
          tail 8: Cont continuation 👍
          alloc 9: cps:bar() env Cont 👍
          stack 10: bar bar 👍
          boot 11: Cont nil 👍
          pass 12: cps:foo() env Cont(continuation) 👍
          trace 13: bar continuation 👍
          trace 14: cps:bar() jump noop() continuation 👍
          pass 15: continuation.mom Cont(continuation) environment.nim
          dealloc 16: cps:bar() env 😎 environment.nim
          trace 17: cps:foo() child bar() continuation normalizedast.nim
          coop 18: Cont nil environment.nim
          trace 19: cps:foo() loop continuation 👍
          trace 20: cps:foo() jump noop() continuation 👍
          tail 21: Cont continuation 👍
          alloc 22: cps:bar() env Cont 👍
          stack 23: bar bar 👍
          boot 24: Cont nil 👍
          pass 25: cps:foo() env Cont(continuation) 👍
          trace 26: bar continuation 👍
          trace 27: cps:bar() jump noop() continuation 👍
          pass 28: continuation.mom Cont(continuation) environment.nim
          dealloc 29: cps:bar() env 😎 environment.nim
          trace 30: cps:foo() child bar() continuation normalizedast.nim
          coop 31: Cont nil environment.nim
          trace 32: cps:foo() loop continuation 👍
          trace 33: cps:foo() jump noop() continuation 👍
        """
    else:
      const
        expected = """
          alloc 0: cps:foo() env Cont 👍
          head 1: trace nil 👍
          boot 2: c nil 👍
          trace 3: foo continuation 👍
          coop 4: Cont nil environment.nim
          trace 5: cps:foo() loop continuation 👍
          trace 6: cps:foo() jump noop() continuation 👍
          tail 7: Cont continuation 👍
          alloc 8: cps:bar() env Cont 👍
          boot 9: Cont nil 👍
          pass 10: cps:foo() env Cont(continuation) 👍
          trace 11: bar continuation 👍
          trace 12: cps:bar() jump noop() continuation 👍
          pass 13: continuation.mom Cont(continuation) environment.nim
          dealloc 14: cps:bar() env 😎 environment.nim
          trace 15: cps:foo() child bar() continuation normalizedast.nim
          coop 16: Cont nil environment.nim
          trace 17: cps:foo() loop continuation 👍
          trace 18: cps:foo() jump noop() continuation 👍
          tail 19: Cont continuation 👍
          alloc 20: cps:bar() env Cont 👍
          boot 21: Cont nil 👍
          pass 22: cps:foo() env Cont(continuation) 👍
          trace 23: bar continuation 👍
          trace 24: cps:bar() jump noop() continuation 👍
          pass 25: continuation.mom Cont(continuation) environment.nim
          dealloc 26: cps:bar() env 😎 environment.nim
          trace 27: cps:foo() child bar() continuation normalizedast.nim
          coop 28: Cont nil environment.nim
          trace 29: cps:foo() loop continuation 👍
          trace 30: cps:foo() jump noop() continuation 👍
        """
    let x = expected.normalize
    let y = s.normalize
    if x != y:
      for (a, b) in zip(x, y).items:
        if a != b:
          checkpoint "< ", a
          checkpoint "> ", b
      fail "trace output doesn't match"

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
    shouldRun 2:

      proc bar() {.cps: Cont.} =
        noop()

      proc boot(c: Cont): Cont =
        ran()
        result = c

      proc foo() {.cps: Cont.} =
        bar()

      foo()

      var c = whelp foo()
      c = cps.trampoline c

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
