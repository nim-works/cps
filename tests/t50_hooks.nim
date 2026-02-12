when defined(isNimSkull):
  {.passC: "-fbracket-depth=1024".}

include preamble

import std/genasts
import std/macros
import std/os
import std/sequtils
import std/strutils

from cps/spec import Hook, cpsStackFrames
from cps/hooks import findColonLit

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
        let sub = fun.split("_", maxsplit=1)[0]
        var last =
          case hook
          of Dealloc: "😎"
          of Stack: sub
          else: astToStr c
        var path = info.filename.lastPathPart
        path = if path == "t50_hooks.nim": "👍" else: path
        found.add "$#: $# $# $#" % [ $hook, $sub, last, path ]
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
          alloc: cps:foo() env Cont 👍
          head: trace nil 👍
          stack: foo foo 👍
          boot: c nil 👍
          trace: foo continuation 👍
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          tail: Cont continuation 👍
          alloc: cps:bar() env Cont 👍
          stack: bar bar 👍
          boot: Cont nil 👍
          pass: cps:foo() env Cont(continuation) 👍
          trace: bar continuation 👍
          trace: cps:bar() jump noop() continuation 👍
          pass: continuation.mom Cont(continuation) environment.nim
          coop: result nil ast.nim
          dealloc: cps:bar() env 😎 environment.nim
          trace: cps:foo() child bar() continuation ast.nim
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          tail: Cont continuation 👍
          alloc: cps:bar() env Cont 👍
          stack: bar bar 👍
          boot: Cont nil 👍
          pass: cps:foo() env Cont(continuation) 👍
          trace: bar continuation 👍
          trace: cps:bar() jump noop() continuation 👍
          pass: continuation.mom Cont(continuation) environment.nim
          coop: result nil ast.nim
          dealloc: cps:bar() env 😎 environment.nim
          trace: cps:foo() child bar() continuation ast.nim
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          dealloc: cps:foo() env 😎 👍
        """
    else:
      const
        expected = """
          alloc: cps:foo() env Cont 👍
          head: trace nil 👍
          boot: c nil 👍
          trace: foo continuation 👍
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          tail: Cont continuation 👍
          alloc: cps:bar() env Cont 👍
          boot: Cont nil 👍
          pass: cps:foo() env Cont(continuation) 👍
          trace: bar continuation 👍
          trace: cps:bar() jump noop() continuation 👍
          pass: continuation.mom Cont(continuation) environment.nim
          coop: result nil ast.nim
          dealloc: cps:bar() env 😎 environment.nim
          trace: cps:foo() child bar() continuation ast.nim
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          tail: Cont continuation 👍
          alloc: cps:bar() env Cont 👍
          boot: Cont nil 👍
          pass: cps:foo() env Cont(continuation) 👍
          trace: bar continuation 👍
          trace: cps:bar() jump noop() continuation 👍
          pass: continuation.mom Cont(continuation) environment.nim
          coop: result nil ast.nim
          dealloc: cps:bar() env 😎 environment.nim
          trace: cps:foo() child bar() continuation ast.nim
          coop: Cont nil environment.nim
          trace: cps:foo() loop continuation 👍
          trace: cps:foo() jump noop() continuation 👍
          dealloc: cps:foo() env 😎 👍
        """
    let x = expected.normalize
    let y = s.normalize
    if x != y:
      if x.len != y.len:
        checkpoint "expected:"
        for n in x.items:
          checkpoint n
        checkpoint "\n"
        checkpoint "received:"
        for n in y.items:
          checkpoint n
      else:
        var i = 0
        for (a, b) in zip(x, y).items:
          if a != b:
            checkpoint "#", i, " < ", a
            checkpoint "#", i, " > ", b
          inc i
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
    shouldRun 5:
      proc dealloc[T: Cont](c: sink T; E: typedesc[T]): E =
        ran()    # (runs twice)
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
    check k
