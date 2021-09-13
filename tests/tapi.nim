include preamble
import killer

import tests/exports

suite "cps api":

  block:
    ## bootstrap
    var k = newKiller 1
    proc bootstrap(): int {.cps: Cont.} =
      noop()
      step 1
      noop()
      return 3

    var i = bootstrap()
    check i is int, "bootstrap's output is not an int"
    check i == 3, "bootstrap's output has the wrong value"

  block:
    ## whelp
    var k = newKiller 0
    proc whelped(): int {.cps: Cont.} =
      noop()
      step 1
      noop()
      return 3

    var c = whelp whelped()
    check "whelp's output is bogus":
      c is Cont

  block:
    ## state symbols and trampoline
    var k = newKiller 1
    proc states(): int {.cps: Cont.} =
      noop()
      step 1
      noop()
      return 3

    var c = whelp states()
    check "whelp initial state surprised us":
      not c.dismissed
      not c.finished
      c.state == State.Running
      c.running
    c = cps.trampoline c
    check "whelp state after trampoline surprised us":
      not c.dismissed
      c.state == State.Finished
      c.finished
      not c.running

  block:
    ## trampolineIt
    var k = newKiller 1
    proc boing(): int {.cps: Cont.} =
      noop()
      step 1
      noop()
      return 3

    var c = whelp boing()
    trampolineIt c:
      check "state inside trampolineIt is " & $it.state:
        not it.dismissed
        it.running
    check "state post-trampolineIt is " & $c.state:
      not c.dismissed
      c.finished

  block:
    ## magic voodoo
    var k = newKiller 4

    proc magic(c: Cont): Cont {.cpsMagic.} =
      step 2
      check not c.dismissed, "continuation was dismissed"
      check not c.finished, "continuation was finished"
      result = c

    proc voodoo(c: Cont): int {.cpsVoodoo.} =
      step 3
      check not c.dismissed, "continuation was dismissed"
      check not c.finished, "continuation was finished"
      result = 2

    proc foo(): int {.cps: Cont.} =
      noop()
      step 1
      check k.step == 1, "right"
      magic()
      check k.step == 2, "magic failed to run"
      noop()
      check voodoo() == 2, "voodoo failed to yield 2"
      noop()
      step 4
      return 3

    check foo() == 3

  block:
    ## exporting CPS procedures works
    check entry() == 42

  block:
    ## accessing exported continuation's result from a continuation works
    skip"later"
    var k = newKiller 2
    proc foo() {.cps: Cont.} =
      step 1
      check entry() == 42
      step 2

    foo()

  block:
    ## one can whelp a cps'd proc that was borrowed
    type
      D = distinct int

    proc bar(x: int) {.cps: Continuation.} =
      discard

    proc bar(d: D) {.borrow.}

    discard whelp bar(42.D)

  block:
    ## one can call a cps'd proc that was borrowed... from inside cps
    type
      D = distinct int

    var k = newKiller 3

    proc bar(x: int; y = 0.0) {.cps: Continuation.} =
      check y == 3.5
      check x == 42
      step 2

    proc bar(d: D; y = 0.0) {.borrow.}

    proc foo() {.cps: Continuation.} =
      step 1
      bar(y = 3.5, d = 42.D)
      step 3

    foo()

  block:
    ## calling magic that is not defined for the base type should not compile
    skip "FIXME: Figure out a way to test compile-time failures":
      type AnotherCont = ref object of Continuation
      proc magic(c: AnotherCont): AnotherCont {.cpsMagic.} = discard

      proc foo() {.cps: Cont.} =
        magic()

  block:
    ## calling magic/voodoo with generics continuation parameter works
    # XXX: Not sure why, but Killer doesn't work here, complaining about
    #      `k` not in scope.
    var r = 0

    type AnotherCont = ref object of Continuation
    proc magic(c: Cont or AnotherCont): auto {.cpsMagic.} =
      inc r
      c

    proc voodoo(c: Cont or AnotherCont) {.cpsVoodoo.} =
      inc r

    proc foo() {.cps: Cont.} =
      inc r
      voodoo()
      magic()

    foo()
    check r == 3

  # Not using block here since using can only be used on the top-level
  using c: Cont

  block:
    ## magic/voodoo can be defined with `using`
    var k = newKiller 4

    proc magic(c): Cont {.cpsMagic.} =
      step 2
      check not c.dismissed, "continuation was dismissed"
      check not c.finished, "continuation was finished"
      result = c

    proc voodoo(c): int {.cpsVoodoo.} =
      step 3
      check not c.dismissed, "continuation was dismissed"
      check not c.finished, "continuation was finished"
      result = 2

    proc foo(): int {.cps: Cont.} =
      noop()
      step 1
      check k.step == 1, "right"
      magic()
      check k.step == 2, "magic failed to run"
      noop()
      check voodoo() == 2, "voodoo failed to yield 2"
      noop()
      step 4
      return 3

    check foo() == 3
