include preamble

suite "calling convention":

  block:
    ## some different callback types
    type
      C = ref object of Continuation
      H {.used.} = proc(x: int): float {.cps: C.}
      I {.used.} = proc(): float {.cps: C.}
      P {.used.} = proc(x: int) {.cps: C.}
      S {.used.} = proc() {.cps: C.}

  block:
    ## produce callbacks from a symbol
    type
      C = ref object of Continuation

    proc toH(x: int): float {.cps: C.} = discard
    proc toI(): float {.cps: C.} = discard
    proc toP(x: int) {.cps: C.} = discard
    proc toS() {.cps: C.} = discard

    let h {.used.} = whelp toH
    let i {.used.} = whelp toI
    let p {.used.} = whelp toP
    let s {.used.} = whelp toS

  block:
    ## produce callbacks that match whelp types
    type
      C = ref object of Continuation
      H {.used.} = proc(x: int): float {.cps: C.}
      I {.used.} = proc(): float {.cps: C.}
      P {.used.} = proc(x: int) {.cps: C.}
      S {.used.} = proc() {.cps: C.}

    proc toH(x: int): float {.cps: C.} = discard
    proc toI(): float {.cps: C.} = discard
    proc toP(x: int) {.cps: C.} = discard
    proc toS() {.cps: C.} = discard

    let h {.used.}: H = whelp toH
    let i {.used.}: I = whelp toI
    let p {.used.}: P = whelp toP
    let s {.used.}: S = whelp toS

  block:
    ## execute a callback with arguments
    type
      C = ref object of Continuation

    proc foo(x: int): float {.cps: C.} =
      result = x.float

    const cb = whelp foo
    var c = cb.call(3)
    var d = cb.call(5)
    check cb.recover(c) == 3.0
    check cb.recover(d) == 5.0

  block:
    ## run a callback from inside cps with callback type
    var k = newKiller 4

    type
      ContCall = proc(a: int): int {.cps: Cont.}

    proc bar(a: int): int {.cps: Cont.} =
      noop()
      step 3
      return a * 2

    proc foo(c: ContCall) {.cps: Cont.} =
      step 1
      var x = c.call(4)
      step 2
      check c.recover(x) == 8
      step 4

    foo: whelp bar

  block:
    ## whelp helps disambiguate cps callbacks at instantiation
    var k = newKiller 2

    type
      ContCall {.used.} = proc(a: int): int {.cps: Cont.}
      MoreCall {.used.} = proc(a: float): float {.cps: Cont.}

    proc bar(a: float): float {.cps: Cont.} =
      noop()
      return a * 2.0

    proc bar(a: int): int {.cps: Cont.} =
      step 2
      noop()
      return a * 2

    proc foo(c: ContCall) {.cps: Cont.} =
      step 1
      var x = c(4)
      check x == 8

    foo: whelp(ContCall, bar)

  block:
    ## run a callback in cps with natural syntax
    when not cpsCallOperatorSupported:
      skip "unsupported on nim " & NimVersion
    else:

      type
        ContCall = proc(a: int): int {.cps: Cont.}

      var k = newKiller 3

      proc bar(a: int): int {.cps: Cont.} =
        noop()
        step 2
        return a * 2

      proc foo(c: ContCall) {.cps: Cont.} =
        step 1
        let x = c(4)
        check x == 8
        step 3

      const cb = whelp bar
      foo cb

  block:
    ## callback illustration
    type
      C = ref object of Continuation
      Callback = proc(x: int): float {.cps: C.}

    proc bar(a: int): float {.cps: C.} =
      return 2.0 * a.float

    const
      cb: Callback = whelp bar

    var x: C = cb.call(2)
    var y: C = cb.call(4)
    check cb.recover(x) == 4.0
    check cb.recover(y) == 8.0
