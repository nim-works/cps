when not compiles(fail "bogus"):
  import pkg/balls

type
  Killer* = object
    final: int
    n: int

proc `=destroy`*(k: var Killer) =
  if k.final != k.n:
    let e = getCurrentException()
    # don't obliterate current exception
    if e.isNil or e isnot FailError or e isnot ExpectedError:
      fail:
        case k.n
        of 0: "uninitialized"
        of 1: "unused"
        else: "misused; " & $(k.n - 1) & " uses, expected " & $(k.final - 1)

proc initKiller*(final = 1): Killer =
  Killer(n: 1, final: final + 1)

proc newKiller*(final = 1): Killer =
  ## FIXME: rename this to initKiller
  Killer(n: 1, final: final + 1)

proc inc*(k: var Killer) = system.inc k.n
template step*(k: Killer): int = k.n - 1

template step*(i: int) {.dirty.} =
  check k.n == i, "expected step " & $k.n & " but hit " & $i
  inc k.n
  k.final = max(i, k.final)
