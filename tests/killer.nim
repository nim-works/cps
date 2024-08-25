when not compiles(fail "bogus"):
  import pkg/balls

type
  Killer* = object
    final: int
    n: int

proc failMsg(k: Killer): string =
  case k.n
  of 0: "uninitialized"
  of 1: "unused"
  else: "misused; " & $(k.n - 1) & " uses, expected " & $(k.final - 1)

proc `=destroy`*(k: var Killer) {.raises: [].} =
  ## Raise a `Defect` if `k` has errors.
  ##
  ## `check()` should be used to have errors raised as test failures.
  ##
  ## This destructor is a last-ditch effort for when `check()` is not
  ## run.
  # Only crash if there's no exception running
  if k.final != k.n and getCurrentException() == nil:
    raise newException(Defect, k.failMsg)

proc check*(k: Killer) {.raises: [FailError].} =
  ## Check `k` for errors.
  if k.final != k.n:
    fail k.failMsg

proc clear*(k: var Killer) =
  ## Clear and defuse `k`. `k` is not reusable after this.
  k.final = 0
  k.n = 0

proc initKiller*(final = 1): Killer =
  Killer(n: 1, final: final + 1)

proc newKiller*(final = 1): Killer =
  ## FIXME: rename this to initKiller
  Killer(n: 1, final: final + 1)

proc inc*(k: var Killer) = system.inc k.n
template step*(k: Killer): int = k.n - 1

template step*(k: var Killer, i: int) =
  bind check

  check k.n == i, "expected step " & $k.n & " but hit " & $i
  inc k.n
  k.final = max(i, k.final)

template step*(i: int) =
  mixin k
  bind step

  step k, i
