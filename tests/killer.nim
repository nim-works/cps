import balls

type
  Killer* = object
    x: int
    n: int

proc `=destroy`*(k: var Killer) =
  if k.x != k.n:
    fail:
      case k.n
      of 0: "uninitialized"
      of 1: "unused"
      else: "misused; " & $(k.n - 1) & " uses, expected " & $(k.x - 1)

proc newKiller*(x = 1): Killer =
  Killer(n: 1, x: x + 1)

template step*(k: Killer): int = k.n - 1

template step*(i: int) {.dirty.} =
  check k.n == i, "out-of-order"
  inc k.n
  k.x = max(i, k.x)
