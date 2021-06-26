import cps

type
  C = ref object of Continuation
    val: int

proc noop(c: C): C {.cpsMagic.} =
  result = c

proc level_two(): int {.cps:C.} =
  noop()
  result = 42
  noop()

proc entry*(): int {.cps:C.} =
  result = level_two()
