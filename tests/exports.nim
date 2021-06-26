import cps

type
  C = ref object of Continuation
    val: int

proc send(c: C, v: int) {.cpsVoodoo.} =
  c.val = v

proc recv(c: C): int {.cpsVoodoo.} =
  c.val

proc level_two() {.cps:C.} =
  send(42)

proc entry*(): int {.cps:C.} =
  level_two()
  result = recv()
