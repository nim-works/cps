# Example 7: Testing Functional/Streams composition

import ../cps/core, options, macros, sugar

proc infiniteList(): int {.coro.} =
  var count = 0
  while true:
    yield count
    inc count

proc takeWhile(source: sink Coroutine, cond: proc(x: int): bool): int {.coro.} =
  # undeclared identifier "source" :/
  while (let val = pull(source); val.isSome() and val.get.cond()):
    yield val
  # while true:
  #   let val: int = source.pull()
  #   if val.isSome() and val.get().cond():
  #     yield val.get()
  #   else: # The end of monkey patching, break or return
  #         # lead to weird AST due to the "exiter" fix in transform.
  #     return

var a = infiniteList() # .takeWhile(x => x < 10)

echo a.pull()
echo a.pull()
echo a.pull()
echo a.pull()
echo a.pull()
