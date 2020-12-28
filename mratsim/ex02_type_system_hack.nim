# Example 2: making Continuation an object
type
  EnvBase = object of RootObj

  EnvA = object of EnvBase
    i: int

  EnvB = object of EnvBase
    j: string

  ContinuationFoo = object
    fn: proc(c: var ContinuationFoo) {.nimcall.}
    envs: ref EnvBase

let a = ContinuationFoo(envs: (ref EnvA)(i: 2))
let b = ContinuationFoo(envs: (ref EnvB)(j: "Hello"))

let s = [a, b]
echo s.repr

import macros

proc foo(c: var ContinuationFoo) =
  discard

dumpTree:
  let c = (ref ContinuationFoo)(fn: foo, envs: EnvA(i: 42))

  type
    ContinuationFoo = object
      fn: proc(c: var ContinuationFoo) {.nimcall.}
      envs: ref EnvBase

  proc foo(x: int){.cps:int.} = discard
