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
