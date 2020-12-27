
import cps/core

type

  Cont = ref object of RootObj
    fn*: proc(c: Cont): Cont {.nimcall.}


proc test(): Cont {.cps.} =

  while true:
    cps sleep()
    if true:
      break


# brokenbreak.nim(10, 21) template/generic instantiation of `cps` from here
t# brokenbreak.nim(15, 7) Error: invalid control flow: break
