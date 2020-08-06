version = "0.0.13"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"
requires "nim >= 1.3.5"

requires "https://github.com/narimiran/sorta < 1.0.0"
requires "https://github.com/disruptek/testes"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
  if getEnv("TRAVIS_COMPILER", "unlikely") == "unlikely":
    execCmd "nim cpp             --gc:refc -r " & test
  else:
    execCmd "nim c              -r " & test
    execCmd "nim c   -d:release -r " & test
    execCmd "nim c   -d:danger  -r " & test
    execCmd "nim c             --gc:arc -r " & test
    execCmd "nim c   -d:danger --gc:arc -r " & test
    execCmd "nim cpp            -r " & test
    execCmd "nim cpp -d:danger  -r " & test
    execCmd "nim cpp           --gc:arc -r " & test
    execCmd "nim cpp -d:danger --gc:arc -r " & test

task test, "run tests for travis":
  execTest("tests/tease.nim")
  execTest("tests/tzevv.nim")

task docs, "generate the docs":
  exec "nim doc --path:. --outdir=docs cps.nim"
  exec "nim doc --path:. --outdir=docs/cps cps/eventqueue.nim"
  exec "nim doc --path:. --outdir=docs/cps cps/semaphore.nim"
  exec "termtosvg docs/demo.svg --loop-delay=5000 --screen-geometry=80x60 --template=window_frame_powershell --command=\"nim c --gc:refc --define:danger -r -f tests/tock.nim\""
  exec "termtosvg docs/tease.svg --loop-delay=10000 --screen-geometry=80x60 --template=window_frame_powershell --command=\"nim cpp --gc:refc --define:danger -r -f tests/tease.nim\""
  exec "termtosvg docs/tzevv.svg --loop-delay=10000 --screen-geometry=80x60 --template=window_frame_powershell --command=\"nim c --gc:refc --define:danger -r -f tests/tzevv.nim\""
