version = "0.0.13"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"
requires "nim >= 1.3.5"

requires "https://github.com/disruptek/testes >= 0.2.1 & < 1.0.0"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
  execCmd "nim c              -r " & test
  execCmd "nim c   -d:release -r " & test
  execCmd "nim c   -d:danger  -r " & test
  execCmd "nim c             --gc:arc -r " & test
  execCmd "nim c   -d:danger --gc:arc -r " & test
  execCmd "nim cpp            -r " & test
  execCmd "nim cpp -d:danger  -r " & test
  execCmd "nim cpp           --gc:arc -r " & test
  execCmd "nim cpp -d:danger --gc:arc -r " & test

task test, "run tests for ci":
  execTest("tests/taste.nim")
  execTest("tests/tzevv.nim")

task docs, "generate the docs":
  exec "nim c --gc:refc --define:danger -r -f tests/tzevv.nim"
  exec "nim c --gc:refc --define:danger -r -f tests/taste.nim"
  exec "nim c --gc:refc --define:danger -r --d:cpsDebug -f tests/tock.nim"
  exec "termtosvg docs/demo.svg --loop-delay=5000 --screen-geometry=80x60 --template=window_frame_powershell --command=bin/tock"
  exec "termtosvg docs/tzevv.svg --loop-delay=10000 --screen-geometry=80x25 --template=window_frame_powershell --command=bin/tzevv"
  exec "termtosvg docs/taste.svg --loop-delay=10000 --screen-geometry=80x60 --template=window_frame_powershell --command=bin/taste"
