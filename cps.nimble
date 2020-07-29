version = "0.0.11"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"
requires "nim >= 1.3.5"

requires "https://github.com/narimiran/sorta < 1.0.0"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
  if getEnv("TRAVIS_COMPILER", "unlikely") == "unlikely":
    execCmd "nim c             --gc:arc -r " & test
  else:
    execCmd "nim c           -f -r " & test
    execCmd "nim c   -d:release -r " & test
    execCmd "nim c   -d:danger  -r " & test
    execCmd "nim c             --gc:arc -r " & test
    execCmd "nim c   -d:danger --gc:arc -r " & test
    execCmd "nim cpp            -r " & test
    execCmd "nim cpp -d:danger  -r " & test
    execCmd "nim cpp           --gc:arc -r " & test
    execCmd "nim cpp -d:danger --gc:arc -r " & test

task test, "run tests for travis":
  execTest("tests/tblock.nim")
  execTest("tests/tsignal.nim")
  execTest("tests/tock.nim")
  execTest("tests/test.nim")
  #execTest("tests/tyield.nim")
  execTest("tests/tfork.nim")
  execTest("tests/tbreak.nim")
  execTest("tests/tfor.nim")
  execTest("tests/tzevv.nim")

task mkdoc, "generate the docs":
  exec "nim doc --path:. --outdir=docs cps.nim"
  exec "nim doc --path:. --outdir=docs/cps cps/eventqueue.nim"
  exec "nim doc --path:. --outdir=docs/cps cps/semaphore.nim"
