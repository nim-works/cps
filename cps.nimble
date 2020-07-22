version = "0.0.4"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"
requires "nim >= 1.3.5"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
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
  execTest("tests/test.nim")
  execTest("tests/tock.nim")
  execTest("tests/tyield.nim")
