version = "0.0.13"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"
requires "nim >= 1.5.1"

requires "https://github.com/disruptek/testes >= 0.7.3 & < 1.0.0"

task test, "run tests for ci":
  when defined(windows):
    exec "testes.cmd"
  else:
    exec findExe"testes"

task demo, "generate the demos":
  exec """demo docs/demo.svg "nim c -d:danger -d:cpsDebug --out=\$1 tests/tock.nim""""
  exec """demo docs/tzevv.svg "nim c --out=\$1 tests/tzevv.nim""""
  exec """demo docs/taste.svg "nim c --out=\$1 tests/taste.nim""""
