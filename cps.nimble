version = "0.0.13"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"

when not defined(release):
  requires "https://github.com/disruptek/testes >= 1.0.0 & < 1.1.0"
  requires "https://github.com/disruptek/criterion < 1.0.0"

task test, "run tests for ci":
  when defined(windows):
    exec "testes.cmd"
  else:
    exec findExe"testes"

task demo, "generate the demos":
  exec """demo docs/demo.svg "nim c -d:danger -d:cpsDebug --out=\$1 tests/tock.nim""""
  exec """demo docs/tzevv.svg "nim c --out=\$1 tests/tzevv.nim""""
  exec """demo docs/taste.svg "nim c --out=\$1 tests/taste.nim""""
