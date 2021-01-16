version = "0.0.14"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"

when not defined(release):
  requires "https://github.com/disruptek/testes#noic"
  requires "https://github.com/disruptek/criterion < 1.0.0"

task test, "run tests for ci":
  when defined(windows):
    exec "testes.cmd"
  else:
    exec findExe"testes"

task demo, "generate the demos":
  exec """demo docs/tock.svg "nim c -d:danger -d:cpsDebug --out=\$1 tests/tock.nim""""
  exec """demo docs/tzevv.svg "nim c --out=\$1 tests/tzevv.nim""""
  exec """demo docs/taste.svg "nim c --out=\$1 tests/taste.nim""""
  exec """demo docs/teventqueue.svg "nim c --out=\$1 tests/teventqueue.nim""""
  exec """demo docs/techo.svg "nim c --out=\$1 tests/techo.nim""""
  exec """demo docs/ttypes.svg "nim c --out=\$1 tests/ttypes.nim""""
