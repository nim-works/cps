version = "0.8.2"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"

task test, "run tests for ci":
  when defined(windows):
    exec "balls.cmd --panics:on --backend:c --define:release --define:danger --gc:arc --gc:orc"
  else:
    exec "balls --panics:on --backend:c --define:release --define:danger --gc:arc --gc:orc"

task demo, "generate the demos":
  exec """demo docs/tzevv.svg "nim c --out=\$1 tests/zevv.nim""""
  exec """demo docs/taste.svg "nim c --out=\$1 tests/taste.nim""""

task matrix, "generate the matrix":
  exec """demo docs/test-matrix.svg "balls" 34"""
