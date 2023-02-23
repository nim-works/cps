version = "0.8.3"
author = "disruptek"
description = "continuation-passing style"
license = "MIT"

task demo, "generate the demos":
  exec """demo docs/tzevv.svg "nim c --out=\$1 tests/zevv.nim""""
  exec """demo docs/taste.svg "nim c --out=\$1 tests/taste.nim""""

task matrix, "generate the matrix":
  exec """demo docs/test-matrix.svg "balls" 34"""
