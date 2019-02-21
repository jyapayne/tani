# Package

version       = "0.1.0"
author        = "Joey Payne"
description   = "Simple testing for Nim."
license       = "MIT"

srcDir = "src"

# Deps
requires "nim >= 0.19.4"

task test, "Run tests":
  exec "nim c -r tests/test.nim"

task testjs, "Run tests on Node.js":
  exec "nim js -d:nodejs -r tests/test.nim"
