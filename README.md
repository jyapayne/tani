# tani
A light Nim testing framework


## Usage

Simply sprinkle the `test` macro anywhere in your code and call the `runTests` macro once from the main module to run all tests. Tests that are in private procs (outside of the main module) will be skipped due to how the macros are implemented.

```nim
import tani

proc doSomething(): string =
  test "test do something":
    let testValue = "the value"
    check doSomething() == testValue

  return "the value"

when defined(test):
  runTests()
```

## Limitations

Due to implementation details, the test macro cannot run tests contained within private procs from a different module. For example, when `nim c -r test2.nim` is called on the below files, `privateProc` will not run, but `privateProc2` will. `privateProc` will not be counted towards the final tests run and the tests will pass. Tani is smart enough to detect what the not run tests are and will show you a warning when they do not run.

```nim
# test1.nim

import tani

proc privateProc(): int =
  test "test private":
    check privateProc() == 3

  # Do something here

  return 3
```

```nim
# test2.nim
import tani
import test1

proc privateProc2(): int =
  test "test private2":
    check privateProc2() == 6

  # Do something here

  return 6

runTests()
```

Output (`nim c -r test2.nim`):

```
[Running] tests in test1.nim

[Not run] test private
  Test code contains private or non accessible symbols:
    check privateProc() == 3
  Location: test1.nim; line 7; col: 4


[Running] tests in test2.nim
[OK]     test private2

[1/1] tests passed for test2.nim.

[Summary]

  [1/1] tests passed.
```
