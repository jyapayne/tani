# tani
A light Nim testing framework


## Usage

For a directory structure like:

```
tests/
  runner.nim
  testThings.nim
```

Simply sprinkle the `test` macro anywhere in your code and call the `runTests` macro once at the end of the module to run all tests collected in that module. You must also defined a `runner` module in the root directory that simply has the following code in it:

```nim
# runner.nim
import tani

discoverAndRunTests()
```

Compile the above with `nim c -d:test -r runner.nim`, and it will automatically find all modules in the same directory and subdirectories and run them if the modules (file names) begin with `test`.

Contents of `testThings.nim`:

```nim
# testThings.nim
import tani

proc doSomething(): string =
  test "test do something":
    let testValue = "the value"
    check doSomething() == testValue

  return "the value"

runTests()
```
