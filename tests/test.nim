import tani
import terminal
import os

proc foo() =
  test "test 2":
    let foo = 100
    foo()
    check(foo < 4)

proc y(): int =
  test "y test1":
    check y() > 0

  test "foo bar":
    echo os.joinPath("test", "output")
    check y() > 0
    checkRaises(Exception):
      raise newException(Exception, "Foo Exception")

  return 10

runTests()
