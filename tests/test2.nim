import tani

type
  MyObj* = object
    stuff*: int

proc stuff2*(): int =
  test "adfsadf":
    check stuff2() == 200
  return 200


proc testing(): MyObj =
  test "my cool test":
    check testing().stuff == 200

  return MyObj(stuff: 200)

proc stuff() =
  test "my cool test":
    var x = 20
    if x == 20:
      echo "hoo"
    stuff()
    check x == 30
    check testing().stuff == 200
  echo "hello"

runTests()
