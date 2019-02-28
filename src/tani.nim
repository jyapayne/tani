import macros
import tables, strutils, os

export tables, strutils, os

when defined(ECMAScript):
  const noColors = true
else:
  const noColors = defined(noColors)
  import terminal
  export terminal

import private/utils

type
  Test = ref object
    procDef: NimNode
    name: NimNode

  TestsModule = ref object
    ## The base TestSuite
    fileName: string
    tests: seq[Test]

  TestsInfo = ref object
    fileName: string
    numTests: int
    testsPassed: int
    lastTestFailed: bool

  TestAssertError = object of Exception
    ## check and other check* statements will raise
    ## this exception when the condition fails
    lineNumber: int
    column: int
    fileName: string
    codeSnip: string
    testName: string
    checkFuncName: string
    valTable: Table[string, string]

  PrivateTestError = object of TestAssertError
    ## Raised when a test tries to access something
    ## that is private to a module

var testsModuleMap {.compileTime.} = newTable[string, TestsModule]()

proc `==`*[T](ar: openarray[T], ar2: openarray[T]): bool =
  ## helper proc to compare arrays
  if len(ar) != len(ar2):
    return false
  for i in countup(0, ar.len()):
    if ar[i] != ar2[i]:
      return false
  return true

template returnException(name, testName, snip, vals, pos, posRel) =
  ## private template for raising an exception
  var
    filename = posRel.filename
    line = pos.line
    col = pos.column

  var message = "\l"
  message &= "  Condition: $2($1)\l".format(snip, name)

  if vals.len() > 0:
    message &= "  Where:\l"
    for k, v in vals.pairs:
      message &= "    $1 -> $2\l".format(k, v)

  message &= "  Location: $1; line $2; col: $3".format(filename, line, col)

  var exc = newException(TestAssertError, message)
  exc.fileName = filename
  exc.lineNumber = line
  exc.column = col
  exc.codeSnip = snip
  exc.testName = testName
  exc.valTable = vals
  exc.checkFuncName = name
  raise exc

proc `$`(test: Test): string =
  return "proc `" & $test.name.toStrLit & "`()"

proc `$`*[T](ar: openarray[T]): string =
    ## Converts an array into a string
    result = "["
    if ar.len() > 0:
        result &= $ar[0]
    for i in 1..ar.len()-1:
        result &= ", " & $ar[i]
    result &= "]"

proc typeToStr*[T](some:typedesc[T]): string = name(T)

template tupleObjToStr(obj): string {.dirty.} =
  var res = typeToStr(type(obj))
  template helper(n) {.gensym.} =
    res.add("(")
    var firstElement = true
    for name, value in n.fieldPairs():
      when compiles(value):
        if not firstElement:
          res.add(", ")
        res.add(name)
        res.add(": ")
        when (value is object or value is tuple):
          when (value is tuple):
            res.add("tuple " & typeToStr(type(value)))
          else:
            res.add(typeToStr(type(value)))
          helper(value)
        elif (value is string):
          res.add("\"" & $value & "\"")
        else:
          res.add($value)
        firstElement = false
    res.add(")")
  helper(obj)
  res

proc `$`*(s: ref object): string =
  result = "ref " & tupleObjToStr(s[]).replace(":ObjectType", "")

proc objToStr*[T: object](obj: var T): string =
  tupleObjToStr(obj)

proc objToStr*[T: tuple](obj: T): string =
  result = "tuple " & tupleObjToStr(obj)

macro toString*(obj: typed): untyped =
  ## this macro is to work around not being
  ## able to override system.`$`
  ##
  ## Basically, I want to use my proc to print
  ## objects and tuples, but the regular $ for
  ## everything else
  let kind = obj.getType().typeKind
  case kind:
    of ntyTuple, ntyObject:
      template toStrAst(obj): string =
        einheit.objToStr(obj)
      result = getAst(toStrAst(obj))
    of ntyString:
      template toStrAst(obj): string =
        "\"" & $(obj) & "\""
      result = getAst(toStrAst(obj))
    else:
      template toStrAst(obj): string =
        $(obj)
      result = getAst(toStrAst(obj))

proc getTestsModule(name: string): TestsModule {.compileTime.} =
  if not testsModuleMap.hasKey(name):
    testsModuleMap[name] = TestsModule(fileName: name)
  return testsModuleMap[name]

proc addTest(testsModule: TestsModule, procDef: NimNode, name: NimNode) =
  testsModule.tests.add(Test(procDef: procDef, name: name))

template strRep(n: NimNode): untyped =
  toString(n)

template tableEntry(n: NimNode): untyped =
  newNimNode(nnkExprColonExpr).add(n.toStrLit(), getAst(strRep(n)))

template recursive(node, action): untyped {.dirty.} =
  ## recursively iterate over AST nodes and perform an
  ## action on them
  proc helper(child: NimNode): NimNode {.gensym.} =
    action
    result = child.copy()
    for c in child.children:
      if child.kind == nnkCall and c.kind == nnkDotExpr:
        # ignore dot expressions that are also calls
        continue
      result.add helper(c)
  discard helper(node)

macro getSyms(code:untyped): untyped =
  ## This macro gets all symbols and values of an expression
  ## into a table
  ##
  ## Table[string, string] -> symbolName, value
  ##
  var
    tableCall = newNimNode(nnkCall).add(ident("toTable"))
    tableConstr = newNimNode(nnkTableConstr)

  recursive(code):
    let ch1 = child
    case ch1.kind:
      of nnkInfix:
        if child[1].kind == nnkIdent:
          tableConstr.add(tableEntry(child[1]))
        if child[2].kind == nnkIdent:
          tableConstr.add(tableEntry(child[2]))
      of nnkExprColonExpr:
        if child[0].kind == nnkIdent:
          tableConstr.add(tableEntry(child[0]))
        if child[1].kind == nnkIdent:
          tableConstr.add(tableEntry(child[1]))
      of nnkCall, nnkCommand:
        tableConstr.add(tableEntry(ch1))
        if ch1.len() > 0 and ch1[0].kind == nnkDotExpr:
          tableConstr.add(tableEntry(ch1[0][0]))
        for i in 1 ..< ch1.len():
          tableConstr.add(tableEntry(ch1[i]))
      of nnkDotExpr:
        tableConstr.add(tableEntry(ch1))
      else:
        discard
  if tableConstr.len() != 0:
    tableCall.add(tableConstr)
    result = tableCall
  else:
    template emptyTable() =
      initTable[string, string]()
    result = getAst(emptyTable())

template check*(code: untyped) =
  ## Assertions for tests
  if not code:
    let
      pos = instantiationInfo(fullpaths=true)
      posRel = instantiationInfo()
      vals = getSyms(code)
      # get ast string with extra spaces ignored
      snip = astToStr(code).strip().split({'\t', '\v', '\c', '\n', '\f'}).join("; ")

    returnException("check", testName, snip, vals, pos, posRel)

template wrapCode(code): untyped =
  # This is needed to prevent an "unreachable code" error
  # if the code block raises an exception
  (proc () = code)()

template checkRaises*(error: untyped, code: untyped): untyped =
  ## Raises a TestAssertError when the exception "error" is
  ## not thrown in the code
  let
    pos = instantiationInfo(fullpaths=true)
    posRel = instantiationInfo()

  try:
    wrapCode(code)
    let
      codeStr = astToStr(code).strip().split().join(" ")
      snip = "$1, $2".format(astToStr(error), codeStr)
      vals = {codeStr: "No Exception Raised"}.toTable()
      testName = testName
    returnException("checkRaises", testName, snip, vals, pos, posRel)

  except error:
    discard
  except TestAssertError:
    raise
  except:
    let
      e = getCurrentException()
      codeStr = astToStr(code).strip().split().join(" ")
      snip = "$1, $2".format(astToStr(error), codeStr)
      vals = {codeStr: $e.name}.toTable()
      testName = testName

    returnException("checkRaises", testName, snip, vals, pos, posRel)


proc printRunning*(testsInfo: TestsInfo) =
  let termSize = getTermSize()
  var
    numTicks = termSize.width
    ticks = ""

  for i in 0..<numTicks:
    ticks &= "-"

  when not defined(quiet):
    when not noColors:
      styledEcho(
        styleBright,
        fgYellow, "\l" & ticks,
        fgYellow, "\l\l[Running]",
        fgWhite, " tests in $1 ".format(testsInfo.fileName)
      )
    else:
      echo "\l$1\l".format(ticks)
      echo "[Running] tests in $1".format(testsInfo.fileName)

proc printPassedTests*(info: TestsInfo) =

  if info.testsPassed == 0 and info.numTests == 0:
    return

  when not noColors:
    # Output red if tests didn't pass, green otherwise
    var color = fgGreen

    if info.testsPassed != info.numTests:
      color = fgRed

  var passedStr = "[" & $info.testsPassed & "/" & $info.numTests & "]"

  when not defined(quiet):
    when not noColors:
      styledEcho(
        styleBright, color, "\l", passedStr,
        fgWhite, " tests passed for ", info.fileName, "."
      )
    else:
      echo "\l$1 tests passed for $2.".format(passedStr, info.fileName)


template runTest(procCall, info, testName) =

  try:
    procCall
    when defined(quiet):
      when noColors:
        stdout.write(".")
      else:
        setForegroundColor(fgGreen)
        writeStyled(".", {styleBright})
        setForegroundColor(fgWhite)
    else:
      var okStr = "[OK]"
      if info.lastTestFailed:
        okStr = "\l" & okStr

      when not noColors:
        styledEcho(styleBright, fgGreen, okStr,
                   fgWhite, "     ", testName)
      else:
        echo "$1     $2".format(okStr, testName)

    info.testsPassed += 1
    info.lastTestFailed = false
  except PrivateTestError as e:
    info.numTests -= 1
    when defined(quiet):
      when noColors:
        stdout.write("N")
      else:
        setForegroundColor(fgBlue)
        writeStyled("N", {styleBright})
        setForegroundColor(fgWhite)
    else:
      when not noColors:
        styledEcho(styleBright,
                   fgBlue, "\l[Not run]",
                   fgWhite, " ", testName)
      else:
        echo "\l[Not run] $1".format(testName)

      let
        name = e.checkFuncName
        snip = e.codeSnip
        line = e.lineNumber
        col = e.column
        filename = e.fileName
        vals = e.valTable

      when not noColors:
        styledEcho(styleDim, fgWhite,
          "  Test code contains private or non accessible symbols:")

        styledEcho(styleDim, fgGreen, "    " & snip)

        styledEcho(
          styleDim, fgWhite,
          "  Location: $1; line $2; col $3".format(filename, line, col))
      else:
        echo "  Test code contains private or non accessible symbols:\l    $1".format(snip)
        echo "  Location: $1; line $2; col: $3".format(filename, line, col)

    info.lastTestFailed = true

  except TestAssertError as e:
    when defined(quiet):
      when noColors:
        stdout.write("F")
      else:
        setForegroundColor(fgRed)
        writeStyled("F", {styleBright})
        setForegroundColor(fgWhite)
    else:
      when not noColors:
        styledEcho(styleBright,
                   fgRed, "\l[Failed]",
                   fgWhite, " ", testName)
      else:
        echo "\l[Failed] $1".format(testName)

      let
        name = e.checkFuncName
        snip = e.codeSnip
        line = e.lineNumber
        col = e.column
        filename = e.fileName
        vals = e.valTable

      when not noColors:
        styledEcho(styleDim, fgWhite, "  Condition: $2($1)".format(snip, name))

        if vals.len > 0:
          styledEcho(styleDim, fgWhite, "  Where:")
          for k, v in vals.pairs:
            styledEcho(styleDim, fgCyan, "    ", k,
                       fgWhite, " -> ",
                       fgGreen, v)
        styledEcho(
          styleDim, fgWhite,
          "  Location: $1; line $2; col $3".format(filename, line, col))
      else:
        echo "  Condition: $2($1)".format(snip, name)
        if vals.len > 0:
          echo "  Where:"
          for k, v in vals.pairs:
            echo "    ", k, " -> ", v

        echo "  Location: $1; line $2; col: $3".format(filename, line, col)

    info.lastTestFailed = true


proc printSummary(totalTestsPassed: int, totalTests: int) =
  when not noColors:
    var summaryColor = fgGreen

    if totalTestsPassed != totalTests:
      summaryColor = fgRed

  var passedStr = "[" & $totalTestsPassed & "/" & $totalTests & "]"

  when defined(quiet):
    when not noColors:
      styledEcho(styleBright, summaryColor,
                 "\l\l", passedStr,
                 fgWhite, " tests passed.")
    else:
      echo "\l\l$1 tests passed.".format(passedStr)
  else:

    let termSize = getTermSize()

    var
      ticks = ""
      numTicks = termSize.width

    for i in 0..<numTicks:
      ticks &= "-"

    when not noColors:
      styledEcho(styleBright,
                 fgYellow, "\l$1\l".format(ticks),
                 fgYellow, "\l[Summary]")

      styledEcho(styleBright, summaryColor,
                  "\l  ", passedStr,
                  fgWhite, " tests passed.")
    else:
      echo "\l$1\l".format(ticks)
      echo "\l[Summary]"
      echo "\l  $1 tests passed.".format(passedStr)

template createRunTests(tests, testsInfo, totalTests, totalTestsPassed) =
  testsInfo.printRunning()

  when defined(quiet):
    echo ""
    when noColors:
      stdout.write(testsInfo.fileName & " ")
    else:
      setForegroundColor(fgWhite)
      writeStyled(testsInfo.fileName & " ", {styleBright})

  tests
  testsInfo.printPassedTests()

  totalTests += testsInfo.numTests
  totalTestsPassed += testsInfo.testsPassed

template makeProc(body, tnameSym, tName, lineInfo, currentDir) =
  when not compiles((proc (tnameSym: string) = body)(tName)):
    (proc (tnameSym: string) =
      let
        #vals = getSyms(body)
        # get ast string with extra spaces ignored
        astBody = astToStr(body).strip().split({'\t', '\v', '\c', '\n', '\f'})
        snip = astBody[1..<astBody.len].join("\l    ")

      var
        filename = lineInfo.fileName.relativePath(currentDir)
        line = lineInfo.line
        col = lineInfo.column

      var message = "\lCode contains private or non accessible symbols."
      message &= "  Location: $1; line $2; col: $3".format(filename, line, col)

      var exc = newException(PrivateTestError, message)
      exc.fileName = filename
      exc.lineNumber = line
      exc.column = col
      exc.codeSnip = snip
      exc.testName = tName
      #exc.valTable = vals
      exc.checkFuncName = tnameSym
      raise exc
    )(tName)
  else:
    (proc (tnameSym: string) = body)(tName)

template createInfo(infoSym, name, ntests, currentDir) =
  let infoSym = TestsInfo(fileName: name.relativePath(currentDir), numTests: ntests)

template createTotalVars(totalTestsPassed, totalTests) =
  var
    totalTests = 0
    totalTestsPassed = 0

template printFinalSummary(totalTestsPassed, totalTests) =
  printSummary(totalTestsPassed, totalTests)

proc expandTests(returnErrorCode: bool, currentDir: string): NimNode =

  result = newNimNode(nnkStmtList)

  let
    totalTestsPassed = genSym(nskVar, "totalPassed")
    totalTests = genSym(nskVar, "totalTests")

  result.add(getAst(createTotalVars(totalTestsPassed, totalTests)))

  for name in testsModuleMap.keys():
    let
      testsModule = testsModuleMap[name]
      infoSym = genSym(nskLet, "testsInfo")
      allTests = newNimNode(nnkStmtList)
      numTests = newLit(testsModule.tests.len())

    result.add(getAst(createInfo(infoSym, name, numTests, currentDir)))

    for test in testsModule.tests:
      let
        body = test.procDef
        tname = genSym(nskParam, "tname")

      body.insert(0,
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            ident("testName"),
            newEmptyNode(),
            tname
          )
        )
      )

      let
        lineInfo = body.lineInfoObj
        testCall = getAst(makeProc(body, tname, test.name, lineInfo, currentDir))
        testRun = getAst(runTest(testCall, infoSym, test.name))

      allTests.add(testRun)

    result.add(getAst(createRunTests(allTests, infoSym, totalTests, totalTestsPassed)))

  result.add(getAst(printFinalSummary(totalTestsPassed, totalTests)))
  if returnErrorCode:
    result.add(quote do: `totalTests` - `totalTestsPassed`)

macro test*(name: string, body: untyped): untyped =
  ## The test macro. No runtime overhead is introduced using it and
  ## it can be run from anywhere, including in the proc being tested.
  let
    fileName = body.lineInfoObj.fileName
    testsInfo = getTestsModule(fileName)
  testsInfo.addTest(body, name)

macro runTests*(returnErrorCode=false): untyped =
  ## This macro must be run in the main testing module that imports
  ## all of the other tests.
  ##
  ## If returnErrorCode is true, the number of failed tests will
  ## be returned.
  let currentDir = getProjectPath()
  return expandTests(returnErrorCode.boolVal, currentDir)
