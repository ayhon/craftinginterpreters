import lox._
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("scanner gives tokens for small code snippet") {
    val source = """
var x = 100;
var y = 200;
for(var i = 0; i < 10; i = i + 1) {
  func add(a,b) { return a + b; }
  print add((x+i)/(y-i));
  print "And done!";
}"""
    val expected = Array(
      "var\tVar",
      "x\tIdentifier",
      "=\tEqual",
      "100\tNumber_\t100.0",
      ";\tSemicolon",
      "var\tVar",
      "y\tIdentifier",
      "=\tEqual",
      "200\tNumber_\t200.0",
      ";\tSemicolon",
      "for\tFor",
      "(\tLeftParen",
      "var\tVar",
      "i\tIdentifier",
      "=\tEqual",
      "0\tNumber_\t0.0",
      ";\tSemicolon",
      "i\tIdentifier",
      "<\tLess",
      "10\tNumber_\t10.0",
      ";\tSemicolon",
      "i\tIdentifier",
      "=\tEqual",
      "i\tIdentifier",
      "+\tPlus",
      "1\tNumber_\t1.0",
      ")\tRightParen",
      "{\tLeftBrace",
      "func\tIdentifier",
      "add\tIdentifier",
      "(\tLeftParen",
      "a\tIdentifier",
      ",\tComma",
      "b\tIdentifier",
      ")\tRightParen",
      "{\tLeftBrace",
      "return\tReturn",
      "a\tIdentifier",
      "+\tPlus",
      "b\tIdentifier",
      ";\tSemicolon",
      "}\tRightBrace",
      "print\tPrint",
      "add\tIdentifier",
      "(\tLeftParen",
      "(\tLeftParen",
      "x\tIdentifier",
      "+\tPlus",
      "i\tIdentifier",
      ")\tRightParen",
      "/\tSlash",
      "(\tLeftParen",
      "y\tIdentifier",
      "-\tMinus",
      "i\tIdentifier",
      ")\tRightParen",
      ")\tRightParen",
      ";\tSemicolon",
      "print\tPrint",
      "\"And done!\"\tString_\tAnd done!",
      ";\tSemicolon",
      "}\tRightBrace",
      "\tEndOfFile"
    )
    val scanner: Scanner2 = new Scanner2(source)
    val tokens: Array[Token2] = scanner.scanTokens()

    // println(source)
    var s = scala.collection.mutable.StringBuilder()
    for (token, expect) <- tokens.zip(expected) do
      assertNoDiff(token.toString, expect.toString)
  }
}
