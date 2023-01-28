package lox

import java.io.{BufferedReader,InputStreamReader};
import java.nio.charset.Charset;
import java.nio.file.{Files, Paths};

object Lox:
  var hadError: Boolean = false
  @main def main(args: String*): Unit = 
    if args.length > 1 then
      println("Usage: scalox [script]")
      sys.exit(64)
    else if args.length == 1 then
      runFile(args(0))
    else
      runPrompt

  def runFile(filename: String): Unit =
    val bytes = Files.readAllBytes(Paths.get(filename))
    run(new String(bytes, Charset.defaultCharset))
    if(Lox.hadError) sys.exit(65)

  def runPrompt =
    val input: InputStreamReader = new InputStreamReader(System.in)
    val reader: BufferedReader = new BufferedReader(input)
    
    print("> ")
    var line = reader.readLine()
    while line != null do
      run(line)
      Lox.hadError = false
      print("> ")
      line = reader.readLine()

  def run(source: String) =
    val scanner = Scanner2(source)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    parser.parse() match
      case Some(expression) => println(expression)
      case None => ()

  def report(line: Int, where: String, what: String) =
    println(s"[ line $line ] Error${where}: $what")
    Lox.hadError = true

  def error(line: Int, message: String) =
    report(line, "", message)

  def error(token: Token2, message: String) = 
    val where = token match
      case Token2.EndOfFile(_) => " at end"
      case _ => s" at '${token.lexeme}'"
    report(token.line, where, message)
