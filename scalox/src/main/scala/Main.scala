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
    
    var line = reader.readLine()
    while line != null do
      run(line)
      Lox.hadError = false

  def run(source: String) =
    val scanner: Scanner = new Scanner(source)
    val tokens: Array[Token] = scanner.scanTokens()

    for token <- tokens do
      println(token)

  def error(line: Int, message: String) =
    report(line, "", message)
  
  def report(line: Int, where: String, what: String) =
    println(s"[ line $line ] Error${where}: $what")
    Lox.hadError = true
