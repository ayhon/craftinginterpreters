package lox

import Token2._

private val my_keywords = Map[String,Int => Token2](
  "and"    -> And.apply,
  "class"  -> Class.apply,
  "else"   -> Else.apply,
  "false"  -> False.apply,
  "for"    -> For.apply,
  "fun"    -> Fun.apply,
  "if"     -> If.apply,
  "nil"    -> Nil.apply,
  "or"     -> Or.apply,
  "print"  -> Print.apply,
  "return" -> Return.apply,
  "super"  -> Super.apply,
  "this"   -> This.apply,
  "true"   -> True.apply,
  "var"    -> Var.apply,
  "while"  -> While.apply,
)

case class Scanner2(val source: String):
  var tokens = Array[Token2]()

  private var start: Int = 0;
  private var current: Int = 0;
  private var line: Int = 1;

  def scanTokens(): Array[Token2] =
    while !isAtEnd do
      start = current
      scanToken()
    tokens :+= EndOfFile(line)
    tokens

  def isAtEnd: Boolean = current >= source.length

  def scanToken() = advance() match
    case '(' => tokens :+= LeftParen(line)
    case ')' => tokens :+= RightParen(line)
    case '{' => tokens :+= LeftBrace(line)
    case '}' => tokens :+= RightBrace(line)
    case ',' => tokens :+= Comma(line)
    case '.' => tokens :+= Dot(line)
    case '-' => tokens :+= Minus(line)
    case '+' => tokens :+= Plus(line)
    case ';' => tokens :+= Semicolon(line)
    case '*' => tokens :+= Star(line)
    case '?' => tokens :+= Question(line)
    case ':' => tokens :+= Colon(line)
    case '!' => tokens :+= (if matches('=')
                           then BangEqual(line) else Bang(line))
    case '=' => tokens :+= (if matches('=')
                           then EqualEqual(line) else Equal(line))
    case '<' => tokens :+= (if matches('=')
                           then LessEqual(line) else Less(line))
    case '>' => tokens :+= (if matches('=')
                           then GreaterEqual(line) else Greater(line))
    case '/' => 
      if matches('/') then 
        while peek() != '\n' && !isAtEnd do advance()
      else
        tokens :+= Slash(line)
    case ' '|'\r'|'\t' => () // Ignore whitespace
    case '\n' => line += 1;
    case '\"' => string()
    case num if isDigit(num) => number()
    case letter if isAlpha(letter) => identifier()
    case unexpected =>
      Lox.error(line, s"Unexpected character $unexpected")

  def matches(expected: Char) =
    val res = !isAtEnd && peek() == expected
    if res then
      current += 1
    res
    
  def peek() = if isAtEnd then 0.toChar else source.charAt(current)
  def peekNext() = if (current +1 >= source.length) then 0.toChar else source.charAt(current+1)
  def advance() = 
    val character = peek()
    current+=1
    character

  def getText(): String = source.substring(start, current)

  def string() =
    while !isAtEnd && peek() != '\"' do
      if peek() == '\n' then
        line += 1
      advance()

    if isAtEnd then
      Lox.error(line, "Unterminated string")
    else
      advance()
      // Trim surrounding quotes
      val value = source.substring(start+1,current-1)
      tokens :+= String_(getText(), value, line)

  def isDigit(c: Char): Boolean = c.isDigit
  def number() =
    while isDigit(peek()) do advance()
    if peek() == '.' && isDigit(peekNext()) then
      advance()
      while isDigit(peek()) do advance()
    tokens :+= Number_(getText(), getText().toDouble, line)
    

  def isAlpha(c: Char): Boolean = c.isUpper || c.isLower || c == '_'
  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)
  def identifier() =
    while isAlphaNumeric(peek()) do advance()
    my_keywords.get(getText()) match 
      case Some(token) => tokens :+= token(line)
      case None =>        tokens :+= Identifier(getText(), line)
