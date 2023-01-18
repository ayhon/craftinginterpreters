package lox

import TokenType._

private val keywords = Map[String,TokenType](
  "and" -> AND,
  "class" -> AND,
  "else" -> AND,
  "false" -> AND,
  "for" -> AND,
  "fun" -> AND,
  "if" -> AND,
  "nil" -> AND,
  "or" -> AND,
  "print" -> AND,
  "return" -> AND,
  "super" -> AND,
  "this" -> AND,
  "true" -> AND,
  "var" -> AND,
  "while" -> AND,
)

case class Scanner(val source: String):
  var tokens = Array[Token]()

  private var start: Int = 0;
  private var current: Int = 0;
  private var line: Int = 1;

  def scanTokens(): Array[Token] =
    while !isAtEnd do
      start = current
      scanToken()
    tokens :+= Token(EOF, "", null, line)
    tokens

  def isAtEnd: Boolean = current >= source.length

  def scanToken() = advance() match
    case '(' => addToken(LEFT_PAREN)
    case ')' => addToken(RIGHT_PAREN)
    case '{' => addToken(LEFT_BRACE)
    case '}' => addToken(RIGHT_BRACE)
    case ',' => addToken(COMMA)
    case '.' => addToken(DOT)
    case '-' => addToken(MINUS)
    case '+' => addToken(PLUS)
    case ';' => addToken(SEMICOLON)
    case '*' => addToken(STAR)
    case '!' => addToken(if matches('=')
                         then BANG_EQUAL else BANG)
    case '=' => addToken(if matches('=')
                         then EQUAL_EQUAL else EQUAL)
    case '<' => addToken(if matches('=')
                         then LESS_EQUAL else LESS)
    case '>' => addToken(if matches('=')
                         then GREATER_EQUAL else GREATER)
    case '/' => 
      if matches('/') then 
        while peek() != '\n' && !isAtEnd do advance()
      else
        addToken(SLASH)
    case '\r'|'\t' => ()
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
    current+=1
    peek()

  def addToken(kind: TokenType, literal: Any = null) =
    val text = source.substring(start, current)
    tokens :+= Token(kind, text, literal, line)

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
      addToken(STRING, value)

  def isDigit(c: Char): Boolean = c.isDigit
  def number() =
    while isDigit(peek()) do advance()
    if peek() == '.' && isDigit(peekNext()) then
      advance()
      while isDigit(peek()) do advance()
    addToken(NUMBER, source.substring(start, current).toDouble)
    

  def isAlpha(c: Char): Boolean = c.isUpper || c.isLower || c == '_'
  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)
  def identifier() =
    while isAlphaNumeric(peek()) do advance()
    val text = source.substring(start, current)
    val kind = keywords.getOrElse(text, IDENTIFIER)
    addToken(kind)
