package lox

import Token2._
import Expr._
import scala.annotation.tailrec

class Parser(private val tokens: Array[Token2]):
  var current: Int = 0

  def parse(): Option[Expr] =
    try
      Some(expression())
    catch
      case e: Parser.ParseError => None

  // Helper methods
  def peek(): Token2 = tokens(current)
  def previous(): Token2 = tokens(current-1)
  def isAtEnd: Boolean = peek() match
    case EndOfFile(_) => true
    case _ => false
  def advance(): Token2 = 
    if !isAtEnd then current += 1
    previous()

  // Parsing states
  def expression(): Expr = equality()

  def equality(): Expr = // comparison ( ("!=" | "==") comparison )*
    @tailrec def continued(left: Expr): Expr = peek() match
        case BangEqual(_) | EqualEqual(_) =>
          val operator = advance()
          val right = comparison()
          continued(Binary(left, operator, right))
        case _ => left
    continued(comparison())

  def comparison(): Expr = // term ( (">" | "<" | ">=" | "<=") term)*
    @tailrec def continued(left: Expr): Expr = peek() match
      case Greater(_)
          |GreaterEqual(_)
          |Less(_)
          |LessEqual(_) =>
            val operator = advance()
            val right = term()
            continued(Binary(left, operator, right))
      case _ => left
    continued(term())

  def term(): Expr = // factor ( ("+" | "-") factor)*
    @tailrec def continued(left: Expr): Expr = peek() match
      case Plus(_) | Minus(_) =>
        val operator = advance()
        val right = factor()
        continued(Binary(left, operator, right))
      case _ => left
    continued(factor())

  def factor(): Expr = // unary ( ("/" | "*") unary)*
    @tailrec def continued(left: Expr): Expr = peek() match
      case Slash(_) | Star(_) =>
        val operator = advance()
        val right = unary()
        continued(Binary(left, operator, right))
      case _ => left
    continued(unary())

  /*
  // Idea: Abstract away the fixity of an operator, as a property of the operator
  def fixity(token: Token2): Int = ???
  def binop(fix: Int): Expr = // binop(N) = binop(N-1) ( tokens_with_fix(N) binop(N-1) )*
    @tailrec def continued(left: Expr): Expr = fixity(peek()) match
      case fix => 
        val operator = advance()
        val right = binop(fix-1)
        continued(Binary(left, operator, right))
      case _ => left
    if fix == 0 then
      primary()
    else continued(binop(fix-1))
  */

  def unary(): Expr = peek() match // ("!" | "-") unary | primary
    case Bang(_) | Minus(_) =>
      val operator = advance()
      val right = unary()
      return Unary(operator, right)
    case _ => primary()

  def primary(): Expr = peek() match
    case False(_)
         |True(_)
         |Nil(_)
         |Number_(_,_,_)
         |String_(_,_,_) =>
           val primary = advance()
           Literal(primary.literal)
    case LeftParen(_) =>
      advance()
      val expr = expression()
      peek() match
        case RightParen(_) => advance()
        case _ => throw Parser.error(peek(), "Expect ')' after expression")
      Grouping(expr)
    case _ => throw Parser.error(peek(), "Expect expression")

  // Error recovery
  def syncronize(): Unit =
    advance()
    while !isAtEnd do
      peek() match
        case Semicolon(_) =>
          advance()
          return
        case Class(_)
             |Fun(_)
             |Var(_)
             |For(_)
             |If(_)
             |While(_)
             |Print(_)
             |Return(_) => return
        case _ => advance()


object Parser:
  class ParseError extends java.lang.RuntimeException
  def error(token: Token2, message: String) =
    Lox.error(token, message)
    Parser.ParseError()
