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
  def expression(): Expr = operation(Precedence.top)
  // def expression(): Expr = equality()

  /*
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
  // */

  // /*
  // Idea: Abstract away the precedence of an operator, as a property of the operator
  enum Precedence:
    case SeqPoint
    case TertiaryConditional
    case Equality
    case Comparison
    case Term
    case Factor
    case Unary
    case Primary

    def lower: Precedence = 
      val lower_ordinal = this.ordinal + 1
      val max_ordinal = Precedence.values.length
      Precedence.fromOrdinal(lower_ordinal min max_ordinal)

  object Precedence:
    val top = Precedence.fromOrdinal(0)

  def precedence(token: Token2): Precedence = token match
    case Comma(_) => Precedence.SeqPoint
    case Question(_) => Precedence.TertiaryConditional
    case BangEqual(_) | EqualEqual(_) => Precedence.Equality
    case Greater(_) |GreaterEqual(_)
         |Less(_) |LessEqual(_) => Precedence.Comparison
    case Plus(_) | Minus(_) => Precedence.Term
    case Slash(_) | Star(_) => Precedence.Factor
    case _ => Precedence.Unary

  def operation(prec: Precedence): Expr = prec match
    case Precedence.TertiaryConditional => tertiary(prec)
    case Precedence.Unary => unary()
    case Precedence.Primary => primary()
    case _ => binary(prec)

  // To decouple this implementation from the tertiary conditional operator, 
  // we should have a function that matches the first part of a tertiary operator
  // with the possible continuations
  def tertiary(prec: Precedence): Expr = // equality ( '?' equality ':' expresion )
    val condition = operation(prec.lower)
    peek() match
      case Question(_) =>
        advance()
        val left = operation(prec.lower)
        peek() match
          case Colon(_) => advance()
          case _ => throw Parser.error(peek(), "Expect ':' in tertiary operator")
        val right = operation(prec.lower)
        Tertiary(condition,left,right)
      case _ => condition

  def binary(prec: Precedence): Expr = // binary(n) = operation(n-1) ( token_with_prec(n) operation(n-1) )*
    @tailrec def continued(left: Expr): Expr =
      if precedence(peek()) == prec then
        val operator = advance()
        val right = operation(prec.lower)
        continued(Binary(left, operator, right))
      else left
    continued(operation(prec.lower))
  // */

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
