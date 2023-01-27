package lox

enum Expr:
  case Binary(left: Expr, operator: Token2, right: Expr)
  case Grouping(expression: Expr)
  case Literal(value: Any)
  case Unary(operator: Token2, right: Expr)

  override def toString = this match
    case Binary(left, op, right) => s"(${op.lexeme} $left $right)"
    case Grouping(expr) => s"(group $expr)"
    case Literal(value) => if value == null then "nil" else value.toString
    case Unary(op, right) => s"(${op.lexeme} $right)"

  def rpn: String = this match // Reverse polish notation
    case Binary(left, op, right) => s"${left.rpn} ${right.rpn} ${op.lexeme}"
    case Grouping(expr) => expr.rpn
    case Literal(value) => if value == null then "nil" else value.toString
    case Unary(op, right) => s"${right.rpn} ${op.lexeme}"

@main def test_expr(args: String*) =
  import Expr._
  val expr = Binary(
    Unary(Token2.Minus(1), Literal(123)),
    Token2.Star(1),
    Grouping(
      Literal(45.67)
    )
  )
  println(expr.rpn)

/*
 * The opposite of a Visitor pattern would
 * be a "performer" pattern in a functional
 * programming language. For every method
 * we declare a type, and for every class,
 * we implement a function which takes a
 * method and performs some action
 * 
 * Given
 * 
 *     class Dog:
 *       eat()
 *       walk()
 *     class Cat
 *       eat()
 *       walk()
 * 
 * We can transform this into
 * 
 *     type Method = Eat | Walk
 *     
 *     type Dog
 *     performDog :: Method -> ()
 *     performDog Eat = ...
 *     performDog Walk = ...
 *     
 *     type Cat
 *     performCat :: Method -> ()
 *     performCat Eat = ...
 *     performCat Walk = ...
 * 
 */
