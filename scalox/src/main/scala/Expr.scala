package lox

enum Expr:
  case Tertiary(condition: Expr, left: Expr, right: Expr)
  case Binary(left: Expr, operator: Token2, right: Expr)
  case Grouping(expression: Expr)
  case Literal(value: Any)
  case Unary(operator: Token2, right: Expr)

  override def toString = this match
    case Tertiary(cond, left, right) => s"(?: $cond $left $right)"
    case Binary(left, op, right) => s"(${op.lexeme} $left $right)"
    case Grouping(expr) => s"(group $expr)"
    case Literal(value) => if value == null then "nil" else value.toString
    case Unary(op, right) => s"(${op.lexeme} $right)"


