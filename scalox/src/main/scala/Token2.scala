package lox

enum Token2(val lexeme: String, val literal: Any, val line: Int):
  override def toString: String = s"$lexeme\t${this.productPrefix}\t${if literal == null then "" else literal}"

  // Single character tokens
  case LeftParen(ln: Int)                       extends Token2("(", null, ln)
  case RightParen(ln: Int)                      extends Token2(")", null, ln)
  case LeftBrace(ln: Int)                       extends Token2("{", null, ln)
  case RightBrace(ln: Int)                      extends Token2("}", null, ln)
  case Comma(ln: Int)                           extends Token2(",", null ,ln)
  case Dot(ln: Int)                             extends Token2(".", null ,ln)
  case Minus(ln: Int)                           extends Token2("-", null ,ln)
  case Plus(ln: Int)                            extends Token2("+", null ,ln)
  case Semicolon(ln: Int)                       extends Token2(";", null ,ln)
  case Slash(ln: Int)                           extends Token2("/", null ,ln)
  case Star(ln: Int)                            extends Token2("*", null ,ln)
 
  // One or two character tokens
  case Bang(ln: Int)                            extends Token2("!",  null ,ln)
  case BangEqual(ln: Int)                       extends Token2("!=", null ,ln)
  case Equal(ln: Int)                           extends Token2("=",  null ,ln)
  case EqualEqual(ln: Int)                      extends Token2("==", null ,ln)
  case Greater(ln: Int)                         extends Token2(">",  null ,ln)
  case GreaterEqual(ln: Int)                    extends Token2(">=", null ,ln)
  case Less(ln: Int)                            extends Token2("<",  null ,ln)
  case LessEqual(ln: Int)                       extends Token2("<=", null ,ln)

  // ltrls
  case Identifier(lxm: String, ln: Int)         extends Token2(lxm, null ,ln)
  case String_(lxm: String, ltrl: Any, ln: Int) extends Token2(lxm, ltrl ,ln)
  case Number_(lxm: String, ltrl: Any, ln: Int) extends Token2(lxm, ltrl ,ln)

  // Keywords
  case And(ln: Int)                             extends Token2("and", null, ln)
  case Class(ln: Int)                           extends Token2("class", null, ln)
  case Else(ln: Int)                            extends Token2("else", null, ln)
  case False(ln: Int)                           extends Token2("false", false, ln)
  case Fun(ln: Int)                             extends Token2("fun", null, ln)
  case For(ln: Int)                             extends Token2("for", null, ln)
  case If(ln: Int)                              extends Token2("if", null, ln)
  case Nil(ln: Int)                             extends Token2("nil", null, ln)
  case Or(ln: Int)                              extends Token2("or", null, ln)
  case Print(ln: Int)                           extends Token2("print", null, ln)
  case Return(ln: Int)                          extends Token2("return", null, ln)
  case Super(ln: Int)                           extends Token2("super", null, ln)
  case This(ln: Int)                            extends Token2("this", null, ln)
  case True(ln: Int)                            extends Token2("true", true, ln)
  case Var(ln: Int)                             extends Token2("var", null, ln)
  case While(ln: Int)                           extends Token2("while", null, ln)
  

  case EndOfFile(ln: Int)                       extends Token2("", null, ln)
