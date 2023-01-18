package lox

case class Token( kind: TokenType,
                  lexeme: String,
                  literal: Any,
                  line: Int
                ):
  def useless = 1
  override def toString: String = s"$kind $lexeme $literal"


