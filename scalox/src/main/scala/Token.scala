package lox

case class Token( kind: TokenType,
                  lexeme: String,
                  literal: Any,
                  line: Int
                ):
  override def toString: String = s"$kind $lexeme $literal"

