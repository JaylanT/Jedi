package system

import expression._

/**
  * Created by jaylantse on 5/4/17.
  */
class SithParsers extends WookieParsers {

  override def expression: Parser[Expression] = declaration | conditional | iteration | assignment | disjunction | failure("Invalid expression")

  override def term: Parser[Expression] = deref | lambda | block | funCall | literal | "(" ~> expression <~ ")"

  def iteration: Parser[Iteration] = "while" ~ "(" ~> expression ~ ")" ~ expression ^^ {
    case e1 ~ ")" ~ e2 => Iteration(e1, e2)
  }

  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case i ~ "=" ~ e => Assignment(i, e)
  }

  def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^ (e => FunCall(Identifier("content"), e :: Nil))
}
