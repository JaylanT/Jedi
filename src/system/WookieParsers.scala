package system

import expression._

/**
  * Created by jaylantse on 5/2/17.
  */
class WookieParsers extends EwokParsers {

  override def term: Parser[Expression] = lambda | block | funCall | literal | "(" ~> expression <~ ")"

  def lambda: Parser[Lambda] = "lambda" ~> parameters ~ expression ^^ {
    case p ~ e => Lambda(p, e)
  }

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }

  def block: Parser[Block] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case e ~ Nil => Block(e :: Nil)
    case e ~ rest => Block(e :: rest)
  }
}
