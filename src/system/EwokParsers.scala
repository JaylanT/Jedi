package system

/**
  * Created by jaylantse on 4/20/17.
  */

import expression._
import value.{Boole, Number}

import scala.util.parsing.combinator._

class EwokParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (name => Identifier(name))

  def term: Parser[Expression] = literal | identifier | "(" ~> expression <~ ")"

  def literal: Parser[Literal] = boole | number

  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ (number => Number(number.toDouble))

  def boole: Parser[Boole] = ("true" | "false") ^^ (boole => Boole(boole.toBoolean))

  def declaration: Parser[Declaration] = "def" ~> identifier ~ "=" ~ expression ^^ {
    case name ~ "=" ~ body => Declaration(name, body)
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^ {
    case ie ~ Nil => ie
    case ie ~ rest => FunCall(Identifier("equals"), ie :: rest)
  }

  def inequality: Parser[Expression] = sum ~ opt("<" ~> sum) ^^ {
    case s ~ None => s
    case s ~ Some(rest) => FunCall(Identifier("less"), List(s, rest))
  }

  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
    case p ~ Nil => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)
  }

  def product: Parser[Expression] = funCall ~ rep(("*" | "/") ~ funCall ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^ {
    case fc ~ Nil => fc
    case fc ~ rest => FunCall(Identifier("mul"), fc :: rest)
  }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case e ~ Nil => e
    case e ~ rest => Conjunction(e :: rest)
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ cons => Disjunction(con :: cons)
  }

  def conditional: Parser[Expression] = "if" ~ "(" ~> expression ~ ")" ~ expression ~ opt("else" ~> expression) ^^ {
    case e1 ~ ")" ~ e2 ~ None => Conditional(e1, e2, None)
    case e1 ~ ")" ~ e2 ~ Some(e3) => Conditional(e1, e2, Some(e3))
  }

  //  using identifier rather than term matches def body to identifier
  def funCall: Parser[Expression] = term ~ opt(operands) ^^ {
    case t ~ None => t
    case t ~ Some(ops) => FunCall(t, ops)
  }

  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => e :: Nil
    case Some(e ~ rest) => e :: rest
  }

  private def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  private def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }
}
