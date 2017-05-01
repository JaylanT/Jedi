package system

import expression.Identifier

import scala.util.parsing.combinator.Parsers

/**
  * Created by jaylantse on 4/20/17.
  */
class JediException(val gripe: String = "Jedi exception") extends Exception(gripe)

class UndefinedException(name: Identifier) extends JediException("Undefined identifier: " + name)

class TypeException(gripe: String = "Type Error") extends JediException(gripe)

class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error")
