package expression
import system.{TypeException, UndefinedException, alu}
import value.{Closure, Environment, Value}


/**
  * Created by jaylantse on 4/20/17.
  */
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {

  override def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env))
    try {
      operator.execute(env).asInstanceOf[Closure].apply(args)
    } catch {
      case _: UndefinedException =>
        operator match {
          case op: Identifier => alu.execute(op, args)
          case op => throw new TypeException(op + " is not an identifier")
        }
      case e: Exception => throw e
    }
  }
}
