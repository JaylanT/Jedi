package expression
import system.alu
import value.{Environment, Value}


/**
  * Created by jaylantse on 4/20/17.
  */
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {

  override def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}
