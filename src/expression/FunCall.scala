package expression
import system.alu
import value.{Closure, Environment, Value}


/**
  * Created by jaylantse on 4/20/17.
  */
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {

  override def execute(env: Environment): Value = {
    val args: List[Value] = operands.map(_.execute(env))
    if (env.contains(operator)) {
      val fun = env(operator)
      fun match {
        case closure: Closure => closure.apply(args)
        case _ => alu.execute(operator, args)
      }
    } else {
      alu.execute(operator, args)
    }
  }
}
