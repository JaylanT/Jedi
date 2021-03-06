package expression

import system.TypeException
import value.{Boole, Environment, Notification, Value}

/**
  * Created by Jaylan on 4/30/2017.
  */
case class Conditional(exp1: Expression, exp2: Expression, exp3: Option[Expression]) extends SpecialForm {

  def execute(env: Environment): Value = {
      val condition = exp1.execute(env)
      if (!condition.isInstanceOf[Boole])
        throw new TypeException("Input must be a boole")

      val boole = condition.asInstanceOf[Boole]
      if (boole.value)
        exp2.execute(env)
      else if (exp3.isDefined)
        exp3.get.execute(env)
      else
        Notification("Unspecified")
  }
}
