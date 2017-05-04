package expression
import system.TypeException
import value.{Boole, Environment, Value}

/**
  * Created by jaylantse on 5/4/17.
  */
case class Iteration(cond: Expression, body: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    def helper(result: Value): Value = {
      val condResult = cond.execute(env)
      if (!condResult.isInstanceOf[Boole])
        throw new TypeException("Condition is not a boole")

      val boole = condResult.asInstanceOf[Boole]

      if (!boole.value) result
      else {
        val bodyResult = body.execute(env)
        helper(bodyResult)
      }
    }
    helper(null)
  }
}
