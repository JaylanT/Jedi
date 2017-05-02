package value

import expression.{Expression, Identifier}
import system.TypeException

/**
  * Created by jaylantse on 4/20/17.
  */
class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {

  def apply(args: List[Value]): Value = {
    if (args.length != params.length)
      throw new TypeException("Missing arguments")

    val tempEnvironment = new Environment
    for (i <- params.length)
      tempEnvironment.put(params(i), args(i))
    defEnv.extension = tempEnvironment

    val result = body.execute(defEnv)
    defEnv.extension = null

    result
  }
}

object Closure {
  def apply(params: List[Identifier], body: Expression, defEnv: Environment) = new Closure(params, body, defEnv)
}
