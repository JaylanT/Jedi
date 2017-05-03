package value

import expression.{Expression, Identifier}
import system.TypeException

/**
  * Created by jaylantse on 4/20/17.
  */
class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {

  def apply(args: List[Value]): Value = {
    if (args.length != params.length)
      throw new TypeException("Missing/extra arguments")

    val tempEnvironment = new Environment
    tempEnvironment.extension = defEnv

    (params, args).zipped.foreach{(i, v) => tempEnvironment.put(i, v)}

    body.execute(tempEnvironment)
  }
}

object Closure {
  def apply(params: List[Identifier], body: Expression, defEnv: Environment) = new Closure(params, body, defEnv)
}
