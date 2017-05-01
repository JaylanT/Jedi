package expression

import value.{Environment, Value}

/**
  * Created by jaylantse on 4/20/17.
  */
trait Expression {

  def execute(env: Environment): Value
}
