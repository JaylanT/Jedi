package expression

import value.{Environment, Value}

/**
  * Created by jaylantse on 4/20/17.
  */
trait Literal extends Expression with Value {
  override def execute(env: Environment): Value = this
}
