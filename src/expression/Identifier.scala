package expression
import value.{Environment, Value}

/**
  * Created by jaylantse on 4/20/17.
  */
case class Identifier(name: String) extends Expression {
  override def execute(env: Environment): Value = env(this)
}
