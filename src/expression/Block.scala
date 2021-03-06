package expression
import value.{Environment, Value}

/**
  * Created by jaylantse on 5/2/17.
  */
case class Block(locals: List[Expression]) extends SpecialForm {

  override def execute(env: Environment): Value = {
    val localEnv = new Environment(env)

    locals.map(_.execute(localEnv)).last
  }
}
