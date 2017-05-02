package expression
import value.{Environment, Notification, Value}

/**
  * Created by jaylantse on 5/2/17.
  */
case class Block(locals: List[Expression]) extends SpecialForm {

  override def execute(env: Environment): Value = {
    env.extension = new Environment

    locals.foreach(_.execute(env.extension))
    env.extension = null
    Notification("Done")
  }
}
