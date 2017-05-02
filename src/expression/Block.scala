package expression
import value.{Environment, Notification, Value}

/**
  * Created by jaylantse on 5/2/17.
  */
case class Block(locals: List[Expression]) extends SpecialForm {

  override def execute(env: Environment): Value = {
    val tempEnvironment = new Environment
    tempEnvironment.extension = env

    locals.foreach(_.execute(tempEnvironment))
    Notification("Done")
  }
}
