package expression
import system.TypeException
import value.{Environment, Notification, Value, Variable}

/**
  * Created by jaylantse on 5/4/17.
  */
case class Assignment(variable: Identifier, update: Expression) extends SpecialForm {

  override def execute(env: Environment): Value = {
    val theVar = variable.execute(env)
    if (!theVar.isInstanceOf[Variable])
      throw new TypeException("Not a variable")

    theVar.asInstanceOf[Variable].content = update.execute(env)
    Notification.OK
  }
}
