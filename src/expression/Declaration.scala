package expression

import value.{Environment, Notification, Value}

/**
  * Created by jaylantse on 4/25/17.
  */
case class Declaration(name: Identifier, body: Expression) extends SpecialForm {

  def execute(env: Environment): Value = {
    val result = body.execute(env)
    env.put(name, result)
    Notification.DONE
  }
}

object Declaration