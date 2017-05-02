package expression
import value.{Closure, Environment}

/**
  * Created by jaylantse on 5/2/17.
  */
case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {

  override def execute(env: Environment): Closure = Closure(params, body, env)
}
