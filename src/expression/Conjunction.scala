package expression

import system.TypeException
import value.{Boole, Environment, Value}

/**
  * Created by jaylantse on 4/25/17.
  */
case class Conjunction(exps: List[Expression]) extends SpecialForm {

  override def execute(env: Environment): Value = {
    def helper(result: Boole, c: Int): Boole = {
      if (!result.value || c == exps.length) result
      else {
        val result = exps(c).execute(env)
        if (!result.isInstanceOf[Boole])
          throw new TypeException("Inputs must be booles")

        helper(result.asInstanceOf[Boole], c + 1)
      }
    }
    helper(Boole(true), 0)
  }
}
