package system

import expression.Identifier
import value.{Boole, Number, Value}

/**
  * Created by jaylantse on 4/27/17.
  */

object alu {
  // dispatcher
  def execute(operator: Identifier, args: List[Value]): Value = {
    operator.name match {
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => less(args)
      case _ => throw new UndefinedException(operator)
    }
  }

  private def add(args: List[Value]): Number = {
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to add must be numbers")
    nums.map(_.asInstanceOf[Number]).reduce(_+_)
  }

  private def sub(args: List[Value]): Number = {
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to sub must be numbers")
    nums.map(_.asInstanceOf[Number]).reduce(_-_)
  }

  private def mul(args: List[Value]): Number = {
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to mul must be numbers")
    nums.map(_.asInstanceOf[Number]).reduce(_*_)
  }

  private def div(args: List[Value]): Number = {
    val nums = args.filter(_.isInstanceOf[Number])
    if (nums.length != args.length)
      throw new TypeException("Inputs to div must be numbers")
    nums.map(_.asInstanceOf[Number]).reduce(_/_)
  }

  private def equals(args: List[Value]): Boole = {
    @scala.annotation.tailrec
    def helper(result: Boole, c: Int): Boole = {
      if (!result.value || c + 1 >= args.length) result
      else {
        val arg1 = args(c)
        val arg2 = args(c + 1)
        if (!arg1.isInstanceOf[Number] || !arg2.isInstanceOf[Number])
          throw new TypeException("Inputs to equals need to be numbers")

        val isEqual = arg1.asInstanceOf[Number] == arg2.asInstanceOf[Number]
        helper(isEqual, c + 1)
      }
    }
    helper(Boole(true), 0)
  }

  private def less(args: List[Value]): Boole = {
    @scala.annotation.tailrec
    def helper(result: Boole, c: Int): Boole = {
      if (!result.value || c + 1 >= args.length) result
      else {
        val arg1 = args(c)
        val arg2 = args(c + 1)
        if (!arg1.isInstanceOf[Number] || !arg2.isInstanceOf[Number])
          throw new TypeException("Inputs to less need to be numbers")

        val isLess = arg1.asInstanceOf[Number] < arg2.asInstanceOf[Number]
        helper(isLess, c + 1)
      }
    }
    helper(Boole(true), 0)
  }
}
