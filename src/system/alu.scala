package system

import expression.Identifier
import value.{Boole, Number, Value, Variable}

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
      case "less" => less(args)
      case "more" => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case "var" => makeVar(args)
      case "content" => content(args)
      case _ => throw new UndefinedException(operator)
    }
  }

  private def add(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ + _)
  }

  private def mul(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ * _)
  }

  private def sub(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ - _)
  }

  private def div(vals: List[Value]): Value = {
    castAsNumbers(vals, "add").reduce(_ / _)
  }

  private def less(vals: List[Value]): Value = {
    val args = castAsNumbers(vals, "less")
    if (args.length != 2) throw new TypeException("less inputs must be numbers")
    if (args.head.value < args(1).value) Boole(true) else Boole(false)
  }

  private def more(vals: List[Value]): Value = {
    val args = castAsNumbers(vals, "more")
    if (args.length != 2) throw new TypeException("more inputs must be numbers")
    if (args.head.value > args(1).value) Boole(true) else Boole(false)
  }

  private def equals(vals: List[Value]): Boole = {
    if (vals.isEmpty) throw new TypeException("equals expected > 0 inputs")
    var more = true
    var result = true
    for(i <- 1 until vals.length if more) {
      {
        if (vals(i) != vals.head) result = false
        more = false
      }
    }
    Boole(result)
  }

  private def unequals(vals: List[Value]): Value = {
    if (vals.length != 2) throw new TypeException("unequals expected 2 inputs")
    if (vals.head != vals(1)) Boole(true) else Boole(false)
  }

  private def content(args: List[Value]): Value = {
    args.head.asInstanceOf[Variable].content
  }

  private def makeVar(args: List[Value]): Variable = {
    new Variable(args.head)
  }

  private def castAsNumbers(vals: List[Value], opcode: String): List[Number] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Number])
  }
}
