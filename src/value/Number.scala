package value

import expression.Literal

/**
  * Created by jaylantse on 4/20/17.
  */
case class Number(value: Double) extends Literal {
  def +(other: Number) = Number(this.value + other.value)
  def *(other: Number) = Number(this.value * other.value)
  def -(other: Number) = Number(this.value - other.value)
  def /(other: Number) = Number(this.value / other.value)
  def <(other: Number) = Boole(this.value < other.value)
  def ==(other: Number) = Boole(this.value == other.value)
  override def toString: String = value.toString
}
