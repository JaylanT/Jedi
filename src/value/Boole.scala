package value

import expression.Literal

/**
  * Created by jaylantse on 4/20/17.
  */
case class Boole(value: Boolean) extends Literal {
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)
  override def toString: String = this.value.toString
}
