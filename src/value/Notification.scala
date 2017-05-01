package value

/**
  * Created by jaylantse on 4/25/17.
  */
class Notification(msg: String) extends Value {
  override def toString: String = msg
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val OK = Notification("ok")
  val DONE = Notification("done")
}