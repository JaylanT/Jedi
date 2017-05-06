package system

import value.Environment

/**
  * Created by jaylantse on 4/20/17.
  */
object console {

  val parsers = new SithParsers
  val globalEnv = new Environment
  var verbose = true

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case tree: parsers.Failure => throw new SyntaxException(tree)
      case _ =>
        val exp = tree.get  // get the expression from the tree
        val result = exp.execute(globalEnv)  // execute the expression
        result.toString  // return string representation of result
    }
  }

  def repl() {
    var more = true
    var cmmd = ""
    while(more) {
      try {
        print("-> ")
        cmmd = scala.io.StdIn.readLine.toString
        if (cmmd == "quit") more = false
        else println(execute(cmmd))
      }
      catch {
        case e: SyntaxException =>
          println(e)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        case e: UndefinedException =>
          println(e)
          if (verbose) e.printStackTrace()
        case e: UndefinedException =>
          println(e)
          if (verbose) e.printStackTrace()
        case e: JediException =>
          println(e)
          if (verbose) e.printStackTrace()
        case e: Exception =>
          println(e)
          more = false
      } finally {
        Console.flush
      }
    }
    println("bye")
  }

  def main(args: Array[String]): Unit = { repl() }
}
