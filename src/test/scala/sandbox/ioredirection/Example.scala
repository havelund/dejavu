package sandbox.ioredirection

import java.io._

object Example {
  def main(args: Array[String]): Unit = {
    println("to console")

    scala.Console.withOut(new PrintStream(new File("bdd.dot"))) {
      println("this goes to file")
    }

    println("to console again")
  }
}
