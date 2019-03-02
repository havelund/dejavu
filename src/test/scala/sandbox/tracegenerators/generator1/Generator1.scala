package sandbox.tracegenerators.generator1

import java.io.{File, PrintWriter}

object Util {
  var pw1: PrintWriter = null
  var pw2: PrintWriter = null
  var count : Int = 0

  val dir1 = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/sandbox/garbagecollection1/"
  val dir2 = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/sandbox/garbagecollection2/"

  def openFile(nr: Int): Unit = {
    assert(nr > 3)
    val file = s"log$nr.csv"
    pw1 = new PrintWriter(new File(dir1 + file))
    pw2 = new PrintWriter(new File(dir2 + file))
  }

  def closeFile(): Unit = {
    pw1.close()
    pw2.close()
    println(s"count = $count")
  }

  def writeln(x: Any) = {
    count += 1
    pw1.write(x + "\n")
    pw2.write(x + "\n")
    println(x)
  }
}

import Util._

/**
  * Generating traces for the property:
  *
  *   property p: forall y . forall z . (r(y,z) -> (!q(y) S p(z)))
  *
  * which is monitored in packages sandbox.garbagecollection{1,2}.
  */

object Generator1 {
  def main(args: Array[String]) = {
    openFile(4)
    var nextQ = 1
    val P = 1000
    val Q = 1000

    for (p <- 1 to P) {
      writeln(s"p,$p")
      for (q <- nextQ to nextQ + Q) {
        writeln(s"q,$q")
      }
      nextQ += Q + 1
      writeln(s"r,$nextQ,$p")
    }
    writeln("r,1,1")

    closeFile()
  }
}
