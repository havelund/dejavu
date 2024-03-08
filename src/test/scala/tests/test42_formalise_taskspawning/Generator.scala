package tests.test42_formalise_taskspawning

import java.io.{File, PrintWriter}

object Util {
  var pw: PrintWriter = null
  var count : Int = 0
  var spawns : Int = 0


  val testdir = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/tests/"
  val dir = testdir + "test42_formalise_taskspawning/"

  def openFile(nr: Int): Unit = {
    val file = s"biglog${nr}k.csv"
    pw = new PrintWriter(new File(dir + file))
  }

  def closeFile(): Unit = {
    pw.close()
    println(s"count = $count")
    println(s"spawns = $spawns")
  }

  def writeln(x: Any) = {
    count += 1
    pw.write(x + "\n")
    println(x)
  }
}

import Util._

object Generator {
  //val (threads, repeat) = (50,100) // 10k
  //val (threads, repeat) = (100,100) // 20k
  val (threads, repeat) = (100,200) // 40k
  var next = 1
  var threadsNew : List[Int] = List()

  def maint(): Unit = {
    writeln(s"spawn,0,$next")
    threadsNew :+= next
    next += 1
    spawns += 1
  }

  def spawn(x:Int): Unit = {
    writeln(s"spawn,$x,$next")
    threadsNew :+= next
    next += 1
    spawns += 1
  }

  def report(x:Int, y:Int): Unit = {
    writeln(s"report,$x,$y,data")
  }

  def main(args: Array[String]) = {
    openFile(40)

    for (_ <- 1 until threads) {
      maint()
    }
    for (x <- 1 until threads) {
      report(x,0)
    }
    for (_ <- 1 to repeat) {
      val threadsOld = threadsNew
      threadsNew = List()
      for (x <- threadsOld) {
        spawn(x)
        report(next-1,0)
      }
    }

    report(0,0)

    closeFile()
  }
}