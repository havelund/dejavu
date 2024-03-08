package tests.test39_formalise_wolper

import java.io.{File, PrintWriter}

object Util {
  var pw: PrintWriter = null
  var count : Int = 0

  val testdir = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/tests/"
  val dir = testdir + "test39_formalise_wolper/"

  def openFile(nr: Int): Unit = {
    val file = s"biglog${nr}k.csv"
    pw = new PrintWriter(new File(dir + file))
  }

  def closeFile(): Unit = {
    pw.close()
    println(s"count = $count")
  }

  def writeln(x: Any) = {
    count += 1
    pw.write(x + "\n")
    println(x)
  }
}

import Util._

object Generator {
  // repeat: repeat the following
  // repeat_toggle: open this many channels
  // repeat_telem: send this many messages on each channel
  //
  // val (repeat, repeat_toggle, repeat_telem) = (10,100,100) // 100k
  // val (repeat, repeat_toggle, repeat_telem) = (100,1000,10) // 1_000k
  // val (repeat, repeat_toggle, repeat_telem) = (1000,100,50) // 5_000k
  val (repeat, repeat_toggle, repeat_telem) = (1000,100,100) // 10_000k

  def telem(x: Int): Unit = {
    writeln(s"telem,$x")
  }

  def toggle(x: Int): Unit = {
    writeln(s"toggle,$x")
  }

  def main(args: Array[String]) = {
    openFile(10000)

    for (_ <- 1 to repeat) {
      for (i <- 1 to repeat_toggle) {
        toggle(i)
      }
      for (i <- 1 to repeat_toggle) {
        for (_ <- 1 to repeat_telem) {
          telem(i)
        }
      }
      for (i <- 1 to repeat_toggle) {
        toggle(i)
      }
    }

    telem(-1)

    closeFile()
  }
}
