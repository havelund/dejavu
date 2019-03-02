package tests.test19_relations

object Util {
  val dir = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/tests/test19_relations/"
}
import Util._
import tests.util.tracegenerator.Generator

class GenerateLog2 extends Generator(dir + "log2.csv") {
  val start = "start"
  val open = "open"
  val close = "close"

  def run(): Unit = {
    val OPEN = 50000; val NR = 1000; val REPEAT = 1000

    var current : Int = 0
    emit(start)
    up(1, OPEN)(open)
    current = OPEN
    repeat(REPEAT) {
      down(current,current - NR)(close)
      current += 1
      up(current, current + NR)(open)
      current += NR
    }
    emit(close,current + 999)
    emit(open,1)
    end()
    println(s"GENERATED NUMBER OF EVENTS     : $counter")
    println(s"GENERATED NUMBER OF FILES      : $current")
    println(s"NUMBER OF VALUES REPRESENTABLE : ${math.pow(2,20)}")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val generator = new GenerateLog2
    generator.run()
  }
}