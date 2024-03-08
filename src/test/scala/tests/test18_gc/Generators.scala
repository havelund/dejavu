package tests.test18_gc

object Util {
  // val dir = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/tests/test18_gc/"
  val dir = "/Users/khavelun/Desktop/"
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

class GenerateLog3 extends Generator(dir + "log3.csv") {
  val start = "start"
  val open = "open"
  val close = "close"

  def run(): Unit = {
    val OPEN = 1000; val NR = 500; val REPEAT = 3000

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

class GenerateLog4 extends Generator(dir + "log4.csv") {
  val start = "start"
  val open = "open"
  val close = "close"

  def run(): Unit = {
    val OPEN = 6; val NR = 5; val REPEAT = 200000

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
    emit(open,current)
    end()
    println(s"GENERATED NUMBER OF EVENTS     : $counter")
    println(s"GENERATED NUMBER OF FILES      : $current")
    println(s"NUMBER OF VALUES REPRESENTABLE : ${math.pow(2,20)}")
  }
}

class GenerateLog5 extends Generator(dir + "log5.csv") {
  val start = "start"
  val open = "open"
  val close = "close"

  def run(): Unit = {
    val REPEAT = 1000000

    var current : Int = 1
    emit(start)
    repeat(REPEAT) {
      emit(open, current)
      emit(close, current)
      current += 1
    }
    emit(close,current)
    emit(open,current)
    emit(open,current)
    end()
    println(s"GENERATED NUMBER OF EVENTS     : $counter")
    println(s"GENERATED NUMBER OF FILES      : $current")
    println(s"NUMBER OF VALUES REPRESENTABLE : ${math.pow(2,20)}")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val generator = new GenerateLog5
    generator.run()
  }
}