package tests.util.tracegenerator

import java.io._

abstract class Generator(dir: String) {
  var counter: Int = 0
  type Evr = (Int, String, List[Any])
  val pwDVU = new PrintWriter(new File(dir + "/log-dejavu.txt"))
  val pwMPY = new PrintWriter(new File(dir + "/log-monpoly.txt"))

  def repeat(nr: Int)(code: => Unit): Unit = {
    for (i <- 1 to nr) {
      code
    }
  }

  def iterate(limit1: Int, limit2: Int, step: Int = 1)(event: String, arguments: Any*): Unit = {
    println(s"---from $limit1 to $limit2 step $step : $event(${arguments.mkString(",")}) ------------------------")
    val args = arguments.toList
    for (i <- limit1 to limit2 by step) {
      counter +=1
      val evr: Evr = (counter, event, i :: args)
      emitEvr(evr)
    }
  }

  def emitEvr(evr: Evr): Unit = {
    val formattedDVU = formatDVU(evr)
    val formattedMPY = formatMPY(evr)
    println(formattedDVU)
    pwDVU.write(formattedDVU + "\n")
    pwMPY.write(formattedMPY + "\n")
  }

  def emit(event: String, args: Any*): Unit = {
    counter += 1
    val evr = (counter, event, args.toList)
     emitEvr(evr)
  }

  def formatDVU(event: Evr): String = {
    val (counter, name, args) = event
    s"$name,${args.mkString(",")}"
  }

  def formatMPY(event: Evr): String = {
    val (counter, name, args) = event
    f"@$counter%07d $name (${args.mkString(",")})"
  }

  //  @000001 open (aaa)
  //  @000002 open (aah)

  def up(limit1: Int, limit2: Int)(event: String, arguments: Any*): Unit = {
    iterate(limit1, limit2)(event: String, arguments: _*)
  }

  def down(limit1: Int, limit2: Int)(event: String, arguments: Any*): Unit = {
    iterate(limit1, limit2, -1)(event: String, arguments: _*)
  }

  def end(): Unit = {
    println(s"$counter events generated")
    pwDVU.close()
    pwMPY.close()
  }

  def run()
}



