package tests.test21_msl

import tests.util.csvreader.CSV
import java.io._

object Constants {
  val dirName = "/Users/khavelun/Desktop/development/ideaworkspace/dejavu/src/test/scala/tests/test21_msl/"
}
import Constants._

object Util {
  type Sclk = Double

  var pw: PrintWriter = null

  def openFile(name: String): Unit = {
    pw = new PrintWriter(new File(name))
  }

  def closeFile(): Unit = {
    pw.close()
  }

  def write(x: Any) = {
    pw.write(x.toString)
  }

  def writeln(x: Any) = {
    pw.write(x + "\n")
  }
}

import Util._

case class Interval(name: String, begin: Sclk, end: Sclk, data: Map[String, Any])

class FileReader {
  def readEVRS(fileName: String, limit: Int = Integer.MAX_VALUE): List[Interval] = {
    val intervals = new scala.collection.mutable.ListBuffer[Interval]()
    val csv = CSV.parsePath(fileName)
    var count = 0
    for (row <- csv) {
      count += 1
      if (count % 1000 == 0) println(s"records processed: $count")
      if (count >= limit) return intervals.toList
      var name = row("name")
      val time = row("sclk").toFloat.toInt
      val words = row("message").split(" +")
      var cmd: String = "DEFAULT"
      name match {
        case "SEQ_EVR_CMD_DISPATCH_ID" => cmd = words(4); name = "CMD_DISPATCH"
        case "SEQ_EVR_WAIT_CMD_COMPLETED_SUCCESS" => cmd = words(5); name = "CMD_COMPLETE"
        case "SDST_EVR_TLM_TR_COMPLETED_ERROR" => name = "TLM_TR_ERROR"
        case _ =>
      }
      val data = Map("cmd" -> cmd)
      intervals += Interval(name, time, time, data)
    }
    intervals.toList
  }
}

object ConvertMSLTrace {
  def main(args : Array[String]): Unit = {
    val intervals: List[Interval] = new FileReader().readEVRS(dirName + "msl/all-evrs.csv", 50000)
    var count: Int = 0
    println("start")
    var cmdNames : Set[String] = Set()
    openFile(dirName + "log_msl_timed.csv")
    for (interval <- intervals) {
      count += 1
      val Interval(cmdName,time,_,map) = interval
      cmdNames += cmdName
      val cmd = map("cmd")
      var outputLine : String = s"$cmdName"
      if (cmd != "DEFAULT") outputLine += s",$cmd"
      outputLine += s",$time"
      writeln(outputLine)
    }
    closeFile()
    println(cmdNames)
  }
}

