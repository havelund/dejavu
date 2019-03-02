package tests.util.csvreader

import java.io.PrintWriter

import scala.io.{BufferedSource, Source}
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by rjoshi on 3/23/16.
  */
case class CSVRow(hdr: Map[String, Int], body: List[String]) {
  def apply(f: String): String = {
    val ix = hdr.getOrElse(f, -1)
    if ((ix < 0) || (ix >= body.length)) null else body(ix)
  }

  def toMap: Map[String, String] = {
    var map = Map.empty[String, String]
    for ((k, v) <- hdr) {
      map += (k -> body(v))
    }
    map
  }
}

class CSV(separator: String) extends Iterable[CSVRow] {
  var pos = 0
  var body: Stream[CSVRow] = null
  var hdr: Map[String, Int] = null
  var keys: List[String] = null
  var len : Int = 0

  def getKeys = keys

  def apply(i: Int) = body(i).toMap

  def iterator = new Iterator[CSVRow] {
    def next(): CSVRow = if (pos >= len) null else { pos += 1 ; body(pos-1) }
    def hasNext = (body != null) && (pos < len)
  }

  def rewind(): Unit = {
    pos = 0
  }

  object Parser extends RegexParsers {
    override def skipWhitespace = false

    def line(in: String) = parseAll(csvRecord, in) match {
      case Success(s, _)     => s
      case NoSuccess(msg, _) => throw new Exception(msg)
    }

    def csvRecord = repsep(rawField, separator)
    def whitespace = """[ \t]*""".r
    def rawField = opt(whitespace) ~> field <~ opt(whitespace)
    def field = quotedField | simpleField
    def simpleField = """[^\n\r%s"]*""".format(separator).r
    def quotedField = "\"" ~> escapedField <~ "\""
    def escapedField = repsep("""[^"]*""".r, "\"\"") ^^ { _.mkString("", "\"", "") }
  }

  def init(lines: Stream[List[String]]) {
    if (lines.nonEmpty) {
      keys = lines.head
      hdr = (keys zip (0 to keys.length).toList).toMap
      body = lines.drop(1).map((r) => CSVRow(hdr, r))
      len = body.length
    }
  }

  def length = if (body == null) 0 else body.length
}

object CSV {
  // def parseCmdString(cmd: String, separator: String = ",") = { val c = new CSV(separator) ; c.init(Process(cmd).lineStream.map(c.Parser.line)) ; c }
  // def parseCmd(cmd: List[String], separator: String = ",") = { val c = new CSV(separator) ; c.init(Process(cmd).lineStream.map(c.Parser.line)) ; c }
  def parseFile(buf: BufferedSource, separator: String = ",") = { val c = new CSV(separator) ; c.init(buf.getLines().map(c.Parser.line).toStream) ; c }
  def parsePath(fname: String, separator: String = ",") = { val c = new CSV(separator) ; c.init(Source.fromFile(fname).getLines().map(c.Parser.line).toStream) ; c }

  def writeMap(m:scala.collection.Map[String,String], keys:List[String], pw: PrintWriter) {
    var sep = ""
    for (k <- keys) {
      val v = if (m == null) k else m.getOrElse(k, "")
      pw.print(s"$sep$v")
      sep = ","
    }
    pw.println()
  }
}
