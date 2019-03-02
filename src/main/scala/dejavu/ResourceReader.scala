package dejavu

import java.io._

/**
  * This class is performing a read of a file that is wrapped with a
  * generated jar file. This approach is e.g. recommended here:
  * https://alvinalexander.com/blog/post/java/read-text-file-from-jar-file.
  */

object ResourceReader {
  def read(rsc: String): String = {
    var value = ""
    val i = ResourceReader.getClass.getResourceAsStream(rsc)
    val r = new BufferedReader(new InputStreamReader(i))
    var l = r.readLine
    while (l != null) {
      value = value + l + "\n"
      l = r.readLine
    }
    i.close()
    value
  }
}

