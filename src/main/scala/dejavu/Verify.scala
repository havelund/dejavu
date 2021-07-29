package dejavu

import scala.io.Source

/**
  * This class offers a main method that runs DejaVu on a log file.
  * DejaVu can also be executed in online mode, monitoring an executing
  * application. However, the current setup is focused on analysis of
  * log files.
  *
  * The main method can be run in two modes:
  *
  * (1) in development mode (when the code is being developed).
  *     This situation is detected by the main method being provided the trace file
  *     as well as the spec file as argument, and any other arguments.
  *     When run in development mode, the main method performs all tasks, such
  *     as compiling the generated monitor, and running the resulting monitor on the
  *     trace.
  *
  * (2) in command line mode in a shell using the dejavu script.
  *     When run in command line mode, the main method is just called on the spec
  *     file (and not the trace file), which generates the monitor code, and the script
  *     will itself compile and run the generated monitor.
  */

object Verify {

  /**
    * Flag indicating whether long tests (> 7 seconds) should be executed.
    */

  val LONGTEST : Boolean = true

  /**
    * Flag indicating whether a test took place. Becomes false when a long test is executed but the <code>LONGTEST</code> flag is false.
    * Is called by the result verification functions.
    */

  var verified : Boolean = false

  /**
    * Method for timing the execution of a block of code.
    *
    * @param text this text is printed as part of the timing information.
    * @param block the code to be executed.
    * @tparam R the result type of the block to be executed.
    * @return the result of ececution the block.
    */

  def time[R](text: String)(block: => R): R = {
    val t1 = System.currentTimeMillis()
    val result = block
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toFloat
    val sec = ms / 1000
    println(s"\n--- Elapsed $text time: " + sec + "s" + "\n")
    result
  }

  /**
    * Executes a UNIX shell command from within the Scala program. Some messages on the input stream
    * and error stream are ignored.
    *
    * @param cmd the shell command to be executed. The vararg solution is needed in case one of
    *            the arguments contains a <code>*</code>.
    */

  def exec(cmd: String*) {
    // println(s"\n--- [${cmd.mkString(" ")}] ---\n")
    val cmdArray = cmd.toArray
    val process =
      if (cmdArray.length == 1) Runtime.getRuntime exec cmd(0) else Runtime.getRuntime exec cmdArray
    val input = process.getInputStream
    val error = process.getErrorStream
    val inputSource = Source fromInputStream input
    val errorSource = Source fromInputStream error
    for (line <- inputSource.getLines()) {
      if (!line.startsWith("Could not load BDD") && !line.startsWith("Resizing node table")) {
        println(line)
      }
    }
    for (line <- errorSource.getLines()) {
      if (!line.contains("warning") && !line.startsWith("Garbage collection")) {
        println(line)
      }
    }
  }

  /**
    * Compiles the generated monitor <code>TraceMonitor.scala</code> and runs it
    * on the trace file (and other options) provided as a string argument.
    *
    * @param args
    */

  def compileAndExecute(args: String): Unit = {
    val lib = Settings.PROJECT_DIR + "/lib"
    time("monitor compilation") {
      println("Compiling generated monitor ...")
      exec(s"scalac -cp .:$lib/commons-csv-1.1.jar:$lib/javabdd-1.0b2.jar TraceMonitor.scala")
    }
    time("trace analysis") {
      println("Analyzing trace ...")
      exec(s"scala -J-Xmx16g -cp .:$lib/commons-csv-1.1.jar:$lib/javabdd-1.0b2.jar TraceMonitor $args")
    }
    exec("sh", "-c", "rm *.class") // multi-argument call needed due to occurrence of *
  }

  /**
    * Main method for executing DejaVu. If only one argument is provided (the specification
    * file - which happens when the main method is called from the dejavu shell script),
    * this main method only generates the monitor <code>TraceMonitor.scala</code>. The script
    * will then compile and run it. If also the trace is provided we are (likely) in development
    * mode, and this main method will also compile and run <code>TraceMonitor.scala</code>. This
    * latter mode is useful when e.g. working in an IDE such as IntelliJ where no script exists.
    *
    * @param arguments following the pattern:
    *                  <code><specfile> [ <logfile> [ <bitspervar> [debug|profile] ] ]</code>
    */

  def main(arguments: Array[String]) {
    SymbolTable.reset()
    val args = arguments.toList
    if (args.length < 1 || args.length > 4) {
      println("*** wrong number of arguments, call with: <specfile> [ <logfile> [ <bitspervar> [debug|profile] ] ]")
      return
    }
    if (args.length > 2 && !args(2).matches("""\d+""")) {
      println(s"*** third argument must be an integer, and not ${args(2)}")
      return
    }
    if (args.length == 4 && args(3) != "debug" && args(3) != "profile") {
      println(s"*** fourth argument must be: debug or profile, and not ${args(3)}")
      return
    }
    time("total") {
      time("monitor synthesis") {
        val p = new Parser
        val file = arguments(0)
        val spec = p.parseFile(file)
        println(spec)
        spec.translate()
      }
      if (args.length > 1) compileAndExecute(args.tail.mkString(" "))
    }
  }

  def apply(arguments : String*): Unit = {
    main(arguments.toArray)
    verified = true
  }

  def long(arguments : String*): Unit = {
    if (LONGTEST) {
      main(arguments.toArray)
      verified = true
    } else {
      verified = false
    }
  }
}
