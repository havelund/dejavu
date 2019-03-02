package sandbox.garbagecollection.garbagecollection0


/* Generic Monitoring Code common for all properties. */

import net.sf.javabdd.{BDD, BDDFactory}
import java.io._

import org.apache.commons.csv.{CSVFormat, CSVRecord}

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

object Options {
  var DEBUG: Boolean = false
  var PRINT: Boolean = false
  var BITS: Int = 3
  var PRINT_LINENUMBER_EACH: Int = 1000
}

object Util {
  type Binding = Map[String, Any]
  val emptyBinding: Binding = Map()

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

  def writeln() = {
    pw.write("\n")
  }

  def writeln(x: Any) = {
    pw.write(x + "\n")
  }
}

import Util._

/**
  * Patterns for checking whether the state contains a certain event.
  */

trait Pattern

case class V(name: String) extends Pattern

case class C(value: Any) extends Pattern

/**
  * A state in a trace. A trace holds one event. Event patterns can be checked against
  * the state using the <code>holds</code> method.
  */

class State {
  type Event = (String, List[Any])

  var current: Event = null

  def update(name: String, args: List[Any]): Unit = {
    current = (name, args)
  }

  def holds(name: String, patterns: List[Pattern]): Option[Binding] = {
    val (cname, cargs) = current
    var binding: Binding = emptyBinding
    if (cname != name) None else {
      assert(patterns.size == cargs.size,
        s"patterns '${patterns.mkString(",")}' do not match args: '${cargs.mkString(",")}'")
      for ((pat, value) <- patterns.zip(cargs)) {
        pat match {
          case C(v) =>
            if (v != value) return None
          case V(x) =>
            if (binding.isDefinedAt(x)) {
              if (binding(x) != value) return None
            } else {
              binding += (x -> value)
            }
        }
      }
      Some(binding)
    }
  }

  override def toString: String = {
    var result = ""
    result += "#########################################################\n"
    result += s"#### ${current._1}("
    result += current._2.mkString(",") + ")\n"
    result += "#########################################################\n"
    result
  }
}

/**
  *
  * A variable is represented by an object of this class.
  *
  * @param name     the name of the variable, used for error messages
  * @param offset   the offset in the total bitvector where the bits for this variable start.
  * @param nrOfBits the number of bits allocated to represent values of this variable.
  * @param B        the BDD factory. As an implicit parameter we don't provide it explicitly in calls
  *                 of the constructor, it is assumed to exist in the context of the call.
  */

class Variable(name: String, offset: Int, nrOfBits: Int)(implicit B: BDDFactory) {
  var bits: Array[Int] = (for (x <- offset + nrOfBits - 1 to offset by -1) yield x).toArray
  var quantvar: BDD = B.buildCube(0, bits).support() // needed to perform quantification.
  var next: Int = -1
  var bdds: Map[Any, BDD] = Map()
  val MAX = Math.pow(2, nrOfBits)

  /**
    * Gets the next natural number in the enumeration of values of this variable.
    *
    * @return the next natural number in the enumeration.
    */

  def nextInt(): Int = {
    next += 1
    assert(next < MAX,
      s"${Options.BITS} bits is not enough to represent variable $name.")
    next
  }

  /**
    * Returns the BDD corresponding to the value, according to the enumeration of the value.
    * Either it exists already or it is built.
    *
    * @param v the value for which a BDD must be created.
    * @return the BDD corresponding to <code>v</code>.
    */

  def getBddOf(v: Any): BDD = {
    bdds.get(v) match {
      case Some(bdd) => bdd
      case None =>
        val bdd = B.buildCube(nextInt(), bits)
        bdds += (v -> bdd)
        bdd
    }
  }
}

/**
  * An object of this class represents all the variables in a formula.
  * It contains a mapping from variable names (strings) to objects of
  * class <code>Variable</code>, each of which contains the hashmap
  * from values of the corresponding variable to BDDs.
  *
  * @param variables the variables in the formula, each indicated by a name and number of bits
  *                  representing it.
  */

class BDDGenerator(variables: List[(String, Int)]) {
  implicit var B: BDDFactory = BDDFactory.init(10000, 10000)
  val True: BDD = B.one()
  val False: BDD = B.zero()
  var offset: Int = 0
  val totalNumberOfBits: Int = variables.map(_._2).sum
  var varMap: Map[String, Variable] = Map()

  B.setVarNum(totalNumberOfBits)

  for ((x, v) <- variables) {
    varMap += (x -> new Variable(x, offset, v))
    offset += v
  }

  /**
    * Get the BDD of value <code>v</code> when assigned to variable <code>x</code>.
    *
    * @param x the variable the value <code>v</code> is assigned to.
    * @param v the value being assigned to <code>x</code>.
    * @return the BDD representing the value <code>v</code>.
    */

  def getBddOf(x: String, v: Any): BDD =
    varMap(x).getBddOf(v)
}

/**
  * The specialized montor for a set of properties must extend this class.
  * It contains the BDD generator (which generates the association between values
  * and BDDs), the state (which contains the current event), and the list of user
  * provided formulas. In addition it provides a set of options that can be set
  * by the user.
  */

class Monitor {
  val state: State = new State
  var formulae: List[Formula] = Nil
  var lineNr: Int = 0

  /**
    * Prints string on std out if <code>DEBUG</code> flag is true.
    *
    * @param str string to print.
    */

  def debug(str: String): Unit = {
    if (Options.DEBUG) println(str)
  }

  /**
    * Converts a BDD to a string.
    *
    * @param bdd BDD to be converted.
    * @return string a string representation of the BDD.
    */

  def bddToString(bdd: BDD): String = {
    if (bdd.isZero)
      "False"
    else if (bdd.isOne)
      "True"
    else
      bdd.toString
  }

  /**
    * Submits an event to the monitor. This again causes the monitor evaluation to be
    * performed, which will evaluate all asserted formulas on this new event.
    *
    * @param name the name of the event.
    * @param args the arguments to the event.
    */

  def submit(name: String, args: List[Any]): Unit = {
    state.update(name, args)
    evaluate()
  }

  /**
    * Vararg (variable length argument list) variant of method above. This form allows calls
    * like <code>submit("send",1,2)</code> rather than writing <code>submit("send",List(1,2))</code>.
    * Submits an event to the monitor. This again causes the monitor evaluation to be
    * performed, which will evaluate all asserted formulas on this new event.
    *
    * @param name the name of the event.
    * @param args the arguments to the event.
    */

  def submit(name: String, args: Any*): Unit = {
    submit(name, args.toList)
  }

  /**
    * Submits an entire trace to the monitor, as an alternative to submitting
    * events one by one. This method can only be called in offline monitoring.
    *
    * @param events the trace.
    */

  def submitTrace(events: List[(String, List[Any])]): Unit = {
    for ((event, args) <- events) {
      submit(event, args)
    }
  }

  /**
    * Submits an entire trace stored in CSV (Comma Separated Value format) format
    * to the monitor, as an alternative to submitting events one by one. This method
    * can only be called in offline monitoring.
    *
    * @param file the log file in CSV format to be verified.
    */

  def submitCSVFile(file: String) {
    val in: Reader = new BufferedReader(new FileReader(file))
    // DEFAULT.withHeader()
    val records: Iterable[CSVRecord] = CSVFormat.DEFAULT.parse(in).asScala
    lineNr = 0
    for (record <- records) {
      lineNr += 1
      if (Options.PRINT && lineNr % Options.PRINT_LINENUMBER_EACH == 0) {
        if (lineNr >= 1000000)
          println(lineNr.toDouble / 1000000 + " M")
        else if (lineNr >= 1000)
          println(lineNr.toDouble / 1000 + " K")
        else
          println(lineNr.toDouble)
      }
      val name = record.get(0)
      var args = new ListBuffer[Any]()
      for (i <- 1 until record.size()) {
        args += record.get(i)
      }
      submit(name, args.toList)
    }

    println(s"Processed $lineNr events")
    in.close()
  }

  /**
    * Evaluates all formulas on a new state (new event). In case a property is violated an
    * error message is printed. There is currently no other consequence of a violated
    * property.
    */

  def evaluate(): Unit = {
    debug(s"\n$state\n")
    for (formula <- formulae) {
      if (!formula.evaluate()) {
        println(s"\n*** Property ${formula.name} violated on event number $lineNr:\n")
        println(state)
      }
    }
  }

  /**
    * Records property violation in the result file. Currently only event number
    * of violating event is recorded. This information is used for unit testing.
    */

  def recordResult(): Unit = {
    writeln(lineNr)
  }

  /**
    * Every formula will be defined as a class extending this class.
    *
    */

  abstract class Formula {
    // A property named xyz will be defined by a class Formula_xyz. Pick out the name xyz:
    var name: String = this.getClass.getSimpleName.split("_")(1)
    // BDD generator:
    var bddGenerator: BDDGenerator = null
    // Pre and now arrays, as in article:
    var pre: Array[BDD] = null
    var now: Array[BDD] = null
    // temporary pointer, used to swap the pre and now arrays:
    var tmp: Array[BDD] = null
    // maps sub-formula indexes to the text format of the sub-formulas, used for
    // debugging purposes:
    var txt: Array[String] = null

    // True and false, assigned values in <code>declareVariables</code> below:
    var True: BDD = null
    var False: BDD = null

    /**
      * Turns an optional binding from variable names to values (an assignment) into a BDD.
      * This is achieved by computing the BDD for each variable/value pair and the  AND-ing these BDDs
      * together. The function is called when an event pattern has matched an incoming
      * event in the state.
      *
      * @param binding the binding to convert into a BDD.
      * @return the BDD resulting from and-ing the BDDs for each variable binding in <code>binding</code>.
      */

    def bddFromBinding(binding: Option[Binding]): BDD = {
      binding match {
        case None => bddGenerator.False
        case Some(b) =>
          var bdd: BDD = bddGenerator.True
          for ((x, v) <- b) {
            bdd = bdd.and(bddGenerator.getBddOf(x, v))
          }
          bdd
      }
    }

    /**
      * Attempts to build a BDD from an event pattern, matching it against the latest
      * incoming event in the current state. A particular event pattern either matches the
      * current event or not. If so, values are bound to formal parameter names of the event,
      * forming a binding (assignment). The BDD is then created from this binding.
      *
      * @param name     the name of the event.
      * @param patterns the patterns that are meant to match the arguments of the actual event.
      * @return the BDD resulting from the match, False if no match occurred.
      */

    def build(name: String)(patterns: Pattern*): BDD =
      bddFromBinding(state.holds(name, patterns.toList))

    /**
      * Declares all variables (each identified by a name) in a formula.
      * This includes initializing the BDD generator, which is stored in
      * <code>bddGenerator</code>, and initializing <code>True</code> and
      * <code>False</code>. The result returned is a list of BDDs (all with value 0, the particular
      * value is not used, only the bit locations), one for each variable, which are used
      * for quantification only.
      *
      * @param variables the names of variables in a formula.
      * @return a list of zero-valued BDDs, one for each variable. Used for quantification
      *         over all the bits that represent that variable.
      */

    def declareVariables(variables: String*): List[BDD] = {
      val variableList = variables.toList
      val varsAndBitsPerVar = variableList.map {
        case v => (v, Options.BITS)
      }
      bddGenerator = new BDDGenerator(varsAndBitsPerVar)
      True = bddGenerator.True
      False = bddGenerator.False
      variableList.map(bddGenerator.varMap(_).quantvar)
    }

    /**
      * The evaluation method for a formula. Must be overridden for each formula.
      * The method will evaluate the formula on each new event.
      *
      * @return true iff. the formula is true on the trace seen so far.
      */

    def evaluate(): Boolean

    /**
      * Returns a string representation of the current values of the <code>pre</code> and
      * <code>now</code> arrays. For each index into these arrays also the text of the
      * subformula is printed for better comprehension.
      *
      * @return string representation of formula state.
      */

    override def toString: String = {
      var result: String = ""
      result += s"===============\n"
      result += s"Property $name:\n"
      result += s"===============\n"
      for (i <- 0 to now.size - 1) {
        result += s"[$i] ${txt(i)}\n\n"
        result += s"pre: ${bddToString(pre(i))}\n"
        result += s"now: ${bddToString(now(i))}\n"
        result += s"-------------\n"
      }
      result
    }

    /**
      * Prints a formula state for debugging. This includes whether the formula is true or not,
      * and the value of the <code>now</code> array, where each entry is printed both as a one
      * line text value, and also as a graph in dot format for visualization with GraphViz.
      */

    def debugMonitorState(): Unit = {
      if (Options.DEBUG) {
        println("================")
        println(s"Property: $name")
        println("================")
        println()
        if (now(0).isZero) {
          println("*** FALSE ***")
          println()
        }
        for (i <- now.size - 1 to 0 by -1) {
          println(s"----- $i -----")
          println(txt(i))
          if (now(i).isOne) println("TRUE") else if (now(i).isZero) println("FALSE") else {
            println(s"now:")
            println(now(i)) // prints BDD as a one line text
            now(i).printDot() // prints BDD in dot format for vizualization with GraphViz
          }
        }
      }
    }
  }

}


/* Specialized Monitoring Code for properties. */

class PropertyMonitor extends Monitor {

  /*
    property noFailure: forall c . forall n . suc(c,n) -> (!err(n) S dis(c,n))
  */

  class Formula_noFailure extends Formula {

    val var_c :: var_n :: Nil = declareVariables("c", "n")

    implicit def liftBDD(bdd: BDD) = new {
      def dot(text: String): Unit = {
        println(s"--- $text:")
        if (bdd == False)
          println("False")
        else if (bdd == True)
          println("True")
        else bdd.printDot()
        println("-----------")
      }
    }

    def line(): Unit = {
      println("-----------")
    }

    override def evaluate(): Boolean = {
      now(3) = build("suc")(V("c"), V("n"))
      now(6) = build("err")(V("n"))
      now(5) = now(6).not()
      now(7) = build("dis")(V("c"), V("n"))
      now(4) = now(7).or(now(5).and(pre(4)))
      now(2) = now(3).not().or(now(4))
      now(1) = now(2).forAll(var_n)
      now(0) = now(1).forAll(var_c)

      debugMonitorState()

      println("@@@@@@@@@@ BEGIN @@@@@@@@@@")

      // Show:
      // - how to obtain an assignment satisfying a BDD
      // - how to substitute a constant for a variable

      // --- First create a BDD with six variables: v

      val bdd = now(4)
      bdd.dot("bdd=now(4)")

      // --- and select a subset of this assignment using existential quantification: v

      val bddSub = bdd.exist(var_c)
      bddSub.dot("bddSub=bdd.exist(var_c)") // resulting in BDD for variables 3, 4, and 5

      // --- obtaining an assignment for a variable: --- v

      val bddSubNot = bddSub.not()
      bddSubNot.dot("bddSubNot=bddSub.not()")

      val bddSelect = bddSubNot.satOne()
      bddSelect.dot("bddSelect=bddSubNot.satOne()")

      // --- Now substituting variable 3 with 0: --- v

      val var3False = bddGenerator.B.nithVar(3)

      val bddSubRestricted = bddSub.restrict(var3False)
      bddSubRestricted.dot("bddRestricted=bddSub.restrict(var3False)")

      // --- Testing for zero: --- v

      // is, and should be, false:
      println(s"var3False.isZero = ${var3False.isZero}")
      line()

      // --- 111...11: --- v

      // --- Using buildCube (has upper limit determined by word size): v

      val bits = Array(5, 4, 3)
      val nrOfBits = 3
      val shift = (1 << (nrOfBits + 1)) - 1
      println(s"shift = $shift")
      val allOnes1: BDD = bddGenerator.B.buildCube(shift, bits)
      allOnes1.dot("bddGenerator.B.buildCube(shift, bits)")

      // --- Using ithVar (flexible): v

      def mkAllOnes(min: Int, max: Int): BDD = {
        assert(min <= max)
        val bdd1 = bddGenerator.B.ithVar(min)
        if (min == max)
          bdd1
        else {
          val bdd2 = mkAllOnes(min + 1, max)
          bdd1.and(bdd2)
        }
      }

      val allOnes = mkAllOnes(3, 5)
      allOnes.dot("mkAllOnes(3,5)")

      // --- free[x] := BDD(1) except 111...1 and then pick one:

      val free = allOnes.not()
      free.dot("allOnes.not")
      val freePick = free.satOne(allOnes, true)
      freePick.dot("free.satOne(allOnes, true)")

      // --- Implication: --- v

      val var2True = bddGenerator.B.ithVar(2)
      // is, and should be, true:
      val twoOrNotThree = var2True.or(var3False)
      val notThree = var3False
      val Implication = notThree.imp(twoOrNotThree)
      Implication.dot("notThree.imp(twoOrNotThree)")

      println("@@@@@@@@@@ END @@@@@@@@@@")

      val error = now(0).isZero
      if (error) recordResult()
      tmp = now
      now = pre
      pre = tmp
      !error
    }

    pre = Array.fill(8)(False)
    now = Array.fill(8)(False)

    txt = Array(
      "forall c . forall n . suc(c,n) -> (!err(n) S dis(c,n))",
      "forall n . suc(c,n) -> (!err(n) S dis(c,n))",
      "suc(c,n) -> (!err(n) S dis(c,n))",
      "suc(c,n)",
      "!err(n) S dis(c,n)",
      "!err(n)",
      "err(n)",
      "dis(c,n)"
    )

    debugMonitorState()
  }

  formulae ++= List(new Formula_noFailure)

}

object TraceMonitor {
  def main(args: Array[String]): Unit = {
    val m = new PropertyMonitor
    openFile("dejavu-results")

    m.submit("dis", "turn", 1)
    m.submit("dis", "radio", 2)
    m.submit("err", 1)

    //    m.submit("dis", "turn", 1)
    //    m.submit("suc", "turn", 1)
    //    m.submit("dis", "radio", 2)
    //    m.submit("dis", "transmit", 3)
    //    m.submit("suc", "transmit", 3)
    //    m.submit("err", 2)
    //    m.submit("suc", "radio", 2)
    closeFile()
  }
}









