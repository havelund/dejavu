
/* Generic Monitoring Code common for all properties. */

import net.sf.javabdd.{BDD, BDDFactory}
import java.io._

import org.apache.commons.csv.{CSVFormat, CSVRecord}

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

object Options {
  var DEBUG: Boolean = false
  var PROFILE: Boolean = false
  var PRINT: Boolean = false
  var BITS: Int = 20
  var PRINT_LINENUMBER_EACH: Int = 1000
  var UNIT_TEST: Boolean = false
  var STATISTICS: Boolean = true
}

object Util {
  type Binding = Map[String, Any]
  val emptyBinding: Binding = Map()

  var resultFile: PrintWriter = null
  var profileFile: BufferedWriter = null

  def openResultFile(name: String): Unit = {
    resultFile = new PrintWriter(new File(name))
  }

  def writelnResult(x: Any) = {
    resultFile.write(x + "\n")
  }

  def closeResultFile(): Unit = {
    resultFile.close()
  }

  def openProfileFile(name: String): Unit = {
    val file = new File(name)
    profileFile = new BufferedWriter(new FileWriter(file))
  }

  def writeProfile(x: Any): Unit = {
    profileFile.write(x.toString)
  }

  def writelnProfile(x: Any): Unit = {
    profileFile.write(x + "\n")
  }

  def writelnProfile(): Unit = {
    profileFile.write("\n")
  }

  def closeProfileFile(): Unit = {
    profileFile.close()
  }

  def debug(str: => String): Unit = {
    if (Options.DEBUG) println(str)
  }

  def bddToString(bdd: BDD): String = {
    if (bdd.isZero)
      "False"
    else if (bdd.isOne)
      "True"
    else
      bdd.toString
  }

  implicit def liftBDD(bdd: BDD) = new {
    def dot(msg: String = "DEBUGGING"): Unit = {
      if (Options.DEBUG) {
        println("@@@@@@@@@@@@@@@@@@@@@")
        println(msg)
        if (bdd.isZero)
          println("False")
        else if (bdd.isOne)
          println("True")
        else
          bdd.printDot()
      }
    }
  }
}

import Util._

/**
  * Patterns for checking whether the state contains a certain event.
  */

trait Pattern

case class V(name: String) extends Pattern {
  override def toString: String = name
}

case class C(value: Any) extends Pattern {
  override def toString: String = value.toString
}

/**
  * A state in a trace. A trace holds one event. Event patterns can be checked against
  * the state using the <code>holds</code> method.
  */

class State {
  type Event = (String, List[Any])

  var current: Event = null

  /**
    * Updates the current state with a new event.
    *
    * @param name the name of the event.
    * @param args the arguments of the event.
    */

  def update(name: String, args: List[Any]): Unit = {
    current = (name, args)
    // println(s"$name(${args.mkString(",")})")
  }

  /**
    * Matches an event pattern as it occurs in a formula against the current event.
    *
    * @param name     the name of the event.
    * @param patterns the argument patterns.
    * @return optional BDD in case there is a match. The BDD will represent the binding of
    *         variables to values.
    */

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
  * A variable is represented by an object of this class.
  *
  * @param F        the formula that the variable is part of.
  * @param name     the name of the variable, used for error messages
  * @param offset   the offset in the total bitvector where the bits for this variable start.
  * @param nrOfBits the number of bits allocated to represent values of this variable.
  */


class Variable(F: Formula)(name: String, bounded: Boolean, offset: Int, nrOfBits: Int) {
  val G = F.bddGenerator
  var bits: Array[Int] = (for (x <- offset + nrOfBits - 1 to offset by -1) yield x).toArray
  val quantvar: BDD = G.getQuantVars(bits)
  // needed to perform quantification.
  var next: Int = -1
  var bdds: Map[Any, BDD] = Map()
  val MAX = Math.pow(2, nrOfBits)
  val allOnes: BDD = {
    var result: BDD = G.True
    for (pos <- bits) {
      result = result.and(G.theOneBDDFor(pos))
    }
    result
  }
  val freeInitially: BDD = allOnes.not
  var free: BDD = freeInitially
  var seen: BDD = G.False
  var inRelation: BDD = G.False

  /**
    * Records the fact that a BDD for this variable occurs in a relation, thus
    * preventing it from being garbage collected.
    *
    * @param bdd the BDD being recorded as being part of a relation.
    */

  def inRelation(bdd: BDD): Unit = {
    if (!bounded) inRelation = inRelation.or(bdd) // only add if not already added (i.e. the variable is bounded)
  }

  /**
    * Returns the BDD corresponding to the value, according to the enumeration of the value.
    * Either it exists already or it is built.
    *
    * @param v the value for which a BDD must be created.
    * @return the BDD corresponding to <code>v</code>.
    */

  def getBddOf(v: Any): BDD = {
    if (bdds.contains(v)) {
      val result = bdds(v)
      result.dot(s"Looking up existing BDD for $v")
      result
    } else {
      free.dot(s"free before get new BDD for variable $name for positions ${bits.mkString(",")}")
      if (timeToGarbageCollect) collectGarbage()
      if (free.isZero) {
        writelnResult(s"${F.monitor.lineNr} oom")
        assert(false, s"Out of memory for variable $name!")
      }
      val result = free.satOne(allOnes, true)
      free = free.and(result.not())
      bdds += (v -> result)
      F.addTouchedByLastEvent(name, v, result)
      result.dot(s"BDD for $name=$v")
      if (bounded) {
        seen = seen.or(result)
        seen.dot("seen thereafter")
      }
      free.dot("free thereafter")
      result
    }
  }

  /**
    * Determines whether it is time to garbage collect for a variable.
    *
    * @return true if it is time to garbage collect.
    */

  def timeToGarbageCollect: Boolean = {
    !bounded && free.isZero
  }

  /**
    * Collects garbage for a variable.
    */

  def collectGarbage(): Unit = {
    F.monitor.garbageWasCollected = true
    debug("+++++ START GARBAGE COLLECTION +++++")
    free = freeInitially
    free.dot(s"##### free initially")
    for (i <- F.indices) {
      val bdd_i = G.getFreeBDDOf(name, F.pre(i)) // not sure we access now at the right time
      free = free.and(bdd_i)
      bdd_i.dot(s"##### bdd_i for index $i")
      free.dot(s"##### free this cycle around")
    }
    free.dot(s"++++++++++ free after garbage collection before taking uncollectable into account ++++++++++")
    inRelation.dot(s"++++++++++ uncollectable before garbage collection ++++++++++")
    free = free.and(inRelation.not())
    free.dot(s"++++++++++ free after garbage collection ++++++++++")
    removeGarbageValues()
  }

  /**
    * Called after the <code>collectGarbage()</code> has been called to remove all
    * value-BDD mappings, where the BDD has been garbage collected.
    */

  def removeGarbageValues(): Unit = {
    val values = bdds.keySet
    for (v <- values) {
      val bdd = bdds(v)
      if (bdd.imp(free).isOne) {
        debug(s"removing variable $name's entry for value $v")
        writelnResult(s"${F.monitor.lineNr} -- $v")
        bdds -= v
      }
    }
    debug(s"Remaining entries for variable $name: ${bdds.keySet.mkString(", ")}")
  }
}

/**
  * An object of this class represents all the variables in a formula,
  * including variables containing time values if timed temporal properties
  * occur in the property.
  *
  * It contains a mapping from variable names (strings) to objects of
  * class <code>Variable</code>, each of which contains the hashmap
  * from values of the corresponding variable to BDDs.
  *
  * @param variables      the variables in the formula, each indicated by
  *                       name, whether it is bounded (true = yes), and number of bits representing it.
  * @param bitsPerTimeVar the number of bits to be allocated per time variable.
  *                       This number is `0` if the property does not contain
  *                       timed temporal operators.
  */

class BDDGenerator(F: Formula)(variables: List[(String, Boolean, Int)], bitsPerTimeVar: Int) {
  var B: BDDFactory = BDDFactory.init(10000, 10000)
  val True: BDD = B.one()
  val False: BDD = B.zero()
  var offset: Int = 0
  val totalNumberOfBits: Int = variables.map(_._3).sum
  var varMap: Map[String, Variable] = Map()
  lazy val otherQuantVars: Map[String, List[BDD]] = {
    val varNames = variables.map(_._1)
    var result: Map[String, List[BDD]] = (for (varName <- varNames) yield (varName -> Nil)).toMap
    for (varName1 <- varNames; varName2 <- varNames if varName1 != varName2) {
      val otherQuantVarsSoFar = result(varName1)
      val newOtherQuant = varMap(varName2).quantvar
      result += (varName1 -> (newOtherQuant :: otherQuantVarsSoFar))
    }
    result
  }

  val nrOfTimeVariables = 5

  if (totalNumberOfBits > 0 || bitsPerTimeVar > 0) {
    B.setVarNum(totalNumberOfBits + (nrOfTimeVariables * bitsPerTimeVar))
  }

  /**
    * Returns a BDD for the bit positions provided as argument. The BDD is used to
    * represent the bits to quantify over for a particular DejaVu formula variable.
    *
    * @param bits the bit positions (variables) to include in the BDD.
    * @return a BDD over those variables.
    */

  def getQuantVars(bits: Array[Int]): BDD = {
    B.buildCube(0, bits).support()
  }

  /**
    * The BDD for a single position that is true only of that bit is 1.
    *
    * @param pos the position making part of the resulting BDD.
    * @return the BDD accepting on 1 for that position.
    */

  def theOneBDDFor(pos: Int): BDD = {
    B.ithVar(pos)
  }

  /**
    * Initializes the <code>varMap</code> variable by mapping each variable in the formula to
    * an instance of the <code>Variable</code> class.
    */

  def initializeVariables(): Unit = {
    for ((x, b, v) <- variables) {
      varMap += (x -> new Variable(F)(x, b, offset, v))
      offset += v
    }
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

  /**
    * Collects the garbage for a variable in a sub-formula. This is done using the formula:
    *
    * <code>
    * forall y0,...,z0,... . (F[1/x0,...,1/xn] <-> F)
    * </code>
    *
    * where <code>x</code> is the variable, and <code>x0,x1,...,xn</code> are the bit positions for that variable,
    * and <code>y0,...,z0,...</code> are the bit positions for all other variables <code>y, z, ...</code>.
    * The formula defines a BDD which accepts values <code>v</code> for <code>x</code> (in <code>F</code>)
    * such that <code>F[v/x]</code> is identical to <code>F[1/x0,...,1/xn]</code>. Those are the values
    * that are no longer needed, hence can be garbage collected. Recall that 111..1 represents all values not
    * yet seen.
    *
    * @param varName the name of the variable being garbage collected (<code>x</code> in the above example).
    * @param formula the formula being garbage collected over (<code>F</code> in the above example).
    * @return the free assignments.
    */

  def getFreeBDDOf(varName: String, formula: BDD): BDD = {
    val variable = varMap(varName)
    val formulaWithOnes = formula.restrict(variable.allOnes)
    var result = formulaWithOnes.biimp(formula)
    for (quantVar <- otherQuantVars(varName)) result = result.forAll(quantVar)
    result
  }
}

/**
  * Maintains trace statistics for a monitoring session. It specifically keeps track of
  * which events occur in the trace, how many times, and how this relates to the events
  * referred to in the specification. Can be useful for debugging a specification.
  *
  * @param events the events referred to in the specification
  */

class TraceStatistics(events: Set[String]) {
  var eventTable: Map[String, Long] = events.map(_ -> 0.asInstanceOf[Long]).toMap

  def upddate(eventName: String): Unit = {
    eventTable.get(eventName) match {
      case None => eventTable += (eventName -> 1)
      case Some(count) => eventTable += (eventName -> (count + 1))
    }
  }

  override def toString: String = {
    var result: String = ""
    result += "\n"
    result += "==================\n"
    result += "  Event Counts:\n"
    result += "------------------\n"
    val maxNameSize = eventTable.keySet.map(_.size).max
    for ((name, count) <- eventTable) {
      val spaces = maxNameSize - name.size
      val namePadded = name + (" " * spaces)
      result += f"  $namePadded : $count"
      if (count == 0) {
        result += " event did not occur in trace\n"
      } else if (!(events contains name)) {
        result += " unknown\n"
      } else {
        result += "\n"
      }
    }
    result += "==================\n"
    result += "\n"
    result
  }
}

/**
  * The generic Monitor class.
  * A specialized monitor for a set of properties must extend this class.
  * It contains the BDD generator (which generates the association between values
  * and BDDs), the state (which contains the current event), and the list of user
  * provided formulas. In addition it provides a set of options that can be set
  * by the user.
  */

abstract class Monitor {
  val state: State = new State
  var formulae: List[Formula] = Nil
  var lineNr: Int = 0
  var garbageWasCollected: Boolean = false
  var statistics: TraceStatistics = new TraceStatistics(eventsInSpec)
  var currentTime: Int = 0
  var deltaTime: Int = 0
  var errors: Int = 0

  /**
    * Sets the current time to the time indicated by the timestamp associated
    * with the latest event. It specifically sets `deltaTime` to denote the
    * difference between the previous time stamp and this one.
    *
    * @param timeStamp the new time value for the latest event.
    */

  def setTime(timeStamp: Int): Unit = {
    deltaTime = timeStamp - currentTime
    currentTime = timeStamp
  }

  /**
    * Returns the set of events referred to in the specification, either defined, or referred to
    * in the LTL formulas. Must be overridden by generated specification specific monitor.
    *
    * @return the set of referred to events.
    */

  def eventsInSpec: Set[String]

  /**
    * Used for timing performance. The timing is printed on standard output.
    *
    * @param block the code block that is being timed.
    * @tparam R the result type of the block.
    * @return the result of the block.
    */

  def time[R](block: => R): R = {
    val t1 = System.currentTimeMillis()
    val result = block
    val t2 = System.currentTimeMillis()
    val ms = (t2 - t1).toFloat
    val sec = ms / 1000
    println()
    println("Elapsed analysis time: " + sec + "s")
    result
  }

  /**
    * Submits an event to the monitor. This again causes the monitor evaluation to be
    * performed, which will evaluate all asserted formulas on this new event.
    *
    * @param name the name of the event.
    * @param args the arguments to the event.
    */

  def submit(name: String, args: List[Any]): Unit = {
    if (Options.STATISTICS) {
      statistics.upddate(name)
    }
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
    end()
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
    val timed: Boolean = file.contains(".timed.")
    var eventSize: Int = 0
    time {
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
        if (timed) {
          eventSize = record.size() - 1
          val timeStamp: Int = record.get(eventSize).toInt
          setTime(timeStamp)
        } else {
          eventSize = record.size()
        }
        for (i <- 1 until eventSize) {
          args += record.get(i)
        }
        submit(name, args.toList)
      }
      println(s"Processed $lineNr events")
      in.close()
      end()
    }
  }

  /**
    * Called at the end of a trace analysis. Only called in connection of
    * log analysis (analysis of finite traces).
    */

  def end(): Unit = {
    println(s"\n$errors errors detected!\n")
    if (Options.STATISTICS) println(statistics)
    if (garbageWasCollected) {
      println("\n*** GARBAGE COLLECTOR WAS ACTIVATED!")
    } else {
      println("\n- Garbage collector was not activated")
    }
  }

  /**
    * Evaluates all formulas on a new state (new event). In case a property is violated an
    * error message is printed. There is currently no other consequence of a violated
    * property.
    */

  def evaluate(): Unit = {
    debug(s"\ncurrentTime = $currentTime\n$state\n")
    for (formula <- formulae) {
      formula.setTime(deltaTime)
      if (!formula.evaluate()) {
        errors += 1
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
    writelnResult(lineNr)
  }

  /**
    * Prints information useful for understanding the data written to the profile CSV file.
    */

  def printProfileHeader(): Unit = {
    formulae(0).printProfileHeader()
  }
}

/**
  * Every formula will be defined as a class extending this class.
  *
  */

abstract class Formula(val monitor: Monitor) {
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
  // indices of temporal formulas, used for computing free assignments during garbage collection:
  val indices: List[Int]
  // stores variable-value-bdd pairs of newly detected values for most recent event, null means no relations:
  val emptyTouchedSet: Set[(String, Any, BDD)] = Set()
  var touchedByLastEvent: Set[(String, Any, BDD)] = emptyTouchedSet
  // records variables referred to in relations. Used to pre-condition update of above variable:
  var varsInRelations: Set[String] = Set()

  /**
    * Type of relational operators.
    */

  trait RelOp {
    def compare(v1: Any, v2: Any): Boolean
  }

  /**
    * The '<' relational operator.
    */

  case object LTOP extends RelOp {
    def compare(v1: Any, v2: Any): Boolean = {
      v1.asInstanceOf[String].toInt < v2.asInstanceOf[String].toInt
    }

    override def toString = "<"
  }

  /**
    * The '<=' relational operator.
    */

  case object LEOP extends RelOp {
    def compare(v1: Any, v2: Any): Boolean = {
      v1.asInstanceOf[String].toInt <= v2.asInstanceOf[String].toInt
    }

    override def toString = "<="
  }

  /**
    * The '>' relational operator.
    */

  case object GTOP extends RelOp {
    def compare(v1: Any, v2: Any): Boolean = {
      v1.asInstanceOf[String].toInt > v2.asInstanceOf[String].toInt
    }

    override def toString = ">"
  }

  /**
    * The '>=' relational operator.
    */

  case object GEOP extends RelOp {
    def compare(v1: Any, v2: Any): Boolean = {
      v1.asInstanceOf[String].toInt >= v2.asInstanceOf[String].toInt
    }

    override def toString = ">="
  }

  /**
    * The '=' relational operator.
    */

  case object EQOP extends RelOp {
    def compare(v1: Any, v2: Any): Boolean = {
      v1 == v2
    }

    override def toString = "="
  }

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
    * Builds a BDD from an event pattern, matching it against the latest
    * incoming event in the current state. A particular event pattern either matches the
    * current event or not. If so, values are bound to formal parameter names of the event,
    * forming a binding (assignment). The BDD is then created from this binding.
    *
    * @param name     the name of the event.
    * @param patterns the patterns that are meant to match the arguments of the actual event.
    * @return the BDD resulting from the match, False if no match occurred.
    */

  def build(name: String)(patterns: Pattern*): BDD =
    bddFromBinding(monitor.state.holds(name, patterns.toList))

  /**
    * Builds a BDD from a relational expression of the form: <code>varName1 op varName2</code>, only
    * comparing the new values seen in this event.
    *
    * @param varName1 the name of the left-hand side variable.
    * @param op       the operator.
    * @param varName2 the name of the right-hand side variable.
    * @return the BDD resulting from comparing new values against previous values.
    */

  def relation(varName1: String, op: RelOp, varName2: String): BDD = {
    val variable1 = bddGenerator.varMap(varName1)
    val variable2 = bddGenerator.varMap(varName2)
    var result: BDD = bddGenerator.False
    for ((varName, value, bdd) <- touchedByLastEvent) {
      if (varName == varName1) {
        for ((value2, bdd2) <- variable2.bdds) {
          if (op.compare(value, value2)) {
            result = result.or(bdd.and(bdd2))
            variable1.inRelation(bdd)
            variable2.inRelation(bdd2)
            debug(s"adding [$varName1:$value] $op $varName2:$value2 to '$varName1 $op $varName2'  BDD")
          }
        }
      }
      if (varName == varName2) {
        for ((value1, bdd1) <- variable1.bdds) {
          if (op.compare(value1, value)) {
            result = result.or(bdd.and(bdd1))
            variable1.inRelation(bdd1)
            variable2.inRelation(bdd)
            debug(s"adding $varName1:$value1 $op [$varName2:$value] to '$varName1 $op $varName2'  BDD")
          }
        }
      }
    }
    result
  }

  /**
    * Builds a BDD from a relational expression of the form: <code>varName op const</code>, only
    * comparing the new values seen in this event.
    *
    * @param varName the name of the left-hand side variable.
    * @param op      the operator.
    * @param const   the right-hand side constant.
    * @return the BDD resulting from comparing new value against the constant.
    */

  def relationToConstant(varName: String, op: RelOp, const: Any): BDD = {
    val variable = bddGenerator.varMap(varName)
    var result: BDD = bddGenerator.False
    for ((`varName`, value, bdd) <- touchedByLastEvent if op.compare(value, const)) {
      result = result.or(bdd)
      variable.inRelation(bdd)
      debug(s"adding [$varName:$value] $op $const to '$varName $op $const'  BDD")
    }
    result
  }

  /**
    * If the formula contains relations (<code>touchedByLastEvent != null</code>), this function
    * adds a newly generated binding of a variable to a value and binding. This is used for updating
    * the relational expressions.
    *
    * @param name  the name of the variable.
    * @param value the value it is bound to.
    * @param bdd   the corresponding BDD generated.
    */

  def addTouchedByLastEvent(name: String, value: Any, bdd: BDD): Unit = {
    if (varsInRelations.contains(name)) {
      touchedByLastEvent += ((name, value, bdd))
      debug(s"recording binding $name -> $value for subsequent relation updating")
    }
  }

  /**
    * Adds a time value `d` to a time value `t`, resulting in the new time value `u` using
    * carrier bits `c` as auxiliary variables.
    *
    * Time values are represented by BDDs. Each such BDD, call it  `B`, represents a sequence of
    * bits `B1,...,Bn` mentioned from least significant bit to most significant bit. This
    * allows a recursive algorithm, which adds bits from lowest to highest significant bit.
    * The `c` the carrier bits used to carry over.
    *
    * E.g. say we want to add `t=01`` (the number 1) and `d=01` (the number 1). These are
    * passed to this function as `10` and `10` respectively (least significant bits first).
    * The function adds `1` and `1` giving `0` and resulting in carrier bit `c1` being `1`.
    * `c1=1` is then used when adding the two `0` resulting in `1`, overall resulting in `01`
    * with the least significant but mentioned first, hence this is
    * `10` in normal bit format (the number 2).
    *
    * The function takes care of the first (least significant) bit addition, and then calls
    * `addConstRest` for the rest of the bits.
    *
    * @param t the time value to add to (from previous event).
    * @param u the resulting time value.
    * @param d the time delta to add to `t` (the time difference between this and previous event).
    * @param c the carrier bits used as auxiliary variable.
    * @return the BDD defining the result `u` of the addition.
    **/

  def addConst(t: List[BDD], u: List[BDD], d: List[BDD], c: List[BDD]): BDD = {
    (t, u, d, c) match {
      case (t_bit :: t_rest, u_bit :: u_rest, d_bit :: d_rest, c_bit :: c_rest) =>
        val initBDD = u_bit.biimp(t_bit.xor(d_bit))
        val initCarrier = c_bit.biimp(t_bit.and(d_bit))
        initBDD.and(initCarrier).and(addConstRest(t_rest, u_rest, d_rest, c_bit :: c_rest))
      case _ => assert(false, "addConst pattern match fails").asInstanceOf[BDD]
    }
  }

  /**
    * Adds a time value `d` to a time value `t`, resulting in the new time value `u` using
    * carrier bits `c` as auxiliary variables.
    *
    * This function is called on all bits following the least significant bit. See
    * `addConst`.
    *
    * @param t the time value to add to (from previous event).
    * @param u the resulting time value.
    * @param d the time delta to add to `t` (the time difference between this and previous event).
    * @param c the carrier bits used as auxiliary variable.
    * @return the BDD defining the result `u` of the addition.
    */

  def addConstRest(t: List[BDD], u: List[BDD], d: List[BDD], c: List[BDD]): BDD = {
    (t, u, d, c) match {
      case (Nil, Nil, Nil, _) => bddGenerator.True
      case (t_bit :: t_rest, u_bit :: u_rest, d_bit :: d_rest, c_prev :: c_cur :: c_rest) =>
        val u_bit_def = u_bit.biimp(t_bit.xor(d_bit).xor(c_prev))
        val c_cur_def = c_cur.biimp(
          (t_bit.and(d_bit)).or(
            t_bit.and(c_prev).or(
              d_bit.and(c_prev)
            )
          ))
        u_bit_def.and(c_cur_def.and(addConstRest(t_rest, u_rest, d_rest, c_cur :: c_rest)))
      case _ => assert(false, "addConstRest pattern match fails").asInstanceOf[BDD]
    }
  }

  /**
    * Returns true of the first `bit1` is one and the second `bit2` is zero.
    *
    * @param bit1 the first bit.
    * @param bit2 the second bit.
    * @return the `True` BDD if the first bit is one and the second is zero.
    */

  def gtBit(bit1: BDD, bit2: BDD): BDD =
    bit1.and(bit2.not())

  /**
    * Determines whether one time value `u` is strictly bigger than another `l`.
    * The time values are presented with the most significant bit first, and the
    * function recurses over the bits comparing them until the result becomes
    * obvious.
    *
    * E.g. to compare binary `101` (number 5) to binary `110` (number 6) the
    * function first compares the first two `1`s, which does not determine the
    * result. It then moves on to the next two bits `0` and `1`, and here it becomes
    * clear that the first number is not bigger than the second.
    *
    * @param u the first time value (the time difference of the current event)
    * @param l the second time value (the limit constant associated with the S-operator)
    * @return the result of the comparison, `True` if the first number is bigger than the second.
    *         Otherwise `False`.
    */

  def gtConst(u: List[BDD], l: List[BDD]): BDD = {
    (u, l) match {
      case (Nil, Nil) => bddGenerator.False
      case (u_bit :: u_rest, l_bit :: l_rest) =>
        gtBit(u_bit, l_bit).ite(
          bddGenerator.True,
          gtBit(l_bit, u_bit).ite(
            bddGenerator.False,
            gtConst(u_rest, l_rest)
          )
        )
      case _ => assert(false, "gtConst pattern match fails").asInstanceOf[BDD]
    }
  }

  /**
    * From a list of BDD variable-numbers (the JavaBDD package represents a
    * variable by a number), the function returns a list of the BDDs, one for
    * each of these variables. The BDD returns `1` for `1` and `0` for `0`.
    *
    * @param positions the numbers of the variables.
    * @return the corresponding one-bit BDDs.
    */

  def generateBDDList(positions: Array[Int]): List[BDD] = {
    for (pos <- positions.toList) yield bddGenerator.theOneBDDFor(pos)
  }

  /**
    * Sets the time delta in the individual formula. Note that the delta stored in the
    * individual formula is a function of the maximal time limit occurring in
    * the formula, in order to save bits. It is meant to be overridden by each
    * formula class if the formula contains time constraints.
    *
    * @param actualDelta the actual difference in time between the timestamp of
    *                    the previous event and the current event.
    */

  def setTime(actualDelta: Int) {}

  /**
    * Returns the True BDD if the Delta time (the time difference between the
    * time stamp of the current event and the time stamp of the previous event)
    * is less than the time limit passed as argument, otherwise the False BDD
    * is returned.
    *
    * @param timeLimit the timelimit (small delta) occuring as part of a temporal
    *                  operator in the property being evaluated.
    * @return the True or False BDD, depending on whether the time passed since last
    *         event is less than the argument time value or not.
    */

  def deltaLessThanTimeLimit(timeLimit: Int): BDD = {
    if (monitor.deltaTime < timeLimit)
      bddGenerator.True
    else
      bddGenerator.False
  }

  /**
    * Declares all variables (each identified by a name) in a formula.
    * This includes initializing the BDD generator, which is stored in
    * <code>bddGenerator</code>, and initializing <code>True</code> and
    * <code>False</code>. The result returned is a list of the Variable objects.
    * In addition BDD variables are allocated for keeping track of time in case
    * the property contains timed temporal operators. In this case `bitsPerTimeVar > 0`.
    *
    * @param variables      the (name,bounded) pairs for variables in a formula.
    * @param bitsPerTimeVar the number of bits to be allocated per time variable.
    *                       This number is `0` if the property does not contain
    *                       timed temporal operators.
    * @return a list of Variable objects, one for each variable.
    */

  def declareVariables(variables: (String, Boolean)*)(bitsPerTimeVar: Int): List[Variable] = {
    val variableList = variables.toList
    val nameList: List[String] = variableList.map(_._1)
    val varsAndBitsPerVar = variableList.map {
      case (n, b) => (n, b, Options.BITS)
    }
    bddGenerator = new BDDGenerator(this)(varsAndBitsPerVar, bitsPerTimeVar)
    bddGenerator.initializeVariables()
    nameList.map(bddGenerator.varMap(_))
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
    * Prints information useful for understanding what is written to the profile CSV file.
    */

  def printProfileHeader(): Unit = {
    println()
    println("================")
    println(s"Property: $name")
    println("================")
    println()
    println("Profile data written to CSV file dejavu-profile.csv")
    println()
    println("Formulas:")
    println()
    // --- profiling: ---
    // val profiledIndices = indices ++ List(4) // access property
    // val profiledIndices = List(25) // fifo property
    // val profiledIndices = List(7) // locking property
    // val profiledIndices = List(13,16) // deadlock property
    // val profiledIndices = List(4,20) // datarace property
    val profiledIndices = indices
    // ------------------
    for (i <- profiledIndices) {
      println(s"----- $i -----")
      println(txt(i))
      writeProfile(s"nodeCount$i,pathCount$i,satCount$i,compr$i,")
    }
    writelnProfile()
  }

  /**
    * Prints a formula state for debugging. This includes whether the formula is true or not,
    * and the value of the <code>now</code> array, where each entry is printed both as a one
    * line text value, and also as a graph in dot format for visualization with GraphViz.
    *
    * In profile mode, profiling data are written to a CSV file.
    */

  def debugMonitorState(): Unit = {
    if (Options.PROFILE) { // Designed to profile one formula.
      val line = new StringBuffer()
      // --- profiling: ---
      // val profiledIndices = indices ++ List(4) // access property
      // val profiledIndices = List(25) // fifo property
      // val profiledIndices = List(7) // locking property
      // val profiledIndices = List(13,16) // deadlock property
      // val profiledIndices = List(4,20) // datarace property
      val profiledIndices = indices
      // ------------------
      for (i <- profiledIndices) {
        val bdd: BDD = now(i)
        val nodeCount: Int = bdd.nodeCount()
        val pathCount: Double = bdd.pathCount()
        val satCount: Double = bdd.satCount()
        val compression: Double = if (nodeCount != 0) satCount / nodeCount else 0
        // val varProfile: Array[Int] = bdd.varProfile()

        line.append(s"$nodeCount,$pathCount,$satCount,$compression,")
      }
      writelnProfile(line)
    }
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



/*
  prop P4 : Forall A . Forall B . Forall C . Forall da . Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da))))) 
*/

class Formula_P4(monitor: Monitor) extends Formula(monitor) {
          
  override def evaluate(): Boolean = {
    // assignments1 (leaf nodes that are not rule calls):
      now(10) = build("end")(V("B"))
      now(14) = build("end")(V("A"))
      now(18) = build("begin")(V("B"),V("db"))
      now(21) = build("begin")(V("A"),V("da"))
      now(24) = build("end")(V("C"))
      now(28) = build("end")(V("B"))
      now(32) = build("begin")(V("C"),V("dc"))
      now(35) = build("begin")(V("B"),V("db"))
      now(39) = build("end")(V("C"))
      now(43) = build("end")(V("A"))
      now(47) = build("begin")(V("C"),V("dc"))
      now(50) = build("begin")(V("A"),V("da"))
    // assignments2 (rule nodes excluding what is below @ and excluding leaf nodes):
    // assignments3 (rule calls):
    // assignments4 (the rest of rules that are below @ and excluding leaf nodes):
    // assignments5 (main formula excluding leaf nodes):
      now(20) = now(21).or(pre(20))
      now(19) = pre(20)
      now(17) = now(18).and(now(19))
      now(16) = now(17).or(pre(16))
      now(15) = pre(16)
      now(13) = now(14).and(now(15))
      now(12) = now(13).or(pre(12))
      now(11) = pre(12)
      now(9) = now(10).and(now(11))
      now(8) = now(9).or(pre(8))
      now(34) = now(35).or(pre(34))
      now(33) = pre(34)
      now(31) = now(32).and(now(33))
      now(30) = now(31).or(pre(30))
      now(29) = pre(30)
      now(27) = now(28).and(now(29))
      now(26) = now(27).or(pre(26))
      now(25) = pre(26)
      now(23) = now(24).and(now(25))
      now(22) = now(23).or(pre(22))
      now(7) = now(8).and(now(22))
      now(49) = now(50).or(pre(49))
      now(48) = pre(49)
      now(46) = now(47).and(now(48))
      now(45) = now(46).or(pre(45))
      now(44) = pre(45)
      now(42) = now(43).and(now(44))
      now(41) = now(42).or(pre(41))
      now(40) = pre(41)
      now(38) = now(39).and(now(40))
      now(37) = now(38).or(pre(37))
      now(36) = now(37).not()
      now(6) = now(7).not().or(now(36))
      now(5) = now(6).forAll(var_dc.quantvar)
      now(4) = now(5).forAll(var_db.quantvar)
      now(3) = now(4).forAll(var_da.quantvar)
      now(2) = now(3).forAll(var_C.quantvar)
      now(1) = now(2).forAll(var_B.quantvar)
      now(0) = now(1).forAll(var_A.quantvar)

    debugMonitorState()

    val error = now(0).isZero
    if (error) monitor.recordResult()
    tmp = now
    now = pre
    pre = tmp
    touchedByLastEvent = emptyTouchedSet
    !error
  }

  val var_A :: var_B :: var_C :: var_da :: var_db :: var_dc :: Nil = declareVariables(("A",false), ("B",false), ("C",false), ("da",false), ("db",false), ("dc",false))(0)

  varsInRelations = Set()
  val indices: List[Int] = List(37,40,41,44,45,48,49,22,25,26,29,30,33,34,8,11,12,15,16,19,20)

  pre = Array.fill(51)(bddGenerator.False)
  now = Array.fill(51)(bddGenerator.False)

  txt = Array(
    "Forall A . Forall B . Forall C . Forall da . Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "Forall B . Forall C . Forall da . Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "Forall C . Forall da . Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "Forall da . Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "Forall db . Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "Forall dc . ((P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))))",
      "(P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))) -> !P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da))))",
      "P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))) & P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))",
      "P (end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da))))",
      "end(B) & @ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))",
      "end(B)",
      "@ P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))",
      "P (end(A) & @ P (begin(B,db) & @ P begin(A,da)))",
      "end(A) & @ P (begin(B,db) & @ P begin(A,da))",
      "end(A)",
      "@ P (begin(B,db) & @ P begin(A,da))",
      "P (begin(B,db) & @ P begin(A,da))",
      "begin(B,db) & @ P begin(A,da)",
      "begin(B,db)",
      "@ P begin(A,da)",
      "P begin(A,da)",
      "begin(A,da)",
      "P (end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db))))",
      "end(C) & @ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db)))",
      "end(C)",
      "@ P (end(B) & @ P (begin(C,dc) & @ P begin(B,db)))",
      "P (end(B) & @ P (begin(C,dc) & @ P begin(B,db)))",
      "end(B) & @ P (begin(C,dc) & @ P begin(B,db))",
      "end(B)",
      "@ P (begin(C,dc) & @ P begin(B,db))",
      "P (begin(C,dc) & @ P begin(B,db))",
      "begin(C,dc) & @ P begin(B,db)",
      "begin(C,dc)",
      "@ P begin(B,db)",
      "P begin(B,db)",
      "begin(B,db)",
      "!P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da))))",
      "P (end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da))))",
      "end(C) & @ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))",
      "end(C)",
      "@ P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))",
      "P (end(A) & @ P (begin(C,dc) & @ P begin(A,da)))",
      "end(A) & @ P (begin(C,dc) & @ P begin(A,da))",
      "end(A)",
      "@ P (begin(C,dc) & @ P begin(A,da))",
      "P (begin(C,dc) & @ P begin(A,da))",
      "begin(C,dc) & @ P begin(A,da)",
      "begin(C,dc)",
      "@ P begin(A,da)",
      "P begin(A,da)",
      "begin(A,da)"
  )

  debugMonitorState()
}
        
/* The specialized Monitor for the provided properties. */

class PropertyMonitor extends Monitor {
  def eventsInSpec: Set[String] = Set("end","begin")

  formulae ++= List(new Formula_P4(this))
}
      
object TraceMonitor {
  def main(args: Array[String]): Unit = {
    if (1 <= args.length && args.length <= 3) {
      if (args.length > 1) Options.BITS = args(1).toInt
      val m = new PropertyMonitor
      val file = args(0)
      if (args.length == 3 && args(2) == "debug") Options.DEBUG = true
      if (args.length == 3 && args(2) == "profile") Options.PROFILE = true
      try {
        openResultFile("dejavu-results")
        if (Options.PROFILE) {
          openProfileFile("dejavu-profile.csv")
          m.printProfileHeader()
        }
        m.submitCSVFile(file)
      } catch {
          case e: Throwable =>
            println(s"\n*** $e\n")
            // e.printStackTrace()
      } finally {
        closeResultFile()
        if (Options.PROFILE) closeProfileFile()
      }
    } else {
      println("*** call with these arguments:")
      println("<logfile> [<bits> [debug|profile]]")
    }
  }
}
      
