package tests_cav

import java.io._

object Util {
  val dir = "/Users/khavelun/downloads"
}
import tests_cav.Util._

abstract class Generator(dir: String) {
  var counter: Int = 0
  type Evr = (Int, String, List[Any])
  val pwDVU = new PrintWriter(new File(dir + "/log.timed.csv"))
  // val pwMPY = new PrintWriter(new File(dir + "/log-monpoly.txt"))

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
    // val formattedMPY = formatMPY(evr)
    println(formattedDVU)
    pwDVU.write(formattedDVU + "\n")
    // pwMPY.write(formattedMPY + "\n")
  }

  def emit(event: String, args: Any*): Unit = {
    counter += 1
    val evr = (counter, event, args.toList)
    emitEvr(evr)
  }

  def formatDVU(event: Evr): String = {
    val (counter, name, args) = event
    s"$name,${args.mkString(",")},$counter"
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
    // pwMPY.close()
  }

  def run()
}

class Generator1 extends Generator(dir) {
  /*
    Property 1:

    prop access :
      Forall user . Forall file .
        access(user,file) ->
          ((!logout(user) S login(user))
            &
           (!close(file) S open(file)))
   */

  val login = "login"
  val logout = "logout"
  val open = "open"
  val close = "close"
  val access = "access"

  def run(): Unit = {
    val MAX = 30; val MID = 20 // 11,006 events generated
    // val MAX = 5000; val MID = 4800 // 11,006 events generated
    // val MAX = 50000; val MID = 48000 // 110,006 events generated
    // val MAX = 500000; val MID = 480000 // 1,100,006 events generated

    up(1, MAX)(login)
    up(1, MAX)(open)
    repeat(1) {
      down(MAX,MID)(access,1)
      down(MAX, MID)(close)
      up(MID, MAX)(open)
    }
    down(MAX, MID)(logout)
    down(MAX, MID)(close)
    emit(access,MAX,1)
    end()
  }
}

class Generator1Low extends Generator(dir) {
  /*
    Property 1:

    prop access :
      Forall user . Forall file .
        access(user,file) ->
          ((!logout(user) S login(user))
            &
           (!close(file) S open(file)))
   */

  val login = "login"
  val logout = "logout"
  val open = "open"
  val close = "close"
  val access = "access"

  def run(): Unit = {
    // val MAX = 25; val MID = 1; val REPEAT = 80 // 10100 events generated
    // val MAX = 25; val MID = 1; val REPEAT = 800 // 100100 events generated
    // val MAX = 25; val MID = 1; val REPEAT = 8000 // 1000100 events generated

    // val MAX = 1000; val MID = 1; val REPEAT = 2 // 14000 events generated
    // val MAX = 1000; val MID = 1; val REPEAT = 20 // 104000 events generated
    val MAX = 1000; val MID = 1; val REPEAT = 200 // 1004000 events generated

    up(1, MAX)(login)
    up(1, MAX)(open)
    repeat(REPEAT) {
      up(MID,MAX)(access,1)
      down(MAX, MID)(logout)
      down(MAX, MID)(close)
      up(MID, MAX)(login)
      up(MID, MAX)(open)
    }
    down(MAX, MID)(logout)
    down(MAX, MID)(close)
    //emit(access,MAX,1)
    end()
  }
}


class Generator2 extends Generator(dir) {
  /*
    Property 2:

    prop commands :Forall m . suc(m) -> Exists p . !fail(m) S dis(m,p)
   */

  val dis = "dis"
  val suc = "suc"
  val prio1 = "1"
  val prio2 = "2"

  def run(): Unit = {
    // val MAX = 8000; val MID = 7000 // 11,004 events generated
    // val MAX = 80000; val MID = 70000 // 110,004 events generated
    val MAX = 800000; val MID = 700000 // 1,100,004 events generated

    up(1, MAX)(dis,prio1)
    repeat(1) {
      down(MAX,MID)(suc)
      up(MID, MAX)(dis,prio2)
    }
    down(MAX, MID)(suc)
    emit(suc,MAX)
    end()
  }
}

class Generator2Low extends Generator(dir) {
  /*
    Property 2:

    prop commands :Forall m . suc(m) -> Exists p . !fail(m) S dis(m,p)
   */

  val dis = "dis"
  val suc = "suc"
  val prio1 = "1"
  val prio2 = "2"

  def run(): Unit = {
    // val MAX = 25; val MID = 1; val REPEAT = 200 // 10050 events generated
    // val MAX = 25; val MID = 1; val REPEAT = 2000 // 100050 events generated
    // val MAX = 25; val MID = 1; val REPEAT = 20000 // 1000050 events generated

    // val MAX = 1000; val MID = 1; val REPEAT = 5 // 12000 events generated
    // val MAX = 1000; val MID = 1; val REPEAT = 50 // 102000 events generated
    val MAX = 1000; val MID = 1; val REPEAT = 500 // 1002000 events generated

    up(1, MAX)(dis,prio1)
    repeat(REPEAT) {
      down(MAX,MID)(suc)
      up(MID, MAX)(dis,prio2)
    }
    down(MAX, MID)(suc)
    // emit(suc,MAX)
    end()
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val generator = new Generator2Low
    generator.run()
  }
}




