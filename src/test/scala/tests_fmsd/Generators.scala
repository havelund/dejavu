package tests_fmsd

object Util {
  val dir = "/Users/khavelun/Desktop/"
}
import Util._
import tests.util.tracegenerator.Generator

class Generator1 extends Generator(dir) {
  /*
    Property 1:

    prop access :
      Forall user . Forall file .
        access(user,file) ->
          [login(user),logout(user))
            &
          [open(file),close(file))
   */

  val login = "login"
  val logout = "logout"
  val open = "open"
  val close = "close"
  val access = "access"

  def run(): Unit = {
    // val MAX = 5000; val MID = 4800 // 11,006 events generated
    // val MAX = 50000; val MID = 48000 // 110,006 events generated
    val MAX = 500000; val MID = 480000 // 1,100,006 events generated

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

class Generator2 extends Generator(dir) {
  /*
    Property 2:

    prop file : Forall f . close(f) -> Exists m . @ [open(f,m),close(f))
   */

  val open = "open"
  val close = "close"
  val read = "read"
  val write = "write"

  def run(): Unit = {
    // val MAX = 8000; val MID = 7000 // 11,004 events generated
    // val MAX = 80000; val MID = 70000 // 110,004 events generated
    val MAX = 800000; val MID = 700000 // 1,100,004 events generated

    up(1, MAX)(open,read)
    repeat(1) {
      down(MAX,MID)(close)
      up(MID, MAX)(open,write)
    }
    down(MAX, MID)(close)
    emit(close,MAX)
    end()
  }
}

class Generator3 extends Generator(dir) {
  /*
    Property 3:

    prop fifo :
      Forall x .
        (enter(x) -> ! @ P enter(x)) &
        (exit(x) -> ! @ P exit(x)) &
        (exit(x) -> @ P enter(x)) &
        (Forall y . (exit(y) & P (enter(y) & @ P enter(x))) -> @ P exit(x))
   */

  val enter = "enter"
  val exit = "exit"

  def run(): Unit = {
    // val MAX = 5000; val MID = 50 // 5,050 events generated
    // val MAX = 10000; val MID = 100 // 10,100 events generated
    // val MAX = 100000; val MID = 1000 // 101,000 events generated
    val MAX = 1000000; val MID = 10000 // 1,010,000 events generated
    up(1, MAX)(enter)
    up(1, MID)(exit)
    emit(exit,MAX)
    end()
  }
}

class Generator4 extends Generator(dir) {
  /*
  Property 4

  prop basic :
    Forall t . Forall l .
      (
        (sleep(t) -> ![acq(t,l),rel(t,l))) &
        (acq(t,l) -> ! exists s . @ [acq(s,l),rel(s,l))) &
        (rel(t,l) -> @ [acq(t,l),rel(t,l)))
      )
   */

  val acq = "acq"
  val rel = "rel"
  val sleep = "sleep"
  val read = "read"
  val write = "write"

  def run(): Unit = {
    // --- TESTING: ---
    // val REPEAT = 2; val THREADS = 2; val LOCKS = 3
    // ---------------------------------------------------
    // val REPEAT = 2; val THREADS = 800; val LOCKS = 3 // 10401 events
    // val REPEAT = 5; val THREADS = 5000; val LOCKS = 2 // 105001 events
    val REPEAT = 1400; val THREADS = 125; val LOCKS = 3 // 1050126 events
    // ===================================================
    var locked: Map[Int, Set[Int]] = Map()

    def addLock(t: Int, l: Int): Unit = {
      locked.get(t) match {
        case None =>
          locked += (t -> Set(l))
        case Some(s) =>
          locked += (t -> (s + l))
      }
    }

    def remLock(t: Int, l: Int): Unit = {
      locked = locked + (t -> (locked(t) - l))
    }

    def cantor(i1: Int, i2: Int): Float = {
      val k1 = i1.toFloat
      val k2 = i2.toFloat
      (k1 + k2)/2 * (k1 + k2 + 1) + k2
    }

    val DEBUG = false

    def debug(s:String): Unit = {
      if(DEBUG) println(s)
    }

    var startLock : Int = 0

    for (r <- 1 to REPEAT) {
      debug(s"@@@ REPEAT $r @@@")
      startLock = 1
      locked = Map()
      debug("### ACQUIRING LOCKS AND RELEASING SOME")
      for (i <- 1 to THREADS) {
        debug(s"=== $i ===")
        val endLock = startLock + LOCKS - 1
        for (j <- startLock to endLock) {
          emit(acq, i, j)
          addLock(i, j)
        }
        val midLock = (startLock + endLock) / 2
        debug("---")
        for (j <- endLock to midLock by -1) {
          emit(rel, i, j)
          remLock(i, j)
        }
        startLock = midLock
      }
      debug("### RELEASING REMAINING LOCKS ###")
      for (t <- locked.keySet) {
        debug(s"=== $t ===")
        for (l <- locked(t)) {
          emit(rel, t, l)
        }
      }
    }
    debug("### SLEEP ###")
    for (t <- 1 to THREADS) {
      emit(sleep,t)
    }
    debug("### ERRORS ###")
    debug("=== release lock not held")
    emit(rel,1,0)
    end()
  }
}

class Generator5 extends Generator(dir) {
  /*
  Property 5:

  prop cycles :
    Forall t1 . Forall t2 . Forall l1 . Forall l2 .
      (@ [acq(t1,l1),rel(t1,l1)) & acq(t1,l2))
      ->
      (! @ P (@ [acq(t2,l2),rel(t2,l2)) & acq(t2,l1)))
   */

  val acq = "acq"
  val rel = "rel"
  val sleep = "sleep"
  val read = "read"
  val write = "write"

  def run(): Unit = {
    // --- TESTING: ---
    // val REPEAT = 2; val THREADS = 2; val LOCKS = 3
    // ---------------------------------------------------
    // val REPEAT = 2; val THREADS = 800; val LOCKS = 3 // 10409 events
    // val REPEAT = 5; val THREADS = 5000; val LOCKS = 2 // 100008 events
    val REPEAT = 1400; val THREADS = 125; val LOCKS = 3 // 1050008 events
    // ===================================================
    var locked: Map[Int, Set[Int]] = Map()

    def addLock(t: Int, l: Int): Unit = {
      locked.get(t) match {
        case None =>
          locked += (t -> Set(l))
        case Some(s) =>
          locked += (t -> (s + l))
      }
    }

    def remLock(t: Int, l: Int): Unit = {
      locked = locked + (t -> (locked(t) - l))
    }

    def cantor(i1: Int, i2: Int): Float = {
      val k1 = i1.toFloat
      val k2 = i2.toFloat
      (k1 + k2)/2 * (k1 + k2 + 1) + k2
    }

    val DEBUG = false

    def debug(s:String): Unit = {
      if(DEBUG) println(s)
    }

    var startLock : Int = 0

    for (r <- 1 to REPEAT) {
      debug(s"@@@ REPEAT $r @@@")
      startLock = 1
      locked = Map()
      debug("### ACQUIRING LOCKS AND RELEASING SOME")
      for (i <- 1 to THREADS) {
        debug(s"=== $i ===")
        val endLock = startLock + LOCKS - 1
        for (j <- startLock to endLock) {
          emit(acq, i, j)
          addLock(i, j)
        }
        val midLock = (startLock + endLock) / 2
        debug("---")
        for (j <- endLock to midLock by -1) {
          emit(rel, i, j)
          remLock(i, j)
        }
        startLock = midLock
      }
      debug("### RELEASING REMAINING LOCKS ###")
      for (t <- locked.keySet) {
        debug(s"=== $t ===")
        for (l <- locked(t)) {
          emit(rel, t, l)
        }
      }
    }
    debug("### ERRORS ###")
    debug("=== deadlock ===")
    emit(acq,1,10)
    emit(acq,1,20)
    emit(rel,1,20)
    emit(rel,1,10)
    emit(acq,2,20)
    emit(acq,2,10)
    emit(rel,2,10)
    emit(rel,2,20)
    end()
  }
}

class Generator6 extends Generator(dir) {
  /*
  Property 6:

  prop datarace :
    Forall t1 . Forall t2 . Forall x .
      (
        (P (read(t1,x) | write(t1,x)))
        &
        (P write(t2,x))
      )
      ->
      Exists l .
        (
          H ((read(t1,x) | write(t1,x)) -> [acq(t1,l),rel(t1,l)))
          &
          H ((read(t2,x) | write(t2,x)) -> [acq(t2,l),rel(t2,l)))
        )
   */

  val acq = "acq"
  val rel = "rel"
  val sleep = "sleep"
  val read = "read"
  val write = "write"

  def run(): Unit = {
    // val REPEAT = 2; val THREADS = 2; val LOCKS = 3; val VARS = 1
    // -----------------------------------------------------------------
    // val REPEAT = 1; val THREADS = 50; val LOCKS = 50; val VARS = 50 // 9608 events
    // val REPEAT = 1; val THREADS = 1000; val LOCKS = 40; val VARS = 10 // 100005 events
    val REPEAT = 35; val THREADS = 500; val LOCKS = 20; val VARS = 10 // 1050005 events
    // =================================================================

    val DEBUG = false

    def debug(s:String): Unit = {
      if(DEBUG) println(s)
    }

    for (r <- 1 to REPEAT) {
      debug(s"### REPEAT $r ###")
      for (t <- 1 to THREADS) {
        debug(s"=== THREAD $t ===")
        for (l <- 1 to LOCKS) {
          emit(acq,t,l)
        }
        debug(s"--- release ---")
        for (l <- LOCKS to 2 by -1) {
          emit(rel,t,l)
        }
        debug(s"--- readin/write ---")
        for (v <- 1 to VARS) {
          emit(read,t,v)
          emit(write,t,v)
        }
        debug(s"--- release last ---")
        emit(rel,t,1)
      }
    }
    debug("--- datarace ---")
    emit(acq,1,1)
    emit(read,1,-1)
    emit(rel,1,1)
    emit(acq,2,2)
    emit(write,2,-1)
    end()
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val generator = new Generator5
    generator.run()
  }
}

/**************************/
/*** UNUSED GENERATORS: ***/
/**************************/

class Generator_NOTUSED_1 extends Generator(dir) {
  /*

  prop unsafeMapIt :
    Forall i . Forall c . Forall m .
      (next(i) & P (iterator(c,i) & P create(m,c)))
      ->
      (!update(m) S iterator(c,i))
   */

  val create = "create"
  val iterator = "iterator"
  val next = "next"
  val update = "update"

  def co(it: Int) = it * 10
  def it(it: Int) = it * -10

  def run(): Unit = {
    val MAX = 2700 // 10801 events generated, spec1: 25.82s, spec2: 7.684s
    // val MAX = 25000 // 100001 events generated, spec1: 859.619s, spec2: 581.012s

    for (i <- 1 to MAX) emit(create,i, co(i))
    for (i <- 1 to MAX) emit(iterator,co(i),it(i))
    for (i <- 1 to MAX) emit(next, it(i))
    for (i <- 1 to MAX/2) emit(update, i)
    for (i <- MAX/2 + 1 to MAX) emit(next,it(i))
    emit(next,it(1))
    end()
  }
}

class Generator_NOTUSED_2 extends Generator(dir) {
  /*

  prop datarace :
  Forall t1 . Forall t2 . Forall x .
    (
      (P (read(t1,x) | write(t1,x)))
      &
      (P write(t2,x))
    )
    ->
    Exists l .
      (
        H ((read(t1,x) | write(t1,x)) -> [acq(t1,l),rel(t1,l)))
        &
        H ((read(t2,x) | write(t2,x)) -> [acq(t2,l),rel(t2,l)))
      )
   */

  val acq = "acq"
  val rel = "rel"
  val sleep = "sleep"
  val read = "read"
  val write = "write"

  def run(): Unit = {
    // --- TESTING: ---
    //val REPEAT = 2; val THREADS = 2; val LOCKS = 3; val VARS = 1
    val REPEAT = 350; val THREADS = 500

    def cantor(i1: Int, i2: Int): Float = {
      val k1 = i1.toFloat
      val k2 = i2.toFloat
      (k1 + k2)/2 * (k1 + k2 + 1) + k2
    }

    def access(t: Int, obj: Float): Unit = {
      emit(acq, t, obj)
      emit(write, t, obj)
      emit(rel, t, obj)
    }

    val DEBUG = true

    def debug(s:String): Unit = {
      if(DEBUG) println(s)
    }

    for (r <- 1 to REPEAT) {
      debug(s"@@@ REPEAT $r @@@")
      for (t <- 1 to THREADS) {
        debug(s"=== $t ===")
        if (t > 1) {
          val obj = cantor(t - 1, t)
          access(t, obj)
        }
        val obj = cantor(t, t + 1)
        access(t,obj)
      }
    }
    end()
  }
}