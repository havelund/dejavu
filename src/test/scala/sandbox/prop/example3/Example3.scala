package sandbox.prop.example3

import sandbox.prop.Monitor

// ----------------
// Generated code :
// ----------------

class PropertyMonitor extends Monitor {

  class Formula_p extends Formula {
    var pre : Array[Boolean] = null
    var now : Array[Boolean] = null
    var latest: Array[Boolean] = null
    var txt: Array[String] = null

    override def evaluate(): Boolean = {

      now(2) = state.holds("a")
      now(1) = !now(2)
      now(3) = state.holds("b")
      now(0) = now(3) || (now(1) && pre(0))

      println(this)
      latest = now
      now = pre
      pre = latest
      latest(0)
    }

    pre = Array.fill(4)(false)
    now = Array.fill(4)(false)
    txt = Array("!a(1) S b(1)","!a(1)","a(1)","b(1)")

    println(this)
  }

  formulae ++= List(new Formula_p)


  class Formula_q extends Formula {
    var pre : Array[Boolean] = null
    var now : Array[Boolean] = null
    var latest: Array[Boolean] = null
    var txt: Array[String] = null

    override def evaluate(): Boolean = {

      now(1) = state.holds("a")
      now(5) = state.holds("a")
      now(4) = !now(5)
      now(6) = state.holds("b")
      now(3) = now(6) || (now(4) && pre(3))
      now(2) = pre(3)
      now(0) = !now(1) || now(2)

      println(this)
      latest = now
      now = pre
      pre = latest
      latest(0)
    }

    pre = Array.fill(7)(false)
    now = Array.fill(7)(false)
    txt = Array("a(1) -> P(!a(1) S b(1))","a(1)","P(!a(1) S b(1))","!a(1) S b(1)","!a(1)","a(1)","b(1)")

    println(this)
  }

  formulae ++= List(new Formula_q)

}

// --------------
// Main program :
// --------------

object Main {
  def main(args: Array[String]): Unit = {
    val m = new PropertyMonitor
    m.submit("b")
    m.submit("c")
    m.submit("c")
    m.submit("c")
    m.submit("a")
  }
}