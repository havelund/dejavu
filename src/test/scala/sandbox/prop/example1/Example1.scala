package sandbox.prop.example1

import sandbox.prop.Monitor

/*
 property P : logout -> past login
 */

class MyMonitor extends Monitor {
  class Formula_P extends Formula {
    var pre = new Array[Boolean](4)
    var now = new Array[Boolean](4)
    var latest: Array[Boolean] = null
    var txt: Array[String] =  Array("logout -> past login","logout","past login","login")
    pre(3) = state.holds("login")
    pre(2) = pre(3)
    pre(1) = state.holds("login")
    pre(0) = !(pre(1)) || pre(2)
    println(this)

    override def evaluate(): Boolean = {
      now(3) = state.holds("login")
      now(2) = now(3) || pre(2)
      now(1) = state.holds("logout")
      now(0) = !now(1) || now(2)
      println(this)
      latest = now
      now = pre
      pre = latest
      latest(0)
    }
  }

  formulae = List(new Formula_P)
}

object Main {
  def main(args: Array[String]): Unit = {
    val m = new MyMonitor
    m.submit("write")
    m.submit("read")
    m.submit("logout")
    m.submit("login")
  }
}
