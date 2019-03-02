package sandbox.prop.example2

import java.util

import sandbox.prop.Monitor

/*
 property P :
   logout -> prev(!logout S login)

                   .. (6)login
  .. (2)prev .. (3)S
                   .. (4)! .. (5)logout
(0)->
  .. (1)logout
 */

class MyMonitor extends Monitor {
  class Formula_P extends Formula {
    var pre : Array[Boolean] = null
    var now : Array[Boolean] = null
    var latest: Array[Boolean] = null
    var txt: Array[String] = null

    override def evaluate(): Boolean = {
      now(6) = state.holds("login")
      now(5) = state.holds("logout")
      now(4) = ! now(5)
      now(3) = now(6) || (pre(3) && now(4))
      now(2) = pre(3)
      now(1) = state.holds("logout")
      now(0) = ! now(1) || now(2)
      println(this)
      latest = now
      now = pre
      pre = latest
      latest(0)
    }

    txt = Array("logout -> prev(!logout S login)",
      "logout",
      "prev(!logout S login)",
      "!logout S login",
      "!logout",
      "logout",
      "login")

    pre = new Array[Boolean](7)
    now = new Array[Boolean](7)

    util.Arrays.fill(pre,false)
    util.Arrays.fill(now,false)
    println(this)
  }

  formulae = List(new Formula_P)
}

object Main {
  def main(args: Array[String]): Unit = {
    val m = new MyMonitor
    m.submit("write")
    m.submit("read")
    m.submit("login")
    m.submit("logout")
    m.submit("logout")
    m.submit("login")
  }
}

