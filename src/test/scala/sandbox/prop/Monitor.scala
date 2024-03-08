package sandbox.prop

class State {
  var current: String = null

  def update(prop: String): Unit = {
    current = prop
  }

  def holds(prop: String): Boolean =
    current == prop

  override def toString: String = {
    var result : String = ""
    result += "===============\n"
    result += s"state: $current\n"
    result += "===============\n"
    result
  }
}

class Monitor {
  val state: State = new State
  var formulae: List[Formula] = Nil

  def submit(prop: String): Unit = {
    state.update(prop)
    evaluate()
  }

  def evaluate(): Unit = {
    println(state)
    for (formula <- formulae) {
      if (!formula.evaluate()) println(s"\n*** Property ${formula.name} violated\n")
    }
  }

  abstract class Formula {
    var name: String = this.getClass.getSimpleName.split("_")(1)
    var pre: Array[Boolean]
    var now: Array[Boolean]
    var txt: Array[String]

    def evaluate(): Boolean

    override def toString: String = {
      var result = s"Property $name:\n"
      result += s"-----------------------------\n"
      result += s"idx | pre   | now   | formula\n"
      result += s"-----------------------------\n"
      for (i <- 0 to now.size - 1) {
        result += f"$i   | ${pre(i)}%5s | ${now(i)}%5s | ${txt(i)}\n"
      }
      result
    }
  }
}
