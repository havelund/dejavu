package sandbox.bdds.example2

import net.sf.javabdd.{BDD, BDDFactory}

object Example2 {
  def print(s: String)(bdd: BDD): Unit = {
    println("=============")
    println(s)
    println("=============")
    bdd.printDot()
  }

  def main(args: Array[String]) {

    var B: BDDFactory = BDDFactory.init(1000, 1000)
    val False = B.zero()
    val True = B.one()
    B.setVarNum(8)

    val varbits1 = Array(3, 2, 1, 0)
    val varbits2 = Array(7, 6, 5, 4)
    val varbits3 = Array(0)

    val var1 = B.buildCube(0, varbits1).support()
    val var2 = B.buildCube(0, varbits2).support()

    val vector1 = B.buildCube(5, varbits1)
    val vector2 = B.buildCube(1, varbits2)
    val vector3 = vector1.and(vector2)
    val vector4 = vector3.exist(var1)
    val vector5 = B.buildCube(1, varbits3)

    print("var1")(var1)
    print("var2")(var2)
    print("vector1")(vector1)
    print("vector2")(vector2)
    print("vector3")(vector3)
    print("vector4")(vector4)

    print("vector5")(vector5)

    val vector6 = vector5.and(vector5)

    print("vector5 & vector5")(vector5)

  }
}
