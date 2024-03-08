package sandbox.bdds.example1

import net.sf.javabdd.{BDD, BDDFactory}

object Example1 {

  def main(args: Array[String]) {

    println("A program to familize myself with BDD");
    runSimplifyBDD()
    println("Program ended");

  }

  def runSimplifyBDD() {
    var B : BDDFactory = BDDFactory.init(1000, 1000)
    val False = B.zero()
    val True = B.one()
    B.setVarNum(8)
    val v0 : BDD = B.ithVar(0)
    val v1 : BDD = B.nithVar(1)
    val v2 : BDD = B.ithVar(2)
    val v3 : BDD = B.ithVar(3)
    val v4 : BDD = B.ithVar(4)
    val v5 : BDD = B.ithVar(5)
    val v6 : BDD = B.ithVar(6)
    val v7 : BDD = B.ithVar(7)

    val a : BDD = v1.xor(v2).not()
    val b : BDD = v2.xor(v3).not()
    val c : BDD = (a.xor(b)).not()
    val d : BDD = v4.xor(v5).not().xor(v6)
    val e : BDD = v6.xor(v7).not()
    val f : BDD = d.xor(e).not()

    val g : BDD = c.xor(f)

    // ----------------------
    val x1 : BDD = B.buildCube(5,Array(2,1,0))
    val res1 : BDD = x1.exist(x1.support())
    val res : BDD = v2.not().and(v5).support()
    println(s"res = [$res]")
    if (res.isOne()) println("!!! TRUE !!!")
    if (res.isZero) println("*** FALSE ***")
    res.printDot()
    // ----------------------

    // g.printDot(); //first graph diagram
    /* At this point
     * let say we know the BDD variable v1 = One (true)
     * What is the code that should be inserted to simplify the BDD
     * so that second graph is like the attached image
     */

    //g.printDot(); //second graph diagram

  }

}



