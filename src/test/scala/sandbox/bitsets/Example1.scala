package sandbox.bitsets

import scala.collection.immutable.BitSet

object BitsetTest {

  def main(args : Array[String]) : Unit = {

    val primebits = BitSet(   2, 3,    5,    7,        11)
    val evenBits =  BitSet(0, 2,    4,    6,    8, 10)
    // not                 1     3     5     7       9 11

    val evenSet = Set(     0, 2,    4,    6,    8, 10);

    println("-----")
    println(primebits(0))
    println(primebits(1))
    println(primebits(2))
    println(primebits(3))
    println(primebits(4))
    println(primebits(5))
    println("-----")

    println(primebits & evenBits)  // BitSet(2)
    println(primebits & evenSet)  // BitSet(2)

    println(primebits &~ evenBits)  // BitSet(3, 5, 7, 11)
    println(primebits &~ evenSet)   // BitSet(3, 5, 7, 11)

    for (b <- primebits) {
      println(b)
    }

    println("---")

    for (i <- 0 to 11) {
      println(s"$i : ${BitSet.fromBitMask(Array(i))}")
    }
  }

}