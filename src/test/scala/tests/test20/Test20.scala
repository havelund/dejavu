package tests.test20

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test20 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test20"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val spec3 = s"$TEST/spec3.qtl"
  val spec4 = s"$TEST/spec4.qtl"
  val spec5 = s"$TEST/spec5.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val log4 = s"$TEST/log4.csv"

  @Test def test1_1(): Unit = {
    Verify(spec1, log1, "3")
    checkResults(7)
  }

  @Test def test2_1(): Unit = {
    Verify(spec2, log1, "3")
    checkResults(7)
  }

  @Test def test3_2(): Unit = {
    Verify(spec3, log2, "3")
    checkResults(10)
  }

  @Test def test4_2(): Unit = {
    Verify(spec4, log2, "3")
    checkResults(10)
  }

  @Test def test5_3(): Unit = {
    Verify(spec5, log3, "3")
    checkResults(7)
  }

  @Test def test5_4(): Unit = {
    Verify(spec5, log4, "3")
    checkResults(3)
  }
}



