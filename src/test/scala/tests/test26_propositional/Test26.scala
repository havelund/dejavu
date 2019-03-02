package tests.test26_propositional

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test26 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test26_propositional"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val log4 = s"$TEST/log4.csv"

  @Test def test1_1(): Unit = {
    Verify(spec1,log1)
    checkResults()
  }

  @Test def test1_2(): Unit = {
    Verify(spec1,log2)
    checkResults(6)
  }

  @Test def test2_3(): Unit = {
    Verify(spec2,log3)
    checkResults()
  }

  @Test def test2_4(): Unit = {
    Verify(spec2,log4)
    checkResults(5)
  }
}
