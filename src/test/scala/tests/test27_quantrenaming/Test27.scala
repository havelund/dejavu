package tests.test27_quantrenaming

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test27 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test27_quantrenaming"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1_1(): Unit = {
    Verify(spec1,log1)
    checkResults()
  }

  @Test def test1_2(): Unit = {
    Verify(spec1,log2)
    checkResults(5,6,8,9)
  }

  @Test def test2_1(): Unit = {
    Verify(spec2,log1)
    checkResults()
  }

  @Test def test2_2(): Unit = {
    Verify(spec2,log2)
    checkResults(5,6)
  }
}
