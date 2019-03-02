package tests.test16

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test16 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test16"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    Verify(spec1,log1, "3")
    checkResults(1,2,3,4,5,6,7)
  }

  @Test def test2(): Unit = {
    Verify(spec1,log2, "3")
    checkResults(1,2,3,4,5,6,7,8)
  }

  @Test def test3(): Unit = {
    Verify(spec2,log1, "3")
    checkResults(2,3,4,5,6)
  }

  @Test def test4(): Unit = {
    Verify(spec2,log2, "3")
    checkResults(1,3,5,7)
  }
}

