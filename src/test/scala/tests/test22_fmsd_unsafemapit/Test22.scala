package tests.test22_fmsd_unsafemapit

import org.junit.Test
import dejavu.Verify
import tests.util.testcase.TestCase

class Test22 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test22_fmsd_unsafemapit"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val log4 = s"$TEST/log4.csv"
  val log5 = s"$TEST/log5.csv" // for long traces

  @Test def test1_1(): Unit = {
    Verify(spec1,log1)
    checkResults()
  }

  @Test def test1_2(): Unit = {
    Verify(spec1,log2)
    checkResults(11)
  }

  @Test def test1_3(): Unit = {
    Verify(spec1,log3)
    checkResults(14)
  }

  @Test def test1_4(): Unit = {
    Verify(spec1,log4)
    checkResults()
  }

  // This test was used for trying out large traces
  @Test def test2_5(): Unit = {
    Verify.long(spec2,log5)
    checkResults(10801)
  }
}
