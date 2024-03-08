package tests.test1_fmcad_access

import org.junit.Test
import dejavu.Verify
import tests.util.testcase.TestCase

class Test1 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test1_fmcad_access"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1)
    checkResults(11006)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2)
    checkResults(110006)
  }

  @Test def test3(): Unit = {
    Verify.long(spec,log3)
    checkResults(1100006)
  }
}
