package tests.test3_fmcad_fifo

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test3 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test3_fmcad_fifo"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1)
    checkResults(6,14,16,17,21)
  }

  @Test def test2(): Unit = {
    Verify.long(spec,log2,"13")
    checkResults(5051)
  }
}

