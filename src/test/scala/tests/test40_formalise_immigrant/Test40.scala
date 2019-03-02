package tests.test40_formalise_immigrant

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test40 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test40_formalise_immigrant"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1,"4")
    checkResults(11)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2,"4")
    checkResults(12)
  }
}

