package tests.test51_sttt_rules

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test51 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test51_sttt_rules"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"

  @Test def test1(): Unit = {
    Verify(spec, log1, "3", "debug")
    checkResults(5)
  }

  @Test def test2(): Unit = {
    Verify(spec, log2, "3")
    checkResults()
  }

  @Test def test3(): Unit = {
    Verify(spec, log3, "3", "debug")
    checkResults()
  }
}

