package tests.test34_states

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test34 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test34_states"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1,"3")
    checkResults(5,8,10)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2,"3")
    checkResults()
  }
}

