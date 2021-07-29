package tests.test57_synthesis_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test57 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test57_synthesis_time"
  val spec = s"$TEST/spec.qtl"
  val log = s"$TEST/spec.csv"

  @Test def test1_1(): Unit = {
    Verify(spec,log,"3")
    checkResults()
  }
}
