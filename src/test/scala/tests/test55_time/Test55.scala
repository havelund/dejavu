package tests.test55_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test55 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test55_time"
  val spec1 = s"$TEST/spec1.qtl"
  val log1 = s"$TEST/log1.timed.csv"

  @Test def test1_1(): Unit = {
    Verify(spec1,log1,"3")
    checkResults(4,12)
  }
}
