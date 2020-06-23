package tests.test56_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test56 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test56_time"
  val spec1 = s"$TEST/spec1.qtl"
  val log1 = s"$TEST/log1.timed.csv"

  @Test def test1_1(): Unit = {
    Verify(spec1,log1,"3")
    checkResults(1674,1680,15808,32618,32717,38605,42991,48749)
  }
}
