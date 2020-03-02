package tests.test53_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test53 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test53_time"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.timed.csv"

  @Test def test1(): Unit = {
    Verify(spec, log1, "3", "debug")
    checkResults(4,7,9)
  }
}

