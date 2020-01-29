package tests.test49_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test49 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test49_time"
  val spec1 = s"$TEST/spec1.qtl" // untimed
  val spec2 = s"$TEST/spec2.qtl" // timed
  val log1 = s"$TEST/log1.timed.csv"

//   @Test def test1(): Unit = {
//     Verify(spec1,log1, "3", "debug")
//     checkResults()
//  }

  @Test def test2(): Unit = {
    Verify(spec2,log1, "3", "debug")
    checkResults()
  }
}

