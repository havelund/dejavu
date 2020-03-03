package tests.test54_time

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test54 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test54_time"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val spec3 = s"$TEST/spec3.qtl"
  val spec4 = s"$TEST/spec4.qtl"
  val log1 = s"$TEST/log1.timed.csv"

  @Test def test1(): Unit = {
    Verify(spec1, log1, "3", "debug")
    checkResults(4, 7, 9)
  }

  @Test def test2(): Unit = {
    Verify(spec2, log1, "3", "debug")
    checkResults(5, 8)
  }

  @Test def test3(): Unit = {
    Verify(spec3, log1, "3", "debug")
    checkResults(5, 8)
  }

  @Test def test4(): Unit = {
    Verify(spec4, log1, "3", "debug")
    checkResults(4, 7, 9)
  }
}

