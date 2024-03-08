package tests.test10

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

/**
  * Testing garbage collection in version 1.1.
  */

class Test10 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test10"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val log4 = s"$TEST/log4.csv"
  val log5 = s"$TEST/log5.csv"
  val log6 = s"$TEST/log6.csv"

  @Test def test1(): Unit = {
    Verify(spec, log1, "3")
    checkResults()
  }

  @Test def test2(): Unit = {
    Verify(spec, log2, "3")
    checkResults()
  }

  @Test def test3(): Unit = {
    Verify(spec, log3, "3")
    checkResults(5)
  }

  @Test def test4(): Unit = {
    Verify.long(spec, log4)
    checkResults(1003001)
  }

  @Test def test5(): Unit = {
    Verify(spec, log5, "3", "debug")
    checkResults(
      10 -- 2,
      10 -- 3,
      10 -- 1,
      13
    )
  }

  @Test def test6(): Unit = {
    Verify(spec, log6, "2")
    checkResults(
      6 -- 1,
      6 -- 2,
      6 -- 3,
      9,
      11 -- 4,
      11 -- 5,
      11 -- 2,
      16 -- 6,
      17
    )
  }
}
