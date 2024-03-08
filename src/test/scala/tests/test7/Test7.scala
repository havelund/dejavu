package tests.test7

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test7 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test7"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    // println(System.getProperty("user.dir"))
    Verify(spec,log1)
    checkResults(7,9)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2)
    checkResults()
  }
}
