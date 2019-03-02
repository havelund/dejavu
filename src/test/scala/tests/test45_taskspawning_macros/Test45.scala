package tests.test45_taskspawning_macros

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test45 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test45_taskspawning_macros"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1)
    checkResults(16,22,27,30)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2)
    checkResults()
  }
}

