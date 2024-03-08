package tests.test28_wellformedness

import java.lang.Throwable

import dejavu.Verify
import org.junit.{Test}
import tests.util.testcase.TestCase

class Test28 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test28_wellformedness"
  val spec = s"$TEST/spec.qtl"

  @Test def test1(): Unit = {
     VerifyNotWF(spec)
  }
}
