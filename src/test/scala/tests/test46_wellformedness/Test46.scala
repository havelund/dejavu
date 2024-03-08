package tests.test46_wellformedness

import org.junit.Test
import tests.util.testcase.TestCase

class Test46 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test46_wellformedness"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val spec3 = s"$TEST/spec3.qtl"
  val spec4 = s"$TEST/spec4.qtl"
  val spec5 = s"$TEST/spec5.qtl"
  val spec6 = s"$TEST/spec6.qtl"
  val spec7 = s"$TEST/spec7.qtl"

  @Test def test1(): Unit = {
     VerifyNotWF(spec1)
  }

  @Test def test2(): Unit = {
    VerifyNotWF(spec2)
  }

  @Test def test3(): Unit = {
    VerifyNotWF(spec3)
  }

  @Test def test4(): Unit = {
    VerifyNotWF(spec4)
  }

  @Test def test5(): Unit = {
    VerifyNotWF(spec5)
  }

  @Test def test6(): Unit = {
    VerifyNotWF(spec6)
  }

  @Test def test7(): Unit = {
    VerifyNotWF(spec7)
  }
}
