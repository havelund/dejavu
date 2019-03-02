package tests.test18_gc

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test18 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test18_gc"
  val spec1 = s"$TEST/spec1.qtl"
  val spec2 = s"$TEST/spec2.qtl"
  val spec3 = s"$TEST/spec3.qtl"
  val spec4 = s"$TEST/spec4.qtl"
  val spec5 = s"$TEST/spec5.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val log4 = s"$TEST/log4.csv"
  val log5 = s"$TEST/log5.csv"

  // =========================
  // Log 1:
  // =========================

  // --- spec 1: ---

  @Test def test1_1(): Unit = {
    Verify(spec1,log1, "3")
    checkResults(12 oom)
  }

  // --- spec 2: ---

  @Test def test2_1(): Unit = {
    Verify(spec2,log1, "3")
    checkResults(12 -- 1,12 -- 2,14 -- 3)
  }

  // --- spec 3: ---

  @Test def test3_1(): Unit = {
    Verify(spec3,log1, "3")
    checkResults(12 oom)
  }

  // -- spec 4: ---

  @Test def test4_1(): Unit = {
    Verify(spec4,log1, "3")
    checkResults(12 -- 1,12 -- 2,14 -- 3)
  }

  // -- spec 5: --

  @Test def test5_1(): Unit = {
    Verify(spec5,log1, "3")
    checkResults(12 oom)  // it should record all errors, for all properties, needs fix.
  }

  // ============================================
  // Log 2: OPEN = 50000 NR = 1000  REPEAT = 1000
  // ============================================

  // --- spec 1: ---

  @Test def test1_2_21(): Unit = {
    Verify.long(spec1,log2, "21")
    checkResultsBrief(2052002)
  }

  @Test def test1_2_20(): Unit = {
    Verify.long(spec1,log2, "20")
    checkResultsBrief(2047575 oom)
  }

  // --- spec 2: ---

  @Test def test2_2_20(): Unit = {
    Verify.long(spec2,log2, "20")
    checkResultsBrief(2052002, 998998 gc)
  }

  @Test def test2_2_15(): Unit = {
    Verify.long(spec2,log2, "16")
    checkResultsBrief(2052002,992992 gc)
  }

  // --- spec 3: ---

  @Test def test3_2_21(): Unit = {
    Verify.long(spec3,log2, "21")
    checkResultsBrief(2052003)
  }

  @Test def test3_2_20(): Unit = {
    Verify.long(spec3,log2, "20")
    checkResultsBrief(2047575 oom)
  }

  // --- spec 4: ---

  @Test def test4_2_20(): Unit = {
    Verify.long(spec4,log2, "20")
    checkResultsBrief(2052003,998998 gc)
  }

  @Test def test4_2_16(): Unit = {
    Verify.long(spec4,log2, "16")
    checkResultsBrief(2052003,992992 gc)
  }

  // =========================================
  // Log 3: OPEN = 1000 NR = 500 REPEAT = 3000
  // =========================================

  // --- spec 1: ---

  @Test def test1_3_21(): Unit = {
    Verify.long(spec1,log3, "21")
    checkResultsBrief(3007002)
  }

  @Test def test1_3_20(): Unit = {
    Verify.long(spec1,log3, "20")
    checkResultsBrief(2096168 oom)
  }

  // --- spec 2: ---

  @Test def test2_3_21(): Unit = {
    Verify.long(spec2,log3, "21")
    checkResultsBrief(3007002)
  }

  @Test def test2_3_20(): Unit = {
    Verify.long(spec2,log3, "20")
    checkResultsBrief(3007002,1047591 gc)
  }

  @Test def test2_3_10(): Unit = {
    Verify.long(spec2,log3, "10")
    checkResultsBrief(3007002,1503000 gc)
  }

  // --- spec 3: ---

  @Test def test3_3_21(): Unit = {
    Verify.long(spec3,log3, "21")
    checkResultsBrief(3007003)
  }

  @Test def test3_3_20(): Unit = {
    Verify.long(spec3,log3, "20")
    checkResultsBrief(2096168 oom)
  }

  // --- spec 4: ---

  @Test def test4_3_20(): Unit = {
    Verify.long(spec4,log3, "20")
    checkResultsBrief(3007003,1047591 gc)
  }

  @Test def test4_3_10(): Unit = {
    Verify.long(spec4,log3, "10")
    checkResultsBrief(3007003,1503000 gc)
  }

  // ======================================
  // Log 4: OPEN = 6 NR = 5 REPEAT = 200000
  // ======================================

  // --- spec 1: ---

  @Test def test1_4_21(): Unit = {
    Verify.long(spec1,log4, "21")
    checkResultsBrief(2400008)
  }

  @Test def test1_4_20(): Unit = {
    Verify.long(spec1,log4, "20")
    checkResultsBrief(2097149 oom)
  }

  // --- spec 2: ---

  @Test def test2_4_21(): Unit = {
    Verify.long(spec2,log4, "21")
    checkResultsBrief(2400008)
  }

  @Test def test2_4_20(): Unit = {
    Verify.long(spec2,log4, "20")
    checkResultsBrief(2400008,1048572 gc)
  }

  @Test def test2_4_10(): Unit = {
    Verify.long(spec2,log4, "10")
    checkResultsBrief(2400008,1199520 gc)
  }

  @Test def test2_4_3(): Unit = {
    Verify.long(spec2,log4, "3")
    checkResultsBrief(2400008,1200000 gc)
  }

  // --- spec 3: ---

  @Test def test3_4_21(): Unit = {
    Verify.long(spec3,log4, "21")
    checkResultsBrief(2400009)
  }

  @Test def test3_4_20(): Unit = {
    Verify.long(spec3,log4, "20")
    checkResultsBrief(2097149 oom)
  }

  // --- spec 4: ---

  @Test def test4_4_20(): Unit = {
    Verify.long(spec4,log4, "20")
    checkResultsBrief(2400009,1048572 gc)
  }

  @Test def test4_4_10(): Unit = {
    Verify.long(spec4,log4, "10")
    checkResultsBrief(2400009,1199520 gc)
  }

  @Test def test4_4_3(): Unit = {
    Verify.long(spec4,log4, "3")
    checkResultsBrief(2400009,1200000 gc)
  }

  // =================================================
  // Log 5: OPEN = 0 NR = 2 REPEAT = 1000000 (kind of)
  // =================================================

  // --- spec 1: ---

  @Test def test1_5_20(): Unit = {
    Verify.long(spec1,log5, "20")
    checkResultsBrief(2000002)
  }

  @Test def test1_5_2(): Unit = {
    Verify(spec1,log5, "2")
    checkResultsBrief(8 oom)
  }

  // --- spec 2: ---

  @Test def test2_5_20(): Unit = {
    Verify.long(spec2,log5, "20")
    checkResultsBrief(2000002)
  }

  @Test def test2_5_2(): Unit = {
    Verify.long(spec2,log5, "2")
    checkResultsBrief(2000002,999998 gc)
  }

  // --- spec 3: ---

  @Test def test3_5_20(): Unit = {
    Verify.long(spec3,log5, "20")
    checkResultsBrief(2000004)
  }

  @Test def test3_5_2(): Unit = {
    Verify(spec3,log5, "2")
    checkResultsBrief(8 oom)
  }

  // --- spec 4: ---

  @Test def test4_5_20(): Unit = {
    Verify.long(spec4,log5, "20")
    checkResultsBrief(2000004)
  }

  @Test def test4_5_2(): Unit = {
    Verify.long(spec4,log5, "2")
    checkResultsBrief(2000004,999998 gc)
  }
}


