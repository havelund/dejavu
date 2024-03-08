package tests_fmsd

import dejavu.{Settings, Verify}
import org.junit.Test
import tests.util.testcase.TestCase

class TestFMSD extends TestCase {
  val TESTS = Settings.PROJECT_DIR + "/src/test/scala/tests_fmsd"

  val TEST1 = s"$TESTS/property1-access"
  val spec1 = s"$TEST1/prop1.dejavu"
  val log1_10 = s"$TEST1/10,000/log-dejavu.txt"
  val log1_100 = s"$TEST1/100,000/log-dejavu.txt"
  val log1_1000 = s"$TEST1/1,000,000/log-dejavu.txt"

  val TEST2 = s"$TESTS/property2-file"
  val spec2 = s"$TEST2/prop2.dejavu"
  val log2_10 = s"$TEST2/10,000/log-dejavu.txt"
  val log2_100 = s"$TEST2/100,000/log-dejavu.txt"
  val log2_1000 = s"$TEST2/1,000,000/log-dejavu.txt"

  val TEST3 = s"$TESTS/property3-fifo"
  val spec3 = s"$TEST3/prop3.dejavu"
  val log3_5 = s"$TEST3/5,000/log-dejavu.txt"
  val log3_10 = s"$TEST3/10,000/log-dejavu.txt"

  val TEST4 = s"$TESTS/property4-locks-basic"
  val spec4 = s"$TEST4/prop4.dejavu"
  val log4_10 = s"$TEST4/10,000/log-dejavu.txt"
  val log4_100 = s"$TEST4/100,000/log-dejavu.txt"
  val log4_1000 = s"$TEST4/1,000,000/log-dejavu.txt"

  val TEST5 = s"$TESTS/property5-locks-cycles"
  val spec5 = s"$TEST5/prop5.dejavu"
  val log5_10 = s"$TEST5/10,000/log-dejavu.txt"
  val log5_100 = s"$TEST5/100,000/log-dejavu.txt"
  val log5_1000 = s"$TEST5/1,000,000/log-dejavu.txt"

  val TEST6 = s"$TESTS/property6-locks-datarace"
  val spec6 = s"$TEST6/prop6.dejavu"
  val log6_10 = s"$TEST6/10,000/log-dejavu.txt"
  val log6_100 = s"$TEST6/100,000/log-dejavu.txt"
  val log6_1000 = s"$TEST6/1,000,000/log-dejavu.txt"

  // ==============
  // Test 1: Access
  // ==============

  @Test def test1_10(): Unit = {
    Verify(spec1, log1_10, "20")
    checkResults(11006)
  }

  @Test def test1_100(): Unit = {
    Verify(spec1, log1_100, "20")
    checkResults(110006)
  }

  @Test def test1_1000(): Unit = {
    Verify(spec1, log1_1000, "20")
    checkResults(1100006)
  }

  // ============
  // Test 2: File
  // ============

  @Test def test2_10(): Unit = {
    Verify(spec2, log2_10, "20")
    checkResults(11004)
  }

  @Test def test2_100(): Unit = {
    Verify(spec2, log2_100, "20")
    checkResults(110004)
  }

  @Test def test2_1000(): Unit = {
    Verify(spec2, log2_1000, "20")
    checkResults(1100004)
  }

  // ============
  // Test 3: Fifo
  // ============

  // 3m16s
  @Test
  def test3_5(): Unit = {
    Verify(spec3, log3_5, "13")
    checkResults(5051)
  }

  // 14m - OOM
  @Test
  def test3_10(): Unit = {
    Verify(spec3, log3_10, "14")
    checkResults(10101)
  }

  // ===================
  // Test 4: Locks Basic
  // ===================

  // 59s
  // @Test
  def test4_10(): Unit = {
    Verify(spec4, log4_10, "20")
    checkResults(10401)
  }

  @Test def test4_100(): Unit = {
    Verify(spec4, log4_100, "20")
    checkResults(105001)
  }

  // 15m
  // @Test
  def test4_1000(): Unit = {
    Verify(spec4, log4_1000, "20")
    checkResults(1050126)
  }

  // ====================
  // Test 5: Locks Cycles
  // ====================

  // 3m
  // @Test
  def test5_10(): Unit = {
    Verify(spec5, log5_10, "20")
    checkResults(9606)
  }

  @Test def test5_100(): Unit = {
    Verify(spec5, log5_100, "20")
    checkResults(100006)
  }

  // 57m
  // @Test
  def test5_1000(): Unit = {
    Verify(spec5, log5_1000, "20")
    checkResults(1050006)
  }

  // ======================
  // Test 6: Locks Datarace
  // ======================

  @Test def test6_10(): Unit = {
    Verify(spec6, log6_10, "20")
    checkResults(10005)
  }

  @Test def test6_100(): Unit = {
    Verify(spec6, log6_100, "20")
    checkResults(100005)
  }

  @Test def test6_1000(): Unit = {
    Verify(spec6, log6_1000, "20")
    checkResults(1050005)
  }
}

