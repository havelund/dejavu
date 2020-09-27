package tests.test39_formalise_wolper

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test39 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test39_formalise_wolper"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val log3 = s"$TEST/log3.csv"
  val biglog100k = s"$TEST/biglog100k.csv"
  val biglog1000k = s"$TEST/biglog1000k.csv"
  val biglog5000k = s"$TEST/biglog5000k.csv"
  val biglog10000k = s"$TEST/biglog10000k.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1,"3")
    checkResults(1,4,9,14,21,24,26)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2,"3")
    checkResults()
  }

  @Test def test3(): Unit = {
    Verify(spec,log3,"3", "debug")
    checkResults()
  }

  // --- long traces: ---

  // repeat: repeat the following
  // repeat_toggle: open this many channels
  // repeat_telem: send this many messages on each channel

  // val (repeat, repeat_toggle, repeat_telem) = (10,100,100) // 100k
  // Processed 102001 events
  // Elapsed trace analysis time: 1.528s
  @Test def test4(): Unit = {
    Verify(spec,biglog100k)
    checkResults(102001)
  }

  // STTT trace T1:
  // ==============
  // val (repeat, repeat_toggle, repeat_telem) = (100,1000,10) // 1_000k
  // Processed 1200001 events
  // Elapsed trace analysis time: 3.274s
  @Test def test5(): Unit = {
    Verify(spec,biglog1000k)
    checkResults(1200001)
  }

  // STTT trace T2:
  // ==============
  // val (repeat, repeat_toggle, repeat_telem) = (1000,100,50) // 5_000k
  // Processed 5200001 events
  // Elapsed trace analysis time: 7.102s
  @Test def test6(): Unit = {
    Verify(spec,biglog5000k)
    checkResults(5200001)
  }

  // STTT trace T3:
  // ==============
  // val (repeat, repeat_toggle, repeat_telem) = (1000,100,100) // 10_000k
  // Processed 10200001 events
  // Elapsed trace analysis time: 12.777s
  @Test def test7(): Unit = {
    Verify(spec,biglog10000k)
    checkResults(10200001)
  }
}

