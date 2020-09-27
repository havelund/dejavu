package tests.test42_formalise_taskspawning

import dejavu.Verify
import org.junit.Test
import tests.util.testcase.TestCase

class Test42 extends TestCase {
  val TEST = PATH_TO_TESTS + "/test42_formalise_taskspawning"
  val spec = s"$TEST/spec.qtl"
  val log1 = s"$TEST/log1.csv"
  val log2 = s"$TEST/log2.csv"
  val biglog10k = s"$TEST/biglog10k.csv"
  val biglog20k = s"$TEST/biglog20k.csv"
  val biglog40k = s"$TEST/biglog40k.csv"

  @Test def test1(): Unit = {
    Verify(spec,log1)
    checkResults(16,22,27,30)
  }

  @Test def test2(): Unit = {
    Verify(spec,log2)
    checkResults()
  }

  // --- long tests: ---

  // threads: how many threads initially spawned by main program
  // repeat: how many times new threads from new threads are spawned
  // number of spawned threads correspond to number of events

  // STTT trace T4:
  // ==============
  // val (threads, repeat) = (50,100) // 10k
  // spawns = 4949
  // Processed 9899 events
  // Elapsed trace analysis time: 34.67s

  @Test def test3(): Unit = {
    Verify(spec,biglog10k)
    checkResults(9899)
  }

  // STTT trace T5:
  // ==============
  // val (threads, repeat) = (100,100) //20k
  // spawns = 9999
  // Processed 19999 events
  // Elapsed trace analysis time: 127.567s

  // 2m10s
  // @Test
  def test4(): Unit = {
    Verify(spec,biglog20k)
    checkResults(19999)
  }

  // STTT trace T6:
  // ==============
  // val (threads, repeat) = (100,200) // 40k
  // spawns = 19899
  // Processed 39799 events
  // Elapsed trace analysis time: 572.443s

  // 8m52s
  // @Test
  def test5(): Unit = {
    Verify(spec,biglog40k)
    checkResults(39799)
  }
}

