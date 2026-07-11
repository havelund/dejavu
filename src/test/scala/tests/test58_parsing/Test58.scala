package tests.test58_parsing

import java.io.File
import org.junit.Test
import org.junit.Assert.assertTrue
import tests.util.testcase.TestCase

/**
  * Parses every .qtl specification in the repository under a time bound.
  *
  * Guards against parser performance regressions: quantifier bodies are full
  * formulas (`ltl`), which in a plain backtracking combinator parser causes
  * exponential re-parsing (several specs took > 10 s before the parser was
  * made a memoizing PackratParser). With memoization every spec parses in
  * tens of milliseconds; the 2-second bound leaves ample slack.
  */
class Test58 extends TestCase {

  val BUDGET_MS = 2000L

  def specsUnder(dir: File): Seq[File] = {
    val entries = Option(dir.listFiles()).map(_.toSeq).getOrElse(Seq())
    entries.filter(_.isDirectory).flatMap(specsUnder) ++
      entries.filter(f => f.isFile && f.getName.endsWith(".qtl"))
  }

  @Test def parseAllSpecsWithinBudget(): Unit = {
    val roots = Seq(
      new File(dejavu.Settings.PROJECT_DIR + "/src/test/scala/tests"),
      new File(dejavu.Settings.PROJECT_DIR + "/out/examples"))
    val specs = roots.flatMap(specsUnder)
    assertTrue(s"expected many specs, found ${specs.size}", specs.size > 50)
    for (spec <- specs) {
      val parser = new dejavu.Parser
      val t0 = System.currentTimeMillis()
      val result = parser.parseAll(parser.document,
        new java.io.FileReader(spec))
      val ms = System.currentTimeMillis() - t0
      assertTrue(s"$spec failed to parse: $result", result.successful)
      assertTrue(s"$spec took $ms ms to parse (budget $BUDGET_MS ms)",
        ms <= BUDGET_MS)
    }
  }
}
