# Release notes

## Unreleased (branch fix-quantifier-scope)

### Quantifier scope restored (behavioral change)

The capital quantifiers `Exists x .` and `Forall x .` once again scope over
the **rest of the formula**, as documented. Since July 2021 ("sped up parser",
commit 55fdb2c) they bound only the next minimal subformula, a temporary
workaround for slow parsing that was never reverted (the `TODO : change back
to ltl` in Parser.scala). Consequences of the workaround, now repaired:

- Specifications such as `Forall u . login(u) -> logout(u)` were rejected
  with "variable occurs free", including examples shipped in this repository
  (e.g. `out/examples/auction/prop1.qtl`) and roughly 100 spec/log pairs of
  the test suite.
- A few accepted specifications became silently vacuous (the implication fell
  outside the quantifier), so the monitor missed real violations
  (`test3_fmcad_fifo`, `test20/spec4`).

**Migration:** if a specification deliberately relied on the tight binding,
add explicit parentheses: `(Forall x . p(x)) -> q` instead of
`Forall x . p(x) -> q`.

### Parser is now a memoizing PackratParser

Restoring the wide scope reintroduces heavy backtracking in a plain
combinator parser (five specs in this repository took over 10 seconds to
parse). The parser is therefore now a `PackratParsers` (memoizing) parser:
all 115 specifications in the repository parse in at most ~55 ms. A new test
(`test58_parsing`) parses every spec under a time budget to guard against
regressions.

### Other fixes

- `Settings.PROJECT_DIR` was hardcoded to a machine-specific path, making the
  test suite unrunnable elsewhere; it now uses the working directory.
- `checkResults` in the test harness compared garbage-collection notices
  (`n -- value`) order-sensitively; their order depends on hash-map iteration
  and varies across JVM/Scala versions. Violation events are still compared
  in order; GC notices are now compared as a set.
- The prebuilt `out/artifacts/dejavu_jar/dejavu.jar` is rebuilt from the fixed
  sources and now bundles the **Scala 2.12** runtime (previously 2.11, which
  no longer builds on modern JDKs). Use Scala 2.12 for compiling the
  generated `TraceMonitor.scala`.

### Test status

The full JUnit suite passes (58 test classes, 251 test methods), run with
Scala 2.12 on JDK 11. Before this branch, 49 methods were failing on master:
44 due to the quantifier workaround (their recorded expectations encode the
intended wide-scope verdicts) and 5 due to the GC-notice ordering issue.

### Independent verification

The fix was additionally verified by differential testing against DejaVuMT,
an independent SMT-based implementation of the same logic: on all 333
(specification, log) pairs shipped in this repository, every pair that both
tools can process now produces identical verdicts.
