# v2 addendum — pointer only. `WRITEUP.md` in this directory is NOT edited.

**Why this file exists.** A v2 run (`audits/2026-08-23_oq120_epsilon_boundary_v2/`) re-scored this
audit's data under a repaired gate. `WRITEUP.md` in this directory **landed** and is part of the
record; silently reconciling a landed writeup to a later re-scoring is the same defect class as
editing a frozen prereg, so it stays as written and the reconciliation lives here.

**What v2 did, and what it did not do.** v2 swept the identical substrate (18 live legs +
`kernel_v1`, same file counts, HEAD unmoved at `f88c8c3c`) and **reproduced this run's transition
data exactly** — same rows, same ε brackets, same MT/FT pairs, same gate sets, 0 legs differing.
**v2 is therefore not a second measurement; it is the same dataset re-scored** under the
MOVED-vs-DECISIVE definition the operator ruled at this run's checkpoint.

**Consequences for reading this directory:**

- **The ordering witness attaches to the DATA, and it lives here.** This directory's
  `PREREGISTRATION.md` (md5 `b181e1a2a9cd42b86d190be09f61d400`) genuinely precedes the data, and
  the data is bit-identical between runs. v2's prereg does not have that property and says so.
- **v1 is the weaker instrument for SCORING; v2 is the weaker instrument for FREEZING.** Both halves
  are true and neither dominates. v2's numbers are the headline; they inherit their ordering
  witness from this directory.
- **The branch did not move.** G1b under this run's gate, G1b under v2's — across a re-specification
  that cut pooled `N_rail` ~5× (9191 → 1852) and took the floor from cleared-900× to failed-by-11-
  of-23-strata. **Same rows, re-scored, branch invariant.**
- Numbers in this directory's `WRITEUP.md` are MOVED-scored where v2's are DECISIVE-scored. Where
  they differ, v2's are the ones to cite; the difference is a scoring definition, not a measurement.

**Read v2's `WRITEUP.md` for the current headline.** Everything here remains valid as the record of
what was known and frozen before the scoring question was settled.
