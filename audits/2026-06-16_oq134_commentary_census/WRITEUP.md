# OQ-134 — Generic commentary-grade corpus census (build + witnesses)

**Date:** 2026-06-16  **Branch:** `worktree-oq134-commentary-census`
**Plan:** `~/.claude/plans/radiant-meandering-donut.md`  **Tracking:** OQ-134 (bundled_with OQ-86, OQ-83)

Automates the by-hand q6 census (in `audits/2026-06-16_q6_crosscheck_completion/WRITEUP.md`) as a
repeatable, kept-fresh pipeline artifact. Per operator ruling, built as a GENERIC commentary census
(not a q6-special one) and wired into `run_pipeline.py`. Commentary-grade: reads engine predicates
only, never feeds classification.

## What shipped

- **`prolog/commentary_census.pl`** — module + multifile `commentary_cell(+Source,+C,-Bucket)` hook,
  `commentary_absence_bucket/2`, `commentary_coverage_decidable/1`, `commentary_census/2`,
  `run_commentary_census/0`. Denominator is `corpus_loader:corpus_constraint/1` (CLAUDE.md mandate).
  Sources: `q6` (= `stakeholder_seats:q6_crosscheck/3`), `extraction_reading` (= OQ-86, fired/silent).
- **`python/run_pipeline.py`** — `_prolog_commentary_census` (Phase-2 task `commentary_census`),
  `_PREAMBLE_MARKERS['commentary_census']`. Parses `CENSUS*` lines → `outputs/commentary_census.{json,md}`
  with a corpus-identity manifest. Fail-loud invariants: Σ buckets == n_corpus AND n_corpus > 0 per source.
- **`prolog/tests/test_commentary_census.pl`** — 20 plunit tests.

## Verification (all witnessed; raw artifacts in this directory)

### 1. Hand-census cross-check — multiset equality, modulo +1 corpus growth
`census_live.txt`. Automated q6 histogram vs the by-hand census (n=71, 2026-06-16). EVERY named cell
is byte-identical; the only difference: `q6_unmeasured` 19→20 and `n_corpus` 71→72. This is exactly the
+1 corpus growth since the hand census — the new story authors no `founding_problem_status`
(corpus_constraint=72, with_founding_status=52, without=20), so it lands in `q6_unmeasured`. Resolved
against the substrate, not narrated. `q6_unclassified` correctly absent (=0) on live.

| bucket | automated (n=72) | hand (n=71) |
|--------|------|------|
| contested_open | 18 | 18 |
| q6_unmeasured | 20 | 19 (+1 corpus growth) |
| q6_signature_unknown | 8 | 8 |
| live_claim_vs_snare_present | 8 | 8 |
| live_claim_vs_tangled_present | 6 | 6 |
| dead_claim_vs_snare_present | 6 | 6 |
| live_claim_vs_rope_present | 2 | 2 |
| live_claim_vs_piton_present | 2 | 2 |
| dead_claim_vs_rope_present | 1 | 1 |
| dead_claim_vs_piton_present | 1 | 1 |
| q6_unclassified | 0 (absent) | 0 |

### 2. plunit — 20/20 pass (`plunit_run.txt`)
`run_tests(commentary_census)`. Sum invariant Σ buckets == n_corpus AND n_corpus>0 (both sources);
`q6_unclassified` ≠ `q6_signature_unknown` (distinct keys); side-absent precedence (contested×unknown
→ `q6_signature_unknown`, not `contested_open`/`q6_unclassified`); per-cell positive controls (fixtures
land in dead/live/contested cells + both absence buckets — proves the probe FIRES, not just returns 0);
`commentary_cell` deterministic; absence-bucket + coverage-decidability declarations; extraction_reading
bivalues. Fixtures are not `corpus_constraint/1` facts, so they do not perturb the denominator.

### 3. Cross-corpus self-labeling (`census_cross_corpus.txt`) — fresh swipl process per corpus
- **Twins (q6_unclassified reachability):** `testsets_haiku` n=960, `q6_unclassified`=1; `testsets_flash`
  n=960, `q6_unclassified`=5 (both match the q6-completion audit). The overlay took effect (n=960 ≠
  default 72 — reset witnessed). The live `0` is corpus-specific, not universal — and the manifest's
  corpus identity makes it self-labeling, never a hardcoded 0.
- **Pre-stakeholder archives = fail-closed positive control:** `kernel_v1`(1106), `original_v5`(702),
  `original_v6`(3380), `testsets_sotu`(189) route 100% to `q6_unmeasured`, **ZERO named cells**. These
  corpora predate stakeholder/founding-problem authoring; a named cell here would be the census
  fabricating a verdict from absence (hard fail). None appeared — the absence buckets are load-bearing,
  not decorative (Build Discipline Pattern 6 honored).

### 4. Pipeline integration (`commentary_census.json`)
`python3 python/run_pipeline.py` → `commentary_census` task `ok`; `outputs/commentary_census.{json,md}`
written. q6 coverage = 0.611 = 44/72 (28 absent = unmeasured 20 + signature_unknown 8);
`extraction_reading` coverage = `null` (N/A — `extraction_silent` present-vs-absent UNRULED, never a
defaulted 1.0). Manifest carries n_constraints=72, corpus_hash, commit. **Classification byte-identical
by construction:** the census is its own swipl process (`["stack.pl","commentary_census.pl"]`), reads
only, asserts nothing — not on the `dr_type`/json_report path (structural witness, same grade as the q6
completion audit).

### 5. Gate — GREEN
`./scripts/gate.sh`: issues_status 134 parsed / 0 malformed; omega check 0 problems; omega selftest 8/8;
known_state 0 problems.

## Coverage semantics (pinned)
Coverage = "both sides were MEASURED," NOT "landed in a named verdict cell" — so `q6_unclassified`
counts as covered (founding_problem_status present + non-unknown dr_type, just not one of the named
status×signature pairs). A source ships a coverage RATIO only when `commentary_coverage_decidable/1`
declares its absence-set RULED complete; empty absence-set ≠ ruled-none, so `extraction_reading` ships
`null` until its silent-vs-absent question is ruled.

## Extension point
A new commentary source is a one-clause `commentary_cell/3` add (+ `commentary_source/1`, optionally
`commentary_absence_bucket/2`, `commentary_coverage_decidable/1`). Future-cheap, no open OQ yet:
`consensus_provenance/2`, `seat_perceived_vs_real/4`, `mandatrophy_gap`.
