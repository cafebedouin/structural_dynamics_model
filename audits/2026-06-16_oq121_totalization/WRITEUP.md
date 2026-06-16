# OQ-121 — Totalize the commentary family + domain-relative census coverage

**Date:** 2026-06-16  **Branch:** `worktree-oq121-totalize-census`  **Tracking:** OQ-121 (bundled_with OQ-134, OQ-86)

A closer look (operator-requested) at OQ-121 found a structural issue bigger than the missing
coverage ruling the OQ originally named.

## The bigger issue

**1. Totalization debt in the commentary family.** The engine already encodes "no verdict ⇒ an
explicit token, never a silent failure" where it matters most — correction-grade
`constraint_signature/2` (`signature_detection.pl:136`, explicit `unknown` fallback "instead of a
default-fabricated verdict") and `q6_crosscheck/3` (explicit absence buckets). The rest of the R3
family never got it: `extraction_reading/2` **failed silently**, `consensus_provenance/2` is silent
on `Ns=[]`. A silently-failing predicate destroys the provenance bit at the source, so no aggregate
can reconstruct it — Build Discipline Pattern 6 in its purest form (the absence is total). The fix is
mostly mechanical: guard A (`extractive_type(dr_type)`) already computes the domain; the predicate
discarded it on failure.

**2. Census coverage was corpus-relative, but domains aren't.** `coverage = (n_corpus − Σabsence)/n_corpus`
is only correct when domain == corpus (true for q6; false for extraction, whose domain is the 50/72
extractive-typed constraints). With no absence buckets that formula gives `72/72 = 1.0` — falsely
claiming coverage of the 22 out-of-domain constraints. Coverage ≠ prevalence ≠ corpus-fraction; one
silent bucket collapsed all three.

## What was built

- `stakeholder_seats:extraction_state/2` — TOTAL (mirrors `q6_cell/2`): every constraint reaches
  exactly one of `out_of_domain` / `extraction_clear` / `extraction_unnameable` / `extraction_fired(Es)`.
  `extraction_reading/2` now rides on `extraction_fired`, so its fire-or-silent report contract is
  UNCHANGED.
- `extraction_unnameable` (extractive ∧ no victim ∧ no nameable extractor) is its own bucket — the
  starkest blindspot, previously swallowed whole. **5 live constraints surfaced** that the silent
  failure had hidden entirely. Counts as MEASURED/covered (declared operator seat, revisable).
- `commentary_census.pl`: three bucket KINDS (out-of-domain / absence / measured); `coverage` is now
  domain-relative `(n_in_domain − Σabsence)/n_in_domain`; `prevalence = fired/n_in_domain` is a
  distinct number. `run_pipeline.py` parses the new `CENSUS_OOD` / `CENSUS_PREVALENCE` / `n_in_domain`
  lines and cross-checks `n_in_domain == n_corpus − Σood`.

## Witnesses (raw artifacts in this directory)

- **`census_live.txt`** — q6 unchanged (n_in_domain 72, coverage 0.611); extraction
  `{extraction_clear 42, extraction_out_of_domain 22, extraction_unnameable 5, extraction_blindspot_fired 3}`,
  Σ=72, coverage 1.0 over 50 in-domain, prevalence 0.06.
- **`plunit_run.txt` — 40/40** (`run_tests(commentary_census)`): per-state positive controls for all
  four `extraction_state` outcomes; contract-preserved silence on clear/unnameable/ood; domain-split
  `n_in_domain = n_corpus − Σood`; coverage decidable for both sources; q6 controls unchanged.
- **`oq86_contract.txt` — 14/14**: `extraction_reading/2` firing contract unchanged by the refactor.
- **`commentary_census.json`** — manifest-bearing artifact with `n_in_domain`, `coverage`,
  `prevalence`, `out_of_domain_buckets`, `prevalence_bucket` per source.
- **Pipeline:** `commentary_census` + `json_report` tasks `ok`. Classification untouched —
  `extraction_state`/`extraction_reading` are not on the `dr_type` path (structural witness).

## Declared seats (revisable)

(a) report both coverage and prevalence — taken; (b) `extraction_unnameable` = covered (the blindspot
question was answered, shape present but unnameable) — taken. Override either by re-declaring the
bucket (e.g. make `extraction_unnameable` an absence bucket) and re-running.

## Residual (extension point, not a live defect)

`consensus_provenance/2` and `seat_perceived_vs_real/4` are still partial-silent but are NOT census
sources, so nothing is broken today. Bring them up to the total shape if/when censused — the
convention (total hook + declared domain/absence/prevalence buckets + decidability flag) is in
`commentary_census.pl`'s header.
