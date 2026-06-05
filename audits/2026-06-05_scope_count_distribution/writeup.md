# SCOPE Count-Distribution Probe — the 7-7-7 watch is RESOLVED (coincidence + run noise, not an implicit target)

**Date:** 2026-06-05. **Question:** after the de-leak removed the 3-axis cap, the first no-cap
witness returned exactly 7 axes / 0 deferred on three distinct mid-richness topics
(`audits/2026-06-05_generation_pipeline_deleak/evidence/scope_t{1,2,3}.txt`) — did an implicit
count target re-form, or was that mid-richness coincidence? **Probe:** SCOPE-only decomposition
(`DRAuditOrchestrator._step_decompose`, temp 0.2, no ceiling) over an 8-topic battery deliberately
spanning structural richness, in TWO ARMS (A = current prompt; B = pre-`d179423d`, because the
lens-diversity instruction is in the SCOPE system prompt — `c-orchestrator.py:177,421` — and a
one-arm FAIL could not have named its lever). Driver:
`python/audits/scope_count_distribution_probe.py`; raw manifests + summary in `evidence/`.

**Pre-registered criteria (set before the runs; see plan + OQ-75):** the pass is NOT global
range — after a flat prior on mid-richness topics, the live question is whether the upper tiers
(T4–T7) spread among themselves. Upper-tier clustering behind a binary floor = FAIL (masked
target) even with healthy global range. 0-deferred everywhere = part of the flat signature.
T7's distinctness must be shown (pasted deltas/observables), not asserted.

## Result table (selected = generation_sequence length)

| id | tier | arm A: cand/sel/def | arm B: cand/sel/def | kernel (readings) |
|---|---|---|---|---|
| T1 | binary (drive-on-right) | 3 / 3 / 0 | 4 / 4 / 0 | — |
| T2 | mountain (2nd law) | 4 / 4 / 0 | 3 / 3 / 0 | — |
| T3 | thin-coord (DST) | 7 / 6 / 1 | 6 / 5 / 1 | — |
| T4 | moderate (minimum wage) | 8 / 6 / 2 | 5 / 5 / 0 | — |
| T5 | mid-bridge (gig economy) | 5 / 5 / 0 | 5 / 7 / 0 | B: kernel (3) |
| T6 | rich-kernel (personhood) | 6 / 6 / 3 | 6 / 6 / 0 | both: kernel (4) |
| T7 | very-rich (US healthcare) | 11 / 11 / 0 | 12 / 9 / 3 | — |
| T8 | replicate of T1 | 5 / 4 / 1 | 5 / 5 / 0 | — |

(16/16 calls succeeded; raw manifests `evidence/manifest_T*_{A,B}.json`, console log in
`evidence/summary_rows.json`.)

## Verdict against the pre-registered signatures: PASS on all

- **Upper tiers spread among themselves:** A: 5/6/6/11; B: 5/7/6/9. No clustering — the
  masked-target sub-signature does not fire. T7's jump (11/9) is decisive.
- **Richness ordering respected:** binary/mountain 3–5 < mid 5–7 < very-rich 9–11.
- **Deferrals occur** (A: T3=1, T4=2, T6=3, T8=1; B: T3=1, T7=3) — selection/triage
  demonstrably fires, unlike the original 0-0-0.
- **Replicate noise small:** T1 vs T8 within ±1 in both arms, vs across-topic range of 8.
- **No fabricated-upward:** binary topic 3–5, below the ≥6 trip (above the 1–2 hypothesis, but
  the pasted T1 axes — coordination equilibrium / infrastructure lock-in / LHT manufacturing
  asymmetry — are genuinely distinct facets: hypothesis under-estimation, not padding).
- **No clamping:** T7 ≠ T1 by 6–8 axes.
- **Arms agree** → the lens-diversity instruction is NOT pinning counts; no lever attribution
  needed. (Arm-level differences are within run noise; one mode flip: T5 decomposed as a
  contested kernel in arm B only, T6 as kernel in both — kernel-recognition is itself somewhat
  noisy at temp 0.2; recorded, not gating.)

**T7 distinctness (shown):** arm A's 11 axes pair into distinct mechanisms with disjoint
observables (employment-tax coupling / age-65 cliff / Medicaid means-test / risk-pool
fragmentation / FFS / PBM rebate opacity / adverse selection / cost-shifting / state expansion
variation / job-lock). One borderline composite: `cost_inflation_mechanism` ("combined effects
of FFS …") — 10/11 clean. Arm B's triage deferred `tax_subsidy_distortion`, which overlaps
`employment_insurance_coupling` — §4 visibly doing overlap-triage, the behavior whose absence
the original 0-deferred uniformity made suspicious. Full pasted deltas/observables in the
session log; manifests in `evidence/`.

## Reading of the original 7-7-7

The bridge replicate settles it: gig economy — 7 axes in the original run — returned 5 (arm A)
/ 7-as-kernel (arm B) here. Combined with the spread above, the original uniformity was
**mid-richness coincidence plus within-topic run noise (±1–2) at temp 0.2**, not an implicit
target. Three mid-richness topics were an unlucky sample — exactly the ambiguity the
richness-spanning battery was designed to break.

## Consequence

OQ-75's 7-7-7 sequencing watch is **resolved**: Stage-2 is not gated on a SCOPE-framing fix.
The axis-count distribution at Stage-2 scale should still be reported (cheap, and this battery
is n=8 topics), but it is a readout, not a gate.
