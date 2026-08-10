# PREREGISTRATION — OQ-151 dual-gauge crosstab + refinement census

**Registered:** 2026-08-09, before any crosstab or census run (md5 of this file
recorded in `audit_log.md` above the first crosstab line — amendment 5).
**Script:** `python/audits/oq151_dual_gauge_crosstab.py` @ commit `3607086f`.
**Detector under audit:** `stakeholder_seats:empty_chair_state/2` @ commit `e07fba7b`.

## Cell semantics (3×3, h1_band × h1_stakeholder, strata {null, 0, >0})

- **(0, >0)** — the observer orbit glues while the authored parties fracture.
  This is the REALIZABLE form of OQ-151's "role-H¹>0 ∧ power-H¹=0" question
  (the role gauge itself is declined; the seat frame is its refinement).
- **(>0, 0)** — observer fracture over seated consensus; every member id is
  intersected with `empty_chair_state/2` in the per-item verification step.
- **Null strata** are reported with reasons (`sheaf_status`/
  `sheaf_undetermined_reason` for h1_band; `n_real` for h1_stakeholder),
  never coerced. A missing key RAISES; `.get(..., 0)` is forbidden (the
  OQ-51/OQ-207 silent-zero trap).
- Five independent per-leg tables, never merged (GAP-31).

## Refinement census (the OQ-151 headline number)

Per leg: full `empty_chair_state/2` histogram + the exhaustive 8-token
partition of the mcc candidate set (= constraints whose
`consensus_provenance/2` verdict is `manufactured_consensus_candidate*`):

- `empty_chair_dissent` / `empty_chair_dissent_untypeable` — genuine
  typed-dissent stratum (the detector's positive class).
- `excluded_untyped` — the OLD false-positive class (untyped chair counted as
  manufactured consensus by the unfiltered flag).
- `excluded_concurs` / `excluded_concurs_untypeable` — typed chair agrees.
- **Declared expected-zero on the mcc set** (structurally impossible cells):
  `included_plural` (mcc requires a unanimous room), `included_insufficient`
  (mcc requires ≥2 real included seats), `no_excluded_seat` (mcc requires a
  chair). **Any nonzero expected-zero cell is a Pattern-2 fork alarm**
  (detector vs `consensus_provenance/2` disagreeing about the room).
- **Checkable identity:** Σ over all 8 tokens restricted to the mcc set
  == |mcc candidates| per leg.

## Per-item verification plan

Every `empty_chair_dissent*` member on testsets/ + every member of the two
determinate off-diagonal cells (0,>0) and (>0,0) on testsets/; on the four
big legs, all dissent members (small stratum) and ≥10 sampled members per
determinate off-diagonal cell. Each re-derived in swipl, per-item lines
pasted into `per_item_verification.log`.

## Consumer sweep (amendment 1)

Grep consumers of `manufactured_consensus_candidate` (both arities/tokens)
across `prolog/`, `python/`, `audits/`, `ISSUES.md`; record in the WRITEUP
whether any live recorded claim rests on the unfiltered candidate set. Any
such claim gets its own correction line in Commit 3 or its own OQ.

## Freshness criterion (Step-0 revised)

All five manifests share the same `code_commit`; `git status --porcelain --
prolog/ python/` pasted at run time with every dirty engine path adjudicated;
`code_dirty:true` recorded as expected-and-explained (unachievable false —
operator's unrelated untracked files; auto-regenerated validation_suite.pl).
Each leg md5-fingerprint-bracketed around its regeneration run.
