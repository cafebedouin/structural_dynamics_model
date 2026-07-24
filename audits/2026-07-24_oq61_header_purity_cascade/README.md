# OQ-61 — Corpus header purity/cascade line: three operator rulings implemented

**Execution date:** 2026-07-24. **Scope:** report-text / aggregation only — no
classification path touched (proven, see `behavior_preservation.txt`).

Implements the three operator rulings on OQ-61 (plan
`review-oq-61-from-issues-md-jolly-fairy.md`). All three are report/aggregation
changes; the change is purely additive to the JSON contract.

## The three rulings and what landed

- **Q1** (cascade saturation) → header shows the **severe fraction**, not the
  saturated categorical (`cascading` fired at NumSevere≥3 absolute — witnessed
  633/643 severe, 211× the threshold). JSON keeps the 4-token `network_stability`
  categorical **byte-identical**. New siblings: `network_n_severe`,
  `network_n_drifting`, `network_cascade_count_threshold`, `severity_by_type`.
- **Q2** (type-composition restatement) → keep raw bands **and** add a type×band
  cross-tab headlining the off-diagonal residual (cover-story / fragile-rope
  candidates). Render-only; new per-row `purity_class` field carries the split.
- **Q3** (hidden no-access count) → split the unscored bucket into the two
  existing absence tokens: `gate_fail` (−1.0 sentinel) vs `no_data` (`unknown`).
  New siblings `purity_n_gate_fail`, `purity_n_no_data`. `malformed`
  (out-of-range) is a fail-closed guard-class — the emit halts on it, it is NOT a
  fifth vocabulary token.

## Witnesses

### Q3 split across all five live legs + kernel_v1 archive (`q3_sweep.log`)

Via `classify_corpus` (fresh process, `asserta` overlay, serialized). Every leg's
sum-invariant `scored + gate_fail + no_data == n_total` closes.

| leg | scored | gate_fail | no_data | total | unscored | status |
|---|---|---|---|---|---|---|
| testsets | 153 | 35 | 11 | 199 | 46 | REPRO OK (46=35+11) |
| testsets_haiku | 492 | 466 | 2 | 960 | 468 | REPRO OK (468=466+2) |
| testsets_flash | 668 | 212 | 80 | 960 | 292 | REPRO OK (292=212+80) |
| testsets_kimi | 700 | 29 | 276 | 1005 | 305 | FRESH (measured) |
| testsets_sonnet | 930 | 1 | 70 | 1001 | 71 | FRESH (measured) |
| kernel_v1 (archive) | 1102 | 2 | 2 | 1106 | 4 | REPRO OK (4=2+2) |

The four census-target legs reproduce the `PYTHON_SWEEP_2026-07-23.md`
sentinel+flip decomposition **exactly**. `testsets_kimi`/`testsets_sonnet`
postdate that sweep — their split is measured and recorded fresh (no
"reproduces" claim), gated only by the sum-invariant + the classifier fixture.

### Behavior-preservation (`behavior_preservation.txt`)

1. **Determinism:** two changed-code runs, canonicalized (list fields sorted,
   manifest dropped) → **0 changed keys, 0 added/removed** (per_constraint and
   diagnostic). Nondeterminism confined to the manifest.
2. **Additive-only:** HEAD baseline vs changed, canonicalized →
   - diagnostic **added** keys: `network_cascade_count_threshold`,
     `network_n_drifting`, `network_n_severe`, `purity_n_gate_fail`,
     `purity_n_no_data`, `severity_by_type`; **removed:** none; **changed shared:
     none** (`network_stability` token byte-identical, `purity_n_scored`/`_total`
     unchanged).
   - per_constraint **added:** `purity_class`; **changed shared: 0**.

### Fixtures (permanent boundary controls)

- `prolog/tests/test_purity_absence_class.pl` — 14 tests: pure classifier over
  all four classes incl. `malformed` (both signs; unreachable via the clamped
  real `purity_score`, so only the pure `purity_absence_classify/2` seam witnesses
  it); real fetch path (bare→no_data, <MinN→gate_fail, golden→scored);
  partition + **token-move** (promote gate_fail→no_data, the matching addend
  moves — the sum-invariant alone would miss a misfile); `purity_score`
  determinism. All 14 pass. Existing `test_purity_absence.pl` (17) still green.
- `python/tests/test_oq61_network_render.py` — 16 tests: all four network-render
  branches + fail-closed inconsistency asserts (n_severe>n_drifting,
  token/threshold mismatch, cascading-at-zero-drift INCONSISTENT marker); Q2
  marginal asserts on well-formed + deliberately-broken tabs; Q1 backstop
  severe-total assert. All 16 pass. (No pytest — self-contained `main()` runner
  per AGENTS.md §5.)

### Rendered header (`rendered_header_testsets.txt`)

All three renders visible; every marginal assert in the Python renderer passed.

## Q1 pre-registered decision rule — RESIDUAL FOUND → ESCALATED (`q1_decision_rule.json`)

The plan fixed, before the run, a decision rule: "no residual signal beyond type
composition" iff (a) for every type |severe-in-type/drifting-in-type −
severe-overall/n_drifting| ≤ 15pp, AND (b) off-diagonal severe mass (severe in
pristine|sound-expected types rope+mountain) / n_drifting < 5%. If either fails →
a residual exists → **escalate to the operator (revisiting the Q1 ruling is
operator territory).**

Evaluated on **kernel_v1** (breadth, N=1106, where the saturation was witnessed;
n_drifting=642, n_severe=617, 96.1% overall):

- **Rule (a) FAILS:** `mountain` 10/15 = 66.7% severe (Δ=29.4pp > 15pp). `rope`
  82.9% (Δ=13.2pp, under). The high-mass types (tangled_rope 98.5%, snare 100%)
  sit near the 96.1% overall; the deviation is real but concentrated in
  low-severity types.
- **Rule (b) FAILS:** off-diagonal severe mass = 68/642 = **10.6% > 5%** — 68
  severe drifting rope/mountain constraints whose type "expects" clean purity.
  These are exactly the Q2 cover-story candidates.

**Verdict: a residual beyond type composition exists.** Per the pre-registered
rule this is **escalated to the operator** — the question of whether the header
severe-fraction alone suffices, or whether the categorical's declared future home
(per-component severity) is now warranted, is an operator ruling. The landed
change already makes the residual *visible* (the Q2 off-diagonal tab + the Q1
`severity_by_type` backstop tab), so it is not hidden — the escalation is about
representation, not concealment. testsets (sparse, n_drifting=35) also fails both
rules but is not the diagnostic corpus for this call.

## Not in scope (unchanged)

No threshold/assessment-logic change (helper extraction is behavior-preserving);
no deletion of `purity_summary`/`network_stability`/the cascade predicate; no
`purity_by_type` JSON field; no third absence token; no band-vocabulary
unification (OQ-62); no classification-path change.

## Doc-currency follow-up (flag, not fixed here)

CLAUDE.md "THREE LIVE LEGS" and MEMORY.md are stale — five live legs now exist on
disk (`testsets`, `testsets_haiku`, `testsets_flash`, `testsets_kimi`,
`testsets_sonnet`). Flagged to the operator for a separate KNOWN_STATE/CLAUDE.md
update (out of scope for the OQ-61 code).
