# OQ-215 arm 3 — five-run variance (PROPOSAL, pre-registered before execution)

**Date:** 2026-07-12. **Spend authorized:** operator, 2026-07-12 ("spend the five variance runs
with the three metrics pre-registered separately"). **Driver:**
`python/audits/oq215_arm3_variance.py` — five SERIAL full-pipeline runs of
`agent/narrative_transform/originals/the-empty-pan.md` (`--skip-engine`, matching the arm-1 and
baseline configuration; serialization per the one-pipeline-at-a-time rule).

## Code state under test

First arm run at the post-ruling instrument state: R2's `<numeric_register>` reaching stage 4 for
the first time (arm 1's stage_3 truncated before it emitted — caps since raised + cap-hit guard),
the composed D9 (own strongest candidate + stage-9 finding adjudication + HOLDS-guard), and the
orchestrator-injected validation mode. Baseline relabel in force: Forty-Hertz is a PARTIAL
(Part One holds, Part Three reverts) — not a clean invariant-preserved exemplar.

## Three metrics, answered separately — one spend, three questions (do not smear)

**M1 — R2's first live test (numeric register).**
Per run: (a) stage_3 output contains a complete `<numeric_register>…</numeric_register>`;
(b) stage-4 and stage-8 densities from the inventory sidecars; (c) the per-entry read of
surviving numbers (who holds the number, on what instrument, acting how — the rift3 lens).
HYPOTHESIS (not expectation-as-fact): with the field live, narration stays un-denominated and
KEEPs are in-scene actions. Reference points: arm-1 stage_4 = 2.98 and stage_8 = 0.48 (achieved
WITHOUT R2); Forty-Hertz baseline stage_4 = 37.6, stage_8 = 47.6. M1 answers "does R2 hold or
improve the un-denominated register when it actually arrives" — it cannot be answered by the
density aggregate alone; the per-entry read is part of the metric.

**M2 — the ruled D9 (kill condition lives here).**
Per run: the final stage_10 D9 entry must carry BOTH labeled subsections —
`STRONGEST CANDIDATE (own):` and `STAGE-9 FINDING ADJUDICATION:`.
**KILL CONDITION (operator): ANY run whose D9 records a score without both subsections (a
"bare-5") STOPS THE ARM immediately** — the compose didn't take, and that finding outranks any
variance number. The driver enforces this mechanically between runs (label check; the witnessed
arm-2 bare-5 trips it, a conforming entry passes — two-sided control below). Score
distribution and refute/concede quality are recorded for the operator read; the mechanical check
gates, the read verdicts.

**M3 — invariant survival variance.**
Per run: (a) stage-9 blind falsifier verdict (HOLDS / LOST / UNVERIFIED) — final cycle;
(b) stage-2 substrate class (foam-class / instrument-unreadable substrate present — recorded for
operator read, not mechanized); (c) R14 floor: Stage-0 `missing_floor` authored + present in the
stage-2 contract. Success bar from the plan: foam-class substrate in most runs (stage-2 gate
confirmed), floor present (contract confirmed), stage-9 HOLDS rate high. A LOST is not
automatically a failure of the arm — it is a datum; the operator reads the flagged passage.

**Threshold recalibration output (operator ruling 2026-07-11):** the five stage-8 densities are
the first real data on the improved-vs-anchored boundary for generated output;
`NUMERIC_DENSITY_THRESHOLD = 25.0` (provisional, two-point) is re-set from them after the
operator read.

## Kill conditions (mechanical, enforced by the driver between runs)

1. Any pipeline run exits non-zero or a stage errors → STOP, report.
2. M2 bare-5: final stage_10 contains a D9 score without both labeled subsections → STOP.
3. NUMERIC_DENSITY_OPEN.md flagged in any run → not a stop (fail-visible is the design), but
   recorded prominently for the read.

## What this arm does NOT decide

Foam-class substrate quality, refutation quality, and earned/unearned adjudication of surviving
numbers are operator reads over the recorded evidence — the driver collects; it does not verdict
them. The meter is a proxy; the read is the verdict.
