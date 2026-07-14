# OQ-214 — deterministic theme-naming meter (`_theme_inventory`): Phase A writeup

**Date:** 2026-07-13 · **Status:** Phase A landed (offline, free); Phase B (end-to-end
pipeline run) held for operator spend-go + cold human read.

## What was built

`_theme_inventory(text)` in `agent/uke_narrative_orchestrator.py`, on the `_numeric_inventory`
(OQ-215) template. Deterministic extraction in six kinds, split by the **bucket rule** (a kind is
density-bearing only if flagging it in `rift3.md` would NOT be a false positive):

- **Density-bearing (move the auto-gate):** `anaphora` (≥2 consecutive sentences sharing a ≥3-word
  sentence-initial phrase), `causal_chain` (a single sentence stacking ≥2 because/therefore
  connectives, OR ≥2 near-consecutive sentences each carrying one — the syllogism tell).
- **Adjudication-only (listed, injected, NEVER density-scored):** `refrain` (normalized sentence
  recurring ≥2×), `aphorism` (generic sentence-initial subject + abstract-noun + no proper noun),
  `resonant_closer` (paragraph-final "the way X" / aphorism-shaped), `word_arithmetic` ("X minus Y
  equals Z" prose).

`density_per_1000` is computed from the two density-bearing kinds ONLY. The gate injects the full
candidate list into stages 7/8, and post-stage-8 escalates OPEN (one revision call over the
density-bearing lines, then `THEME_DENSITY_OPEN.md`) — **it never auto-rejects.** The invariant +
kill condition are written verbatim into the in-source comment block and locked by a regression
test.

## Evidence (raw: `theme_density_table.txt`; script: `calibrate_theme_meter.py`)

The SEED (defect) arms are embedded inside `../2026-07-12_oq218_scored_snare/blind_arm_payload_run{1,2,3}.md`,
split on the `STORY A/B` delimiter and labelled via `AB_KEY_run{1,2,3}.md`. Density-bearing per-1000:

| run | SEED (defect) | IMPROVED (v0.2) | note |
|---|---|---|---|
| 1 | 3.64 (anaph=18 causal=10) | 3.84 (anaph=18 causal=10) | **identical gateable counts** |
| 2 | 3.31 (anaph=14 causal=5) | 3.53 (anaph=14 causal=5) | **identical gateable counts** |
| 3 | 9.10 (anaph=36 causal=34) | 7.21 (anaph=30 causal=24) | only run where they diverge |

Controls (all PASS at threshold 8.0):
- **Positive:** SEED arms surface density-bearing candidates (max 9.10); named MANIFEST patterns
  caught by some kind (refrain ×40 run1, syllogism → causal ×34 run3, double anaphora → anaph ×36 run3).
- **Negative:** clean human originals 0.00–1.88 (well separated).
- **Flag-not-fail / earned-dense:** `rift3.md` = 5.12 and `the-empty-pan_rev2.md` = 3.87 — both
  FLAG (rift3's institutional refrain ×5, empty-pan's survival-math ×3) but stay below threshold.

## The finding (contradicts the naive design expectation)

**The density-bearing kinds do NOT separate the OQ-218 defect from its v0.2 fix.** In runs 1 and 2
the SEED and IMPROVED arms have *identical* anaphora/causal counts — the improvement changed only
the merit-correlated kinds (run1 refrain 40→20, aphorism 66→59) and word count. The SEED→IMPROVED
signal lives almost entirely in the kinds the operator ruling forbids gating on. Furthermore
earned-dense `rift3` (5.12) outscores two of the three SEED defects (3.64, 3.31): the meter
**cannot** separate earned-dense from lazy-dense on the gateable axis.

This is not a detector artifact — the identical counts across the improvement, and the clean 0–1.88
band on human literature, show the extractor measures real structure. It is the
"expected-precision-problem contract" the plan anticipated, now confirmed by data.

**The null IS the deliverable.** General law, learned twice: *a defect is mechanically gateable IFF
it has a merit-INDEPENDENT signature.* Counting had one (a digit means the same in a defect and a
masterpiece → `_numeric_inventory` gates); theme-naming does not (its surface IS the surface earned
prose uses on purpose → this meter collapses to a candidate list under adjudication). The difference
is not meter quality. **The bucket rule turned a false success into a true null:** had refrain stayed
on the gate, calibration would have LOOKED successful — defect-high, fix-low — while issuing revision
calls against rift3's creed and every earned refrain, actively suppressing craft. The refrain fight
is what made the result honest.

**Honest close.** WHAT SHIPPED: the last waivable absence-claim ("Theme-naming: none found") is now
backed by a mechanical high-recall candidate list — the self-certifiable scan is closed. WHAT DID
NOT: explanation over-run is not metered and cannot be by this approach; the register problem stays
where the Q2 double-No always put it — in the register, adjudicated by a reader, unreachable by
regex. The meter's existence does not move the register problem.

## Consequence for the threshold

`THEME_DENSITY_THRESHOLD = 8.0`, **PROVISIONAL**, set ABOVE every observed earned/good dense story
(rift3 5.12, run3-IMPROVED 7.21) so none of them gate. Only the single most extreme defect (run3
SEED 9.10) trips it, and it merely escalates OPEN. **Reopens at the first earned-dense encounter
above 8.0** (exactly as `NUMERIC_DENSITY_THRESHOLD` was provisional pending variance). Per the
plan's rule: a threshold set from a gap whose high side contains only lazy-dense LLM prose would
flag the first good dense story — so it is set from the earned-dense observations instead.

**The real value is the full 6-kind candidate list injected for per-instance adjudication** — where
the defect signal actually lives — not the deliberately narrow auto-gate.

## Verification artifacts
- `calibrate_theme_meter.py` — the calibration script (controls verdict, exit 0 = PASS).
- `theme_density_table.txt` — raw per-story density table + controls verdict (saved run).
- `python/tests/test_theme_inventory.py` — permanent regression test: density-bearing kinds fire;
  refrain never arms the gate (the bucket invariant, locked).

## Next: human read FIRST — it may moot Phase B
The cold human read is not merely ahead of Phase B procedurally; it answers the question the null
raised — *is the register problem even the right target?* If a cold reader hears the improved stories
as machine-made at the SENTENCE level, explanation over-run was a symptom and the meter treated the
wrong thing. Sequence: (1) get the cold human read (unsampled across the arc; gates the Clean Small
Song). (2) THEN decide — either ONE Phase-B run (witness the candidate-list injection threads a live
run, satisfying the engine-change-stays-OPEN rule; low-information now the gate barely fires) OR
close OQ-214 directly as *"candidate list shipped; explanation-over-run confirmed un-meterable by
mechanical extraction; register work reassigned to the read layer."* Phase B is a generation-pipeline
(ENGINE) change and stays OPEN until witnessed live IF chosen. No tokens spent.

**[EDGE] design-level claim surfaced, for operator ruling (not self-closed):** if the residual defect
class is merit-correlated and mechanically inseparable, no deterministic backstop closes it — counting
was plausibly the last defect with a mechanical signature, and the assisted posture becomes the
permanent architecture rather than a maturity stage. "Improve the pipeline" then shifts from *more
meters* to *better readers in the loop*. Recorded in OQ-214; the operator rules it, not this audit.
