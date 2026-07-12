# OQ-215 arm 3 — five-run variance (READOUT)

**Date:** 2026-07-12. **Design:** PROPOSAL.md (pre-registered before spend). **Driver:**
`python/audits/oq215_arm3_variance.py` — five serial full-pipeline runs of
`originals/the-empty-pan.md`, kill conditions enforced mechanically between runs. **Outcome:**
5/5 runs completed; no kill condition fired. Raw table: `RESULTS.tsv`; per-run driver logs
`run{1..5}_driver.log`; run dirs `uke/the_empty_pan_17838{66096,67840,70196,72143,74917}/`.

The three pre-registered metrics, read separately (per the ruling — one spend, three questions):

## M1 — R2's first live test (numeric register)

`<numeric_register>` complete in **5/5** stage-3 blueprints (first live firings ever — arm 1's
was truncated off). Densities per 1,000 words:

| run | stage_4 | stage_8 |
|---|---|---|
| 1 | 0.0 | 0.0 |
| 2 | 1.44 | 0.12 |
| 3 | 0.55 | 0.0 |
| 4 | 1.29 | 0.47 |
| 5 | 0.22 | 0.0 |

Reference: arm-1 (R2 absent) stage_4 = 2.98 / stage_8 = 0.48; anchored baseline 37.6 / 47.6.
Per-entry read of everything that survived to stage 8 across all five runs: **six number-words
total** ("two" ×1 in run 2; "one" ×4 in run 4 — ordinary prose usage, no tallies, no countdowns,
zero numerals, zero monotone sequences). Direction is consistent with R2 doing work (stage-4
densities sit below the no-R2 arm-1 draw), though with one arm-1 draw as the comparator the
attribution is directional, not established. Watch-item follow-up (OQ-214 class): run 3 (silver
assay world) carries word-arithmetic — the assayer reads the percentage aloud (earned:
institutional voice, in-scene action) and the narrator computes his fee in his head, concluding
"Mendoza had not stolen. The fee was the fee" (earned: positional access, acted on); the closer
"Percentage of loss. Value after extraction." shades thematic. Flag-not-fail; logged for
OQ-214's calibration set.

## M2 — the ruled D9 (kill condition lived here; it never fired)

**4/4 runs that reached stage 10 produced conforming D9 entries** — both witness subsections
present, zero bare-5s. Exemplar (run 1): the own-candidate is genuinely hostile (the passage
closest to correctable-error framing, with an explicit hostile-reviewer reading), the refutation
runs through the invariant's own mechanics ("the true value is not hidden from better
measurement; it is destroyed by the act of bringing tea to measurement"), and the stage-9
finding is adjudicated explicitly (CONCUR). Run 5 never reached stage 10: stage 9 routed
STRATEGY at the cycle limit (repetitive ending named as the weakness) and the pipeline exited
for human review — the review loop refusing to certify is the designed behavior, not a K2 event
(K2 = a *recorded score* without witnesses; a run that never grants VALIDATION records none).
Refutation quality across runs 1–4 = operator read, pending.

## M3 — invariant survival variance

**Stage-9 blind falsifier: HOLDS 5/5. Stage-0 `missing_floor` authored 5/5. Stage-2 SECTION 0
contract present 5/5.** Five *different* worlds, each with an in-principle-unreadable substrate:
tea *zhēnwèi* (true-taste that cannot survive the journey to measurement); scanned stone whose
reading "is a lie the moment it appears — the act of asking reorganized what you asked about";
assayed silver where "the paper becomes the thing that moves through the world while the stone
stays behind"; inner-ear equilibrium vs a Posture Index zero-pointed to the Margrave's own
soldiers (the floor made explicit); a dyer's madder-moment that "dies the instant the inspector
asks you to demonstrate it." Foam-class adjudication of each substrate = operator read, pending
— but every candidate is instrument-unreadability-in-principle, not hidden-value.

## Threshold recalibration (executed per the 2026-07-11 ruling)

`NUMERIC_DENSITY_THRESHOLD` 25.0 → **10.0**: the witnessed improved ceiling is ~0.5/1000 (six
improved runs: five arm-3 + arm 1), the defect band 37.6–47.6. 10.0 is 20× the improved ceiling;
an earned-instrument story tripping it costs one revision call + an OPEN read (the gate
escalates, never auto-rejects — the designed rift3-class behavior).

## What remains (operator)

1. Read verdicts: refutation quality (M2, runs 1–4), foam-class substrates (M3), run-3
   word-arithmetic adjudication, and at least one full story read (the meter is a proxy).
2. Run 5's STRATEGY-exit story is at `stories/` rev with its named weakness — a normal
   human-review handoff, not a defect.
3. Protocol arms 4–6 (R5/R6, R7, R9 spot checks) were witnessed live on arm 1; re-spot on any
   arm-3 run if desired. Arm 7 (R12 model A/B) remains optional and separate.
4. OQ-215 close-out decision.
