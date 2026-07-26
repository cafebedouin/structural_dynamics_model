# OQ-216 stage-2 contract extractor redesign — witness record (2026-07-25)

Fix commit: `4878df78`. Full defect analysis and correction block: ISSUES.md OQ-216
(2026-07-25 entry). This dir holds the fresh-draw evidence; the no-cost probes ran
against committed/existing run dirs and are reproduced by the fixture suite.

## What was witnessed, in order

**1. Probes (no API), before the draw set:**

- Fixture suite `python/tests/test_stage2_contract_extraction.py`: 7/7 —
  canonical extracts, drifted-H3 extracts, misordered bounds correctly,
  no-header/EOF/missing-falsifier/prose-mention all fail loud.
- Real artifacts through old-vs-new extractor:
  - `rotation_seven_1784005086` (canonical): old==new **byte-identical** (2,294ch) — no
    regression on the clean path.
  - `the_floating_city_xixi_1784000706` (old false-negative): old 18,049ch (to-EOF
    over-capture; the shipped `invariant_contract_output.md` in that run dir is the
    18,266-byte production witness) → new **1,693ch bounded**.
  - `prometheus_1785030750` (drifted H3, was guard-blocked ×2): old 0ch → new
    **2,159ch extracts**, all four components present.
  - `quellcrist_1784034874`, `112_ergodocity_kids_1783916200` (no contract heading at
    all): old and new both reject; new names the reason.

**2. Fresh-draw set (Sonnet-5, production `_run_stage_2` path via
`stage2_draw_witness.py`, cached run-dir inputs, OQ-219 clause injected on all):**

| draw | tokens_out | block | verdict |
|---|---|---|---|
| prometheus_draw1 | 7,410 | 2,481ch | PASS |
| prometheus_draw2 | 6,942 | 0ch | GUARD-FAIL: no contract heading found |
| prometheus_draw3 | 6,010 | 1,981ch | PASS |
| quellcrist_draw1 | 4,932 | 2,976ch | PASS |

draw2 is the quellcrist/ergodocity shape (Step-0 working notes → SECTION 1 directly;
contract never written as a carryable block). Fail-loud is the CORRECT verdict there —
R13 genuinely has nothing to thread — so 3/4 is the expected profile, not a miss:
the redesign absorbs the recoverable drift shapes and still refuses the unrecoverable one.

## Round 2 (same day): root mechanism + post-fix witness

The assembled stage-2 prompt was SELF-CONTRADICTORY: `_run_stage_2`'s trailing
instruction — the last thing the model reads — said "Output TWO sections"
(Section 1 / Section 2), omitting SECTION 0, while stage2.md's system text mandated
three. The drift is exact compliance with the tail. Fixed in `a5b499be` (tail names
SECTION 0 first, Step 0 worked-silently).

Post-fix draw set (same driver, same cached inputs, clause injected):

| draw | tokens_out | block | verdict | first line |
|---|---|---|---|---|
| postfix_draw1 | 5,771 | 2,296ch | PASS | `# SECTION 0: INVARIANT CONTRACT` |
| postfix_draw2 | 5,758 | 2,884ch | PASS | `# SECTION 0: INVARIANT CONTRACT` |
| postfix_draw3 | 5,930 | 2,690ch | PASS | `# SECTION 0: INVARIANT CONTRACT` |

3/3 pass and the drift SHAPE is absent (no Step-0 preamble at all); token spread
tightened to 5,758–5,930 (pre-fix 6,010–9,304). n=3 — directional, not a rate.

**Shape taxonomy:** Shape A = complete contract, drifted heading (prometheus
originals; floating_city's misordering) — extractor recovers. Shape B = Step-0 notes
visible, carryable contract never authored (ergodocity, quellcrist, draw2 — each
missing substrate/inhabitation content entirely, so genuinely incomplete, not a third
heading variant) — fail-loud is the only correct behavior.

**floating_city downstream audit:** stage-9's INVARIANT FALSIFIER verdict is grounded
in true contract content (the blob's head IS the contract; no constraint-table/omega
symptoms in the adjudication); stage 10 never ran (ROUTE: STRATEGY exit). No witnessed
threading corruption in the shipped story; dilution retroactively unmeasurable.

## Standing observations

- Drift is Sonnet-5-endemic on floor-primary sources: across all prometheus draws ever
  taken (2 original + 3 here), 3/5 show a drift shape. The OQ-219 clause amplifies but
  does not cause it (ergodocity drifted 2026-07-12, pre-clause).
- Prompt-side re-assertion remains available as hygiene (reduces drift frequency); the
  extractor + content guard is the load-bearing correctness layer.
- The prometheus run can now be resumed: `--from-stage stage_3` after installing a
  passing draw as `stage_2_output.md`, or re-run `--from-stage stage_2` (~2/3 pass rate
  per draw). Which draw becomes the story is an authoring choice — operator's seat.
