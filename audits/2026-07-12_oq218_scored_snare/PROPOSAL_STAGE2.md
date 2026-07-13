# OQ-218 Stage 2 — PROPOSAL (pre-registration; authored before any batch run)

**Date:** 2026-07-12. **Authorization:** Stage-1 ruling (STAGE1_ADJUDICATION.md) — "the gate
opens; the fresh-source batch is released."

## Question

Does the scored-Snare class weakness (the rev6 shape: gate-green story whose invariant survives
only partial-via-ending) appear on fresh scored-Snare sources, and does UKE_STORY v0.2 repair it
where it appears? Retrospective validity preserved by model pinning (below).

## Sources (3, distinct instrument classes; Stage-0 certified 2026-07-12, commit `434ec74d`)

1. `agent/narrative_transform/originals/the_good_name_book.md` (credit standing)
2. `agent/narrative_transform/originals/the_eighth_commentary.md` (examination)
3. `agent/narrative_transform/originals/the_table_of_winters.md` (actuarial table)

## Per-source chain (pre-registered)

1. **Pipeline run** — `agent/uke_narrative_orchestrator.py --skip-engine <src>` (arm-3 driver
   template), **all ten Anthropic stages pinned to `claude-sonnet-4-5-20250929`** via
   `--stage-N-model` overrides (operator instruction; matches the Stage-1 seed era; R12
   confound dissolved; 4.5-anchored density baselines remain valid). Runs SERIALIZED.
2. **Read the seed** against the rev6 shape: blind stage-9 falsifier verdict + density +
   gate route recorded per run. Whether the weakness appears is an OUTCOME, not an expectation
   — either answer is data (plan-discipline: no pre-asserted result).
3. **v0.2 improvement pass** (this instance = improver): triage + stance declared before any
   change; deformation budget 1–2; §4 gates per change; manifest per §7; consolation level
   named. Pause pre-§6 per executor separation.
4. **Blind arms per source** — unlabeled randomized A/B (titles/trailers stripped), two fresh
   instances, different model families (Sonnet arm + Gemini arm), payload only. Q1/Q2 verbatim;
   **Q4 is the discrimination instrument** (Stage-1 instrument note); Q3 reserved for a human
   arm if available.
5. **Operator legs** — contaminated §1a audit + ruling, per source. Both legs must pass per
   source for that source to count toward "repair confirmed at class level."

## Pre-registered outcomes

- **Class weakness appears + v0.2 repairs it** (re-founded grain, blind-arm discrimination):
  supports keeping R3(b) with the v0.2 chain as the standing repair route.
- **Class weakness appears + v0.2 fails to repair** (prettier correctable-bias story): the
  standing fallback executes — R3(b) drops to the hard ban WITH this evidence.
- **Class weakness does not appear** (seeds hold the invariant without repair): the rev6 case
  reads as variance, not class; R3(b) stands; the watch flag closes.
- Mixed results: adjudicated per source by the operator; the entry must not average them.

## Deviations log

- **2026-07-12, run 2 first attempt (`the_eighth_commentary_1783908190`):** halted at stage 3 by
  the OQ-216 cap-hit guard (`12288 >= 12288`) — fail-loud as designed, no partial output consumed.
  Plumbing fix applied per the guard's prescription: `MAX_TOKENS["stage_3"]` 12288 → 16384
  (commit `25b27343`; run 1's blueprint had reached 11,546 — cap marginally sized, not a one-off).
  No measurement instrument touched; failed run dir kept as evidence; run 2 relaunched. This is
  a loud retry after a witnessed infrastructure fix, not a silent one.

## Kill/stop conditions

- Any pipeline run fails (rc != 0 or no run dir): stop the batch, report, no silent retry.
- Any improvement pass that cannot declare a stance from the triage: that source routes to
  operator review instead of improvement.

## Cost

~$1.7/run × 3 (arm-1 precedent) + v0.2 passes (improver time) + 6 blind arms (~$0.05 Gemini;
Sonnet agent negligible). Within the released ~$7–10 envelope.
