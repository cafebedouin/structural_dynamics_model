# OQ-218 Stage 2 — Batch READOUT (2026-07-12)

**All three runs complete** per `PROPOSAL_STAGE2.md`. Model-pinned (all Anthropic stages
`claude-sonnet-4-5-20250929`), serialized, one deviation (stage-3 cap raise, logged in the
proposal). Spend: pipelines $2.22 + $0.35 (failed attempt) + $1.47 + $1.51 ≈ $5.55, plus six
blind arms (~$0.15). Within the released envelope.

**Adjudication status: everything below is the improver's read + machine verdicts + blind-arm
outputs. Per-source adjudication (contaminated §1a + ruling) is the operator's; the class
verdict maps to a pre-registered outcome but is NOT ruled here.**

## Per-source table

| | run 1: good_name_book | run 2: eighth_commentary | run 3: table_of_winters |
|---|---|---|---|
| Seed | "The Keeping" (7,690 w) | "The Red Ink" (5,752 w) | "The Hands That Measure" (7,706 w) |
| Stage-9 falsifier | HOLDS — "measurement itself converts keeping into debt"; accurate AND destructive | HOLDS — "fairness and adequacy structurally incompatible" | HOLDS — "epistemological incommensurability", not measurement error |
| Stage-8 density | 0.0 | 0.0 | 0.78 |
| Stage-10 | D9=5, 39/40, HUMAN REVIEW | D9=5, PUBLISH | D9=5/5, 39/40, HUMAN REVIEW |
| v0.2 triage | **Type A** (theme-naming, F39 residue, refrain ×4, warm ending) | **Type A** (§VII thesis-essay ×4) | **Type A** (mild; syllogism ×4, double anaphora, hope-clause) |
| Pass | Path A, net −401 + workshop deformation | Path A, net −362 + rubric-line deformation | Path A, net −215, **subtraction-only** (deformations rejected at governor) |
| Sonnet arm | Named deformation blind ("founding instance retroactively absorbed"); Q2 improved = bilateral-logging line | Named deformation blind ("regression, not reform"); Q2 double-No | **Exact delta recovery**; seed's extra text judged "restatement A leaves implicit"; Q2 double-No + house-style critique |
| Gemini arm | Named deformation on Q1+Q4; Q2 seed = **the cut F39 line** | Named deformation on Q1+Q4; Q2 tie (shared line) | **Non-discriminating; confabulated** (claimed seed omits Master Feng — false, grep ×5 both; both Q2 quotes shared) |

## Class finding (maps to pre-registered outcome 3)

**The rev6 scored-Snare weakness (Type B, correctable-real) appeared on 0 of 3 fresh sources.**
All three seeds carried the invariant on structural grounds mid-story (not merely
ending-foreclosure); no better-meter subplot appeared anywhere; run 3's seed even stages the
falsifier (seasonal adjustment factors) inside the antagonist's own scene and defeats it
structurally. Under the pre-registration: *"Class weakness does not appear: the rev6 case reads
as variance, not class; R3(b) stands; the watch flag closes."* — pending the operator's
per-source adjudication and ruling. Caveat carried: three sources is a small N; all three
sources were authored by the same instance that authored rev6's source (the-empty-pan was NOT —
different author era — so the batch does not perfectly replicate rev6's provenance).

## The pipeline's characteristic defect at this regime is Type A, not Type B

3/3 seeds triaged to **explanation over-run** — the story states its thesis after showing it
(philosophy-seminar endings, interior essays, repeated syllogisms, F39 vocabulary leakage in
run 1). The R1–R14 battery killed counting; the residual template signature is REGISTER-level:
Sonnet r3's blind critique enumerates the house patterns (arc-closing refrains, glossed
tricolons, causal-chain formulas, repeated scene-closers). This feeds OQ-214's territory: the
theme-naming meter is the right next deterministic backstop, and "explanation over-run" is its
core target class, now witnessed 3/3 on fresh material.

## v0.2 instrument findings

1. **Deformations land blind.** Both addressed deformations (workshop; rubric-line) were named
   by both arms via Q1/Q4 with the pre-registered address recovered in the arms' own words
   ("founding instance retroactively absorbed"; "awareness produces regression, not reform").
2. **Q4 is confirmed as the discrimination instrument** (Stage-1 note replicated ×3): every
   discriminating result came through the comparative frame; Q1-alone separated nothing on
   near-identical pairs.
3. **Subtraction-only deltas sit below one arm's floor.** Sonnet recovered the −215-word delta
   exactly; Gemini confabulated a difference (false Master Feng omission) rather than reporting
   near-identity. For future subtraction-only passes, a "these may be identical — say so if so"
   escape should be added to the blind prompt, or Q4 results from weaker arms discounted.
4. **Residue-inversion (run 1, replicated across both arms at Stage 1's cut-ratification's
   mirror):** cold readers Q2-pick framework residue ("ontologically real") as inimitable —
   out-of-register jargon reads as originality. The transparency gate and Q2-inimitability pull
   opposite ways on residue; Q2 alone must not adjudicate keeps.
5. **The governor can return empty.** Run 3's offense stage rejected both deformation
   candidates; the blind result (no break lost, redundancy removed) ratifies that a null
   deformation pass is a valid v0.2 outcome, not a skipped stage.
6. **Q2 yield is low on pipeline prose:** 6 arm-answers across the batch produced exactly one
   improved-story inimitable quote (r1 bilateral-logging line). Consistent with the house-style
   finding: sentence-level inimitability is scarce in this register regardless of structural
   sharpening.

## Infrastructure

One OQ-216 cap-hit guard fire (stage_3, run 2 first attempt) — fail-loud worked, cap raised
12288→16384 (commit `25b27343`), loud retry clean. The guard built for the arm-1 truncation
class caught its recurrence at the first opportunity.

## Files

Seeds: `stories/the_{good_name_book,eighth_commentary,table_of_winters}_rev1.md`. Improved:
`the_keeping_v02_pathA.md`, `the_red_ink_v02_pathA.md`, `the_hands_that_measure_v02_pathA.md`.
Manifests: `MANIFEST_RUN{1,2,3}.md`. Arms: `blind_arm_{sonnet,gemini25pro}_run{1,2,3}.md`.
Keys: `AB_KEY_run{1,2,3}.md`. Run dirs: `agent/narrative_transform/uke/…` (committed).
