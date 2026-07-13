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

## Addendum (2026-07-13): the absence claim's witness

The claim "no better-meter subplot appeared anywhere" is now swept, not asserted. Correctable-
vocabulary sweep (recalibrat*/rebuil*/fairer/representative sample/true value/mismeasur*/
improve-the-{system,template,...}/reform*, case-insensitive, hits read in context):

- **Positive control (rev6):** 16 hits — the known rebuild subplot recovered exactly
  (recalibration ×8, rebuild/rebuilt ×3, representative sample ×3).
- **Seed 1 (The Keeping): 0 hits.**
- **Seed 2 (The Red Ink): 1 hit — homograph.** "The crowd shifts and reforms around the board"
  (bodies re-forming at the results posting; not reform-the-system). Adjudicated non-hit.
- **Seed 3 (The Hands That Measure): 0 hits.**

Sweep script inline in the session transcript; control fired before the absence was read.

## Addendum (2026-07-13): ergodicity run (first Sonnet-5-default production run)

`112_ergodocity_kids_1783916200` (operator-invoked, default models = claude-sonnet-5 post-
migration): completed to a final story; **stage 2 omitted the SECTION 0: INVARIANT CONTRACT
block** (folded the material into SECTION 1 "Step 0" — content at the wrong address), so R13
threading ran dead and stage 9 could only report "contract not available." The OQ-216 stage-2
census candidate fired in production; fail-loud guard built + two-sided-witnessed (fires on the
ergodicity stage_2, passes on all three batch stage_2s). Also the first Sonnet-5 density point:
4.84/1000 (11 number-words, 0 numerals) on a 2,280-word story — below threshold, above the 4.5
batch's 0.0–0.78; re-baseline datum, not a baseline. Register note (operator's Web-Claude read):
the "resonant closer" tic — each unit landing on a summarizing image, often "the way X" — logged
to OQ-214's calibration set.

## Appendix (2026-07-13): OPERATOR RULING — OQ-218 close

Filed verbatim in the OQ-218 entry (ISSUES.md); mirrored here per filing instruction. Summary:
STAGE 1 repair CONFIRMED both legs (v0.2 Ω_E1 → resolved: Type-B repair POSSIBLE where the
grain's shadow is live — possibility, not guarantee). STAGE 2 operator leg PASS ×3 (Wang-scene
falsifier structural; run-1 triage ratified, workshop deformation load-bearing; consolation
levels ratified). CLASS RULING: rev6 = VARIANCE, NOT CLASS (1 weak in 8 scored-instrument runs,
ending-saved); scope = certified-grain sources. R3(b) CONDITIONAL → STANDING (stage2.md hard-ban
fallback documented, not armed); watch CLOSES with travelling reopen condition (from the
Sonnet-5 re-baseline: one Type-B seed = re-arm; two in any five consecutive runs = reopen with
this trail). Residual Type-A class = OQ-214's mandate. NOT RULED: v0.2 §5 untested (Ω_E2 open);
sentence-register ceiling (OQ-214/OQ-220); human gold arm gates PUBLICATION (Clean Small Song
first), not this close; improved stories are audit artifacts (rev6-improved carries the
operator's Cliffside 806-812 flag).

Operator-leg additions to the record: the run-3 foreclosure is the same temporal-category move
as Stage 1's ("the trace records the preparation") — 2-for-2 from the same authoring instance;
the house's signature unreadability mechanism, watch for template-hardening at higher N. Run-2
residue note: the kept crystallization block survives the pass — explanation over-run reduced,
not eliminated (OQ-214's job, not v0.2's).
