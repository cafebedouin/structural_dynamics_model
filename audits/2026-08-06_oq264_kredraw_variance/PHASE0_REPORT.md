# OQ-264 Phase 0 report — k=3 variance floor of pooled idiom SHARE (free-data arm)

Date: 2026-08-06. Inputs: the six committed manifests (PROPOSAL.md §1; input md5s
re-verified against `1bd57a84`: Biopower `722602a7…`, Cap `18f726ab…`). Pre-registration:
PROPOSAL.md committed `fd58d3a1` BEFORE any scoring; packet `6fc1ef9a` (sha256
`3d247582…`, 37 entries, seed 4289324239); blinded calls `0a28d7ca` committed BEFORE
mapping `e4c293d4` (blind-order evidence in git history). Full compute output below is
from `python3 python/audits/oq264_idiom_share.py compute --holdout holdout_expected.json`
(deterministic; re-runnable from the committed artifacts). Zero API calls were made.

## CORRECTION BLOCK (2026-08-06, same day, post-checkpoint operator review; registered in PROPOSAL_ADDENDUM.md, computed in DENOM_SENSITIVITY.txt — the original text below is preserved, not rewritten)

1. **The headline as first written was understated; the corrected headline is:**
   *range 0.25; PASS under the rev-2 (recalibrated) modifier, INDETERMINATE under the
   plan's rev-1 modifier; the modifier was recalibrated in Phase A on simulation grounds
   (CALIBRATION.txt), before any scoring and for data-independent reasons — but the
   observed value landed exactly on the boundary the recalibration made passable.* Both
   rule-verdicts are part of any citation of this result.
2. **The 0.25 is a denominator artifact, not share variance.** TAG = 3, 3, 4 over
   D = 6, 4, 6: the max-pairwise Δ (0.500 → 0.750 — the ENTIRE range) falls between two
   draws with IDENTICAL numerator (TAG = 3). The whole measured spread is produced by
   the unit population churning at fixed judgment. The pooled observable did not escape
   the per-reading churn floor — its denominator (kernel readings ∪ selected axes) is
   built from per-reading identity, so it inherited the churn with a sign flip: a draw
   that mints FEWER readings scores as MORE tag-idiomatic. Under a fixed baseline D=6
   (exploratory, NON-GATING — DENOM_SENSITIVITY.txt), the range drops to 0.167, the
   max-pair moves to base↔r2, and the spread becomes judgment-driven (the one TAG
   move). The denominator convention alone moves the "measured floor" by 0.083 (a third
   of the committed range) and changes which pair carries it. This is a SPECIFICATION
   question that precedes any k-sizing question; more draws of the same observable
   measure the confounded quantity more precisely. (Selected-axes-only convention is
   n/a for Biopower under the committed unit typing — kernel-linked axes are typed as
   kernel readings; stated, not hidden.)
3. **6/6 duplicate agreement is a bound, not zero.** One-sided 95% binomial upper bound
   on the per-item disagreement rate = 1 − 0.05^(1/6) ≈ **0.393**. The clause that let
   the sens-1 PASS stand ("measured-zero scorer variance") rests on an interval
   consistent with an error rate up to ~39%, any of which suffices at sensitivity 1 to
   have produced the pass. Everywhere this report says "measured scorer variance zero,"
   read "no disagreement observed in 6 pairs; 95% upper bound 0.393."
4. **Cap concordance check (registered drift probe):** no consistent cross-file
   direction on any observable (share directions even oppose: Biopower +/+, Cap −/na) —
   the cross-day drift residue is UNSUPPORTED by the cheapest available check (not
   excluded; weak check at n=3 per file).
5. **Consequence for the gate meaning:** "candidate instrument" is withdrawn as the
   operative reading. What Phase B measured is closer to: *share as specified is
   unstable, and it is unstable because of the denominator, at fixed judgment.* The
   committed verdict PASS(sens1) stands as the pre-registered gate's output (rules were
   followed; nothing is re-gated), but the instrument question it was built to answer
   is superseded by the specification finding above.

## Headline (at its scoped altitude) — SUPERSEDED BY CORRECTION BLOCK ITEMS 1–2 AND 5

**FINAL GATE VERDICT: PASS(sens1) — PROVISIONAL at k=3 by the k-monotonicity clause.**
The pooled idiom share on the Biopower triple spans 0.500–0.750 (range = 0.25, exactly
the top of the PASS band), with measured scorer variance ZERO (duplicates 6/6 exact) and
the component rule satisfied. The pass asserts SHARE-stability only: the unit population
itself churned 33% at fixed input (D = 6→4→6), so the manifests are NOT stable — the
ratio is what survives. A pass at the band boundary with sensitivity 1 stands only
because measured scorer variance is zero (recalibrated §5 rule) and is one class-flip
from INDET; k=3 cannot do better on this lattice.

## Controls (all pre-registered; all passed)

- **Planted judged control (HALT):** plant-card scored `card` (NON-TAG ✓), plant-tag
  scored `tag` (TAG ✓) — no HALT.
- **Mechanical denominator control:** formula yields 6/4/6 and 6/4/3, matching the
  plan's predictions AND SCORING.md's baseline denominators — ALL PASS, no fix path
  (CALIBRATION.txt).
- **Duplicate scorer-variance instrument:** 6/6 exact, 6/6 TAG-side. Caveat: duplicates
  are byte-identical and were recognized as repeats (declared in calls.json), so this
  measures within-pass call stability, not scorer independence.
- **Holdout reliability (contaminated, secondary):** 6/6 exact vs SCORING.md — an upper
  bound (executor read SCORING.md this session; PROPOSAL §3).
- **Consistency check with the committed record:** blinded baseline TAG counts
  reproduce ADDENDUM §3's pinned values exactly (Bio 3/6, Cap 2/6) without the mapping
  being available at call time.

## Component ranges (reported separately, per the component rule)

| Quantity | Biopower triple | Note |
|---|---|---|
| shares | 0.500, 0.750, 0.667 | range **= 0.25** (PASS band boundary) |
| TAG counts | 3, 3, 4 | range 1 |
| D | 6, 4, 6 | range 2 — a 33% unit-population swing at fixed input |
| pooled share | 0.625 | component residuals 0.75 / 0.50 / 0.25 (max allowed 1.0) ✓ |
| sensitivity | 1 | one class flip changes the raw band; stands via clean duplicates |

Directional observation (k=3, not a claim): both 08-05 redraws sit above the 08-03
baseline (0.750, 0.667 vs 0.500). At k=3 noise and day/build drift are
indistinguishable; see confounds below.

## Variance attribution (the decomposition the instruments exist for)

- **Scorer:** measured ~0 (duplicates 6/6; holdout 6/6 as a contaminated upper bound).
- **Generator:** the mechanical comparators churn at or above the share range with no
  judgment involved — Biopower D range 2 (relative 2/6 ≈ 0.33 ≥ share range 0.25),
  selected-axes range 2, deferred-axes range 1. **Variance localizes to the GENERATOR;
  the judged layer adds ≈ nothing detectable at this n.**
- Consequence: the share observable's k=3 floor (range ≈ 0.25 on this lattice) is a
  property of decompose redraw churn, not of the scoring rubric.

## Cap K triple — churn-extreme contrast (feeds no gate)

- **capk/r2 is the categorical outcome KERNEL-MINTING CHURN:** no contested kernel
  minted at fixed input (`commitment_system_recognition` absent); its fallback
  population (3 selected axes) has share 1.000 — shown as contrast only, excluded from
  every range per the pre-registered zero-kernel rule. The fallback population is
  visibly a DIFFERENT observable: all three axes are block-name-shaped
  (tag-echoing), which is why pooling it would have manufactured a fake 0.75 range.
- Non-zero-kernel Cap pair: shares 0.333, 0.250 (range 0.083, n=2, contrast only).
- Mechanical comparators: kernel-readings range 5 (5→4→0), selected-axes range 3,
  contested-kernel FLIPS — the churn-extreme label from ARM0_HALT_REPORT.md is
  reproduced at every mechanical observable.

## Declared confounds (carried, not resolved)

- **Drift vs model version:** all six manifests share `scope_model: claude-sonnet-5`,
  prompt commit `d179423d`, schema commit `43ee9613`; no finer build string exists in
  any manifest or log (checked; the alias is the only identifier, 16 log occurrences).
  A silent server-side build change between 08-03 and 08-05 is not excludable from
  local records — scoped residue on the directional observation above.
- **Leak:** the blind covered the redraws; baseline re-scores are contaminated
  (ADDENDUM §3 counts + SCORING.md calls read in-session) and labeled as such
  throughout. File identity is unmaskable by construction; the blind covers draw
  identity; executor = scorer (single-agent session).
- **Floors, not estimates:** k=3 bounds variance from below (the OQ-259 §4 caveat
  applies unchanged); a share stable across 3 draws may churn at higher k — hence
  PROVISIONAL, with the pre-registered retraction path (Phase C reports full-k range
  AND mean over all 3-draw subsets).

## Gate consequence (per PROPOSAL §6) and OPERATOR CHECKPOINT

PASS band → **the pooled idiom share is a candidate instrument** (share-stability only;
provisional in k; judged observable with measured-zero scorer variance at this n).
Phase C (spend) sizes k against the observed range — it runs ONLY on operator go.

**Decisions at this checkpoint (operator's seat):**

1. **Phase C go / no-go**, and if go: spend ceiling; **k = 3 vs 5** additional Biopower
   draws (~103K tok/call: k=3 ≈ 310K input tok, k=5 ≈ 515K); the new draws pool with
   the 3 free ones (k=6–8 anchor) under a fresh packet + duplicates.
2. **AT Fiat inclusion** (~34K tok/call): purpose pre-registered as reproduce-rate
   measurement ONLY (closes the ruling's "no Arm-0 measurement" rider; baseline reading
   set pinned by ADDENDUM §4 subject+stance method); its D is likely 3–4 — coarser
   lattice, does NOT feed the share gate.
3. Or **standard-only closure**: resolve OQ-264 on the free-data result alone (k=3
   floor ≈ 0.25 share range over 33% unit churn; per-reading identity remains
   non-citable from single draws) — weaker instrument validation, zero spend.

No Phase-C run and no ISSUES.md closure until this ruling. (Phase D propagation —
Amendment-5 qualifier, OQ-259 items 2–3 re-scope, CLAUDE.md churn block — follows the
ruling per the plan.)
