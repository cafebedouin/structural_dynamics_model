# OQ-258 referent discriminator — WRITEUP (executed 2026-08-03 → 2026-08-04)

**Pre-registration:** `PROPOSAL.md`, committed BEFORE any generation at `74e74e35`
(item set, arms, regimes, Δ=0.15, interpretation table, witnesses — all pinned there).
**Contract fix commit (Phase 2):** `685ed7cf`.

## Verdict (pinned interpretation table, row 3)

**Reader-position variance survives its first real test. Referent ambiguity did NOT own the
2026-07-27 channel-legibility finding.** The channel-conditional reliability caveat
(KNOWN_STATE 2026-07-27) HARDENS.

| Quantity | Value |
|---|---|
| n paired items | 15/18 (3 declared drops, below) |
| Baseline mean spread (18 items) | 0.5933 |
| **Arm B (old contract, redraw null)** | **0.4633** |
| **Arm A (referent-fixed contract)** | **0.5167** |
| delta (B − A) | **−0.0533** (A *wider* than B) |
| Wilcoxon two-sided | W=22.0, p=0.328 |
| Pinned boundaries | Δ=0.15; B-elevated ≥0.38; A≈B quantum 0.05 |

Row evaluation in pinned order: row 1 (owned) requires B−A ≥ 0.15 with p<.05 — fails (delta
is negative). Row 2 (regression dominates) requires mean(B) < 0.38 — fails (0.4633: the
phenomenon REPLICATES on redraw, ~77% of the top-spread-selected baseline elevation retained
against a regression-to-mean expectation). Row 3 fires: |B−A| — p=0.328 ≥ 0.05 — A ≈ B with
B elevated.

Secondary statistic (descriptive): within-18 stratum means — armB tacit 0.4625 /
none_apparent 0.4643; armA tacit 0.4937 / none_apparent 0.5429 (baseline, inverted by
selection: 0.632 / 0.545). No referent-work signature (the fix should have compressed
none_apparent at least as much as tacit; instead both rose slightly under A).

## The named single-item witness FAILED — informatively

Pinned witness: haiku on `animal_status__abolitionist_reading` under referent (b) should
author ε ≥ 0.5 (baseline 0.00). **Result: haiku authored ε = 0.00 in BOTH arms.** Its Arm A
`logic_rationale` shows the instruction was processed and overridden, not missed:

> "Extractiveness is zero in this reading because animals are assigned rights-bearing status
> under which NO instrumental use is permissible. The question is not 'how much extraction is
> happening' but 'is the use itself legitimate'—and the reading's answer is categorically no."

And kimi FLIPPED INTO the zero camp under the fix (B: 0.82 → A: 0.02), rationale:

> "Extractiveness is near-zero (0.02) because the constraint itself blocks rather than
> extracts; suppression is low (0.15) because the constraint's force is presented as normative
> recognition rather than coercion..."

Per-leg on the flagship item — B: haiku 0.00 / flash 0.95 / kimi 0.82 / sonnet 0.91
(spread 0.95); A: haiku 0.00 / flash 1.00 / kimi 0.02 / sonnet 0.91 (spread **1.00**).
The fix *widened* the flagship divergence.

**Mechanism diagnosis (kimi specimen).** Kimi's Arm A referent is neither (a) endorsed nor
(b) contested arrangement: it scored **the reading's normative principle itself as "the
constraint"** ("the constraint itself blocks rather than extracts"). The live ambiguity the
2026-07-27 spread rides is upstream of the (a)/(b) choice OQ-258 posited: **what counts as
"the constraint" in a kernel-reading story** — the standing social arrangement, or the
reading-as-constraint (the moral/normative claim the reading asserts). The Phase-2 paragraph
fixes (a)-vs-(b) and leaves constraint-identity open; authors route around the fix through
that opening. This is why the one-paragraph contract fix cannot remove the artifact — and it
makes the OQ-258 typed fallback (a DECLARED per-story referent field, compared
within-declaration) the natural next instrument, now with a third enum value needed:
`standing_arrangement | endorsed_alternative | reading_as_constraint`.

[EDGE] The ruling's own clause "assessed by the reading's own lights" is part of the leak:
under the abolitionist's lights the contested arrangement supports both "maximal extraction"
(flash 1.00) and "extraction is a category error" (haiku 0.00) — fixing the referent while
keeping assessment reading-relative (the OQ-26-compatible half of the ruling) leaves the
scale itself seat-relative. Referent-fixing under reading-indexed assessment is structurally
insufficient to force cross-author agreement; that is a finding about the framework, not a
defect of the prompt paragraph.

## Witness chain

- **Positive control (measurement):** `measure.py` re-extraction reproduces
  `items_baseline.json` exactly (18 items × 4 legs) — PASS, printed before any arm read;
  `items_baseline.json` itself exact-matched the recorded 2026-07-27 `top_divergers` for
  14/18 items (`pin_items.py` output; 4 items absent from the results JSON's top-25 window,
  flagged `reextracted_only`).
- **Stamp witness:** `stamp_witness_census.txt` — all 70 Arm B stories stamp `prompt_commit`
  `8080348c` (pre-fix), all 71 Arm A stories stamp `685ed7cf` (post-fix); VIOLATIONS: none.
- **Baseline legs untouched:** `baseline_leg_fingerprints_before.txt` ==
  `baseline_leg_fingerprints_after.txt` (md5 per leg, diff empty) across the whole audit.
- **Per-item table:** `measurement_results.json` (`per_item`).
- **Post-archive re-run:** after evidence moved under `generated/`, `measure.py` (with the
  archive-aware `leg_dir`) reproduced identical numbers (control PASS, same means/p/verdict).

## Declared drops (pairwise-complete rule, PROPOSAL)

Three items each failed ≥9 consecutive draws in ONE leg with the same validation-failure
class (missing required `stakeholders`, occasional enum/JSON-parse variants):

| Item | Leg | Arm |
|---|---|---|
| `press_reformation_causation__technological_determinism` | haiku | B |
| `hebrew_linguistic_life__liturgical_preservation_reading` | flash | B |
| `competence_occupation__real_incident_necessity` | haiku | A |

Per-run `failures_*.json` / `rejections_*.json` copies in this dir (failure-budget rule).
Note the failures are item×leg-systematic, not uniform noise — same item recovers instantly
on other legs.

## Deviations from plan (all declared at decision time)

1. **Kimi transport:** the Arm B Moonshot batch stalled at 0/18 for ~8h
   (`gen_armb_kimi_batch_stalled.log`); switched to the PROPOSAL's declared `--sync`
   fallback (identical sampling params; k2.6 reasoning ~16.5k output tok/story matches the
   baseline thinking-ON regime). Arm A kimi ran sync directly.
2. **Wrapper rebinds four constants, not three** (adds `OUT_DIR`) so failure logs stay
   arm-scoped.
3. **Straggler re-invocations:** each failing leg got up to 2 extra wrapper invocations
   (3 attempts each, ladder-idempotent) beyond the single-run 3-attempt budget before a drop
   was declared at 9 draws.
4. **Within-18 stratum inversion** (tacit 0.632 > none_apparent 0.545) declared in PROPOSAL
   before spend; secondary statistic registered against it.

## Spend (token_acc, all runs summed per leg)

haiku ~$1.66 (batch); flash ~$1.10 (interactive-rate estimate lines; runs used batch+cache);
sonnet ~$1.79 (batch intro); kimi ~1.08M in / ~0.59M out tokens (sync, includes mandatory
reasoning; no $ rate line in driver). Total well under $10 for 141 stories.

## What closes, what opens

- **OQ-258 RESOLVES** with the discriminator's answer: referent ambiguity does not own the
  channel-legibility finding; the contract fix (685ed7cf) stands regardless (it fixes the
  rebuild's contract; both witnessed referent-(a)-style zeros persist through it).
- **The channel-conditional reliability caveat hardens** into a durable framework note:
  observer-axis quantities downstream of ε (H¹, type, orbit) remain channel-conditional on
  tacit/referent-weak constraints, and a one-paragraph contract fix does not remove it.
- **Follow-up minted (OQ-263):** constraint-identity ambiguity — the declared-referent-field
  fallback, now three-valued (`standing_arrangement | endorsed_alternative |
  reading_as_constraint`), with the kimi flip + haiku category-refusal as its founding
  specimens.
