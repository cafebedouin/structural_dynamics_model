# OQ-118 Limb 4 (Half-1) — frozen prediction: cast-field graded re-test

PRE-REGISTRATION, frozen before instrument build + run. A NEW test on the frozen draws,
NOT a retrofit of SIGMA_SEAT_PREDICTION.md (5f2a626c). No spend (re-analysis of existing 17 draws).

## Question
Do the draw-unstable cast fields (stakeholder multisets, beneficiaries/victims/
vindicated_propositions; all 0/6 under exact set-match) reflect FRESH casts (genuine generation
variance / seat-expression) or RENAMED-same casts (naming drift over a stable underlying
structure)? Exact set-match cannot separate these; a graded distance can.

## Substrate (frozen)
`audits/2026-06-12_cohort_zero/replicates/` (17 draws). Cast fields:
`stakeholders.{roster_card, role_multiset, power_multiset, time_horizon_multiset,
exit_options_multiset, spatial_scope_multiset}`, `base_properties.{beneficiaries, victims,
vindicated_propositions}`.

## Instrument to build
Field-level graded distance d_f(draw_i, draw_j) in [0,1] per cast field, applied within-story
(3 draws of a kernel) and between-story (across kernels). Graded = token-set overlap that scores
"renamed same cast" near and "fresh cast" far, NOT exact set equality. Pattern-5: split
positive-agreement from agreement-in-absence; all-empty = absence, never scored near.

## Positive control (MANDATORY, pinned BEFORE the real-draw run — the instrument is itself a claim)
- renamed-near: a pair that is the SAME cast with names permuted/paraphrased -> MUST score d < τ_near.
- fresh-far:    a pair with disjoint roles/powers                            -> MUST score d > τ_far.
- absence:      all-empty vs all-empty                                       -> agreement-in-absence, excluded.

Pin τ_near, τ_far before touching the real-draw verdicts. Control fails -> HALT; no real-draw read counts.

**Calibration anchor (REQUIRED — the control must match the drift it will judge, not a synthetic
maximum).** The renamed-near pair MUST be drawn from *observed* naming variation in these 17 draws —
a hand-checked real within-story pair that is visibly the-same-cast-renamed (e.g. a draw where
"central bank" became "the Fed" but the role/power structure is identical) — NOT an invented
permutation. Rationale: a synthetic permutation can be more aggressive or gentler than the model's
actual drift; calibrating τ_near against it sets the threshold where synthetic drift lives, not
where real drift lives, so the control PASSES while real renamed casts read as fresh (or vice
versa). This is the prose_presence lesson one level up: a control that fires on the easy/extreme
case can still be hollow on the case actually being measured. If no real within-story
same-cast-renamed pair can be found to anchor τ_near, that ABSENCE is itself a result (the drift is
not "renaming") — record it; do not fall back to a synthetic anchor.

## Prediction (discriminating)
- naming-drift hypothesis: within-story d < between-story d for cast fields (casts stable up to
  renaming; mirrors the story-level within ~0.37 < between ~0.59 already witnessed).
- fresh-cast hypothesis:   within-story d ≈ between-story d (cast redrawn each time; no separation).

## Outcomes (pre-committed)
- within < between beyond the control-calibrated margin -> casts are renaming-stable; the analysis
  contract becomes "canonicalize, then compare" (cast IS comparable up to canonicalization).
- within ≈ between -> casts genuinely redrawn; NOT cross-draw comparable; broad-A's floor stands.
- AMBIGUOUS (ranges overlap) -> report ambiguous, force NO verdict; n=6 / 3-draw is the ceiling —
  escalate for more draws if worth it.

**Resolution-ceiling note (honest framing of Limb 4's cost).** The cheap-re-analysis framing is
optimistic. The story-level metric ALREADY overlaps (within max 0.543 ≥ between min 0.500) where
the signal was strongest; field-level distances have fewer items per comparison and higher
variance, so overlap can only widen, not narrow. There is therefore a live possibility the
field-level metric returns AMBIGUOUS *by construction* at n=6 — i.e. Limb 4 may be pre-destined to
"escalate for more draws" before a byte of real verdict is read. Honest advertisement: Limb 4 is
"cheap re-analysis that will PROBABLY report it needs spend to resolve," not "cheap re-analysis
that likely resolves." The spend did not vanish; it moved from draws to more-draws — the same shape
as the "draws on disk" optimism this OQ already corrected once. Run the control round first; if the
control margin cannot be cleared by the n=6 field-level resolution, say so and stop — that
null-of-resolution is the result, not a failure to find one.

## Discipline
Mismatch is a finding about where the construction boundary sits, escalated — never an inline
redraw or a retrofit of this prediction. The control is written and pinned BEFORE the real-draw run.
