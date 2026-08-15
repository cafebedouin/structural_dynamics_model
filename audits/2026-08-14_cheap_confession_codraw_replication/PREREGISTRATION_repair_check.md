# PREREGISTRATION — v4 closing repair-check

**Written:** 2026-08-14, before the run. Co-draws in flight; neither result seen.
**Question:** v4 rewrote the closing from an asserted natural limit to "a dated empirical
claim with a falsifier attached." **Did the repair change the structure, or only the label?**

## The trap, declared first

The `false_natural_law` signature keys on an explicit mountain claim plus Boltzmann
non-compliance plus excess extraction. v4's closing states in plain text that the claim is
*not* a fact about nature. A generator reading that sentence will not author a mountain
claim. **Therefore `false_natural_law` cannot fire, and its not firing is uninformative by
construction.**

Pre-committed: **a "no false_natural_law" result may NOT be reported as the repair
succeeding.** That reading would be selecting the comfortable falsifier from a menu — the
essay's own ¶89, applied to its author. Only the metric comparison below carries a verdict.

## Baseline — the v2-authored story, pinned

`omega_production_cost_asymmetry` (from v2's "What survives"):

| field | value |
|---|---|
| base_extractiveness (ε) | 0.61 |
| suppression_requirement | 0.42 |
| theater_ratio | 0.58 |
| coupling score | 0.75 |
| signature | false_natural_law (high) |
| claimed_type | mountain |

## The test

Generate a constraint story from **v4's "What survives" section** and compare its authored
`base_extractiveness` / `suppression_requirement` / `theater_ratio` / coupling against the
baseline.

- **LEXICAL repair** — claimed_type moves off mountain, but the metrics sit *inside* the
  same-input churn interval (below). The essay changed what it claims, not what it commits
  to: same structure, better manners.
- **STRUCTURAL repair** — metrics move *outside* the churn interval, in the direction of
  lower extraction/enforcement. The hedge reflects a changed commitment.
- **FAILED repair** — `false_natural_law` fires anyway. Strongest result, least likely.

## The churn interval — supplied by the other experiment, not guessed

I have no noise estimate for this generator on this input, so **any numeric threshold I
picked now would be invented.** Instead: the three co-draws (`codraw_01/02/03`) regenerate
`omega_production_cost_asymmetry` from the same frozen manifest. Their observed
min–max range on each field **is** the same-input churn floor for this exact story.

**Pre-committed rule:** a field counts as moved iff it falls outside the co-draw
min–max range for that field. Committed before either result is visible. If the co-draw
range for a field is degenerate (all three identical), widen to ±0.02 — the authored-grid
quantum — rather than treating an exact tie as zero-width.

**If ≥2 of the 3 metric fields move outside range in the low-extraction direction ⇒
STRUCTURAL. If ≤1 ⇒ LEXICAL.** Mixed-direction movement ⇒ report as INDETERMINATE, not
as either limb.

## Named input differences — this is not a clean A/B

Declared, not discovered afterward:

1. **Input scope.** The baseline story was authored from the *whole* v2 essay as one axis
   among four; this run sees only the closing section (~1.6 KB). OQ-264 found redraw
   stability is file-structure-dependent, so scope difference is a live confound for the
   metric comparison. Chosen deliberately: the dominant threat is §6 of v4 telling the
   generator that this exact claim was previously flagged, and section-only removes it.
2. **No research grounding** (`--skip-search`); the baseline run had it.
3. **n=1.** This is one draw. It cannot distinguish "unchanged" from "moved and moved back."
   A LEXICAL verdict here is therefore an *observation*, not a replication — same standard
   applied to the co-draws, applied here.

## What this does not test

Whether the closing is *true*. The engine reads authored structure, not the world. A
structural repair means the essay's commitments changed, not that the abiding cost is in
fact moving — that remains the falsifier v4 attached, and it is longitudinal.
