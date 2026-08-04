# Results — channel-legibility coding (per PREREG_legibility_coding.md)

**Date:** 2026-07-27 · **Coders:** 7 blind subagents (seed-pool substrate only; batches in
`coding_batches.json`) · **Codes:** `codes.json` (68/68 coded) · **Analysis:**
`coding_analysis.py` → `coding_analysis_output.txt` (the witness; all numbers below from it).

## Pre-registered outcomes

- **Control gate: PASS (7/8).** Sole miss: `feud_obligation_kernel__christianized_pacification`
  expected tacit, coded mixed (defensible — ecclesiastical doctrine is textual).
- **H1 SUPPORTED.** Channel × decile 2×2 (mixed/none excluded, per prereg):
  top-spread 10 tacit / 6 text_legible; bottom-spread 5 tacit / 16 text_legible.
  Fisher two-sided **p = 0.023**, in the predicted direction (top more tacit).
- **R1 (topic heat) FAILS.** hot_topic × decile: top 7/23, bottom 12/18, p = 0.27 — and the
  raw direction is *reversed* (the agreement decile is hotter). Topic heat does not explain
  the sort; several maximally-hot items (FISA 702, affirmative action, vaccine mandates,
  death penalty) sit in the *agreement* decile because their channels are statutory.
- statable_party=no proxy: directionally consistent (top 5 no / 10 yes; bottom 4 no / 18
  yes), weak.

## Post hoc observations (labeled as such; not pre-registered)

- **The largest asymmetry is `none_apparent`: 8 in top-spread vs 1 in bottom.** Items where
  the coder could discern *no burden channel at all* (quantum interpretations, Genesis
  hermeneutics, press–Reformation causation, the DR-ontology self-referential item)
  concentrate heavily in the maximal-disagreement decile. Read jointly with the Part-1
  addendum (haiku's authored-0.00 on `animal_status__abolitionist` = referent-scope
  disagreement, not a null): **author divergence concentrates where the ε referent is weak
  or the channel is tacit** — tacit+none = 18/30 top vs 6/30 bottom.
- Exploratory per-author deviations within the coded sample (n per stratum 9–22): 4-author
  spread by stratum — text_legible 0.168, mixed 0.254, tacit 0.427, none_apparent 0.488.
  kimi runs high on tacit items (+0.154 vs item mean); flash runs sharply low on
  referent-weak items (−0.259) — consistent with a low-default when nothing extractable is
  discernible.

## What this does and does not establish

- Establishes (at this sample size, single-coder): the four-model ε spread **sorts by
  channel legibility, not topic heat** — LLM-authored ε loses inter-author reliability
  precisely on tacit/referent-weak constraints. As an instrument note: per-story ε on such
  constraints should be treated as low-reliability; the `epsilon_provenance/5` Author atom
  (kimi/sonnet legs) already supports leg-sensitivity checks at read time.
- Does NOT establish the Claude-web inversion thesis (systematic *under*-authoring where
  harm is unstatable). Variance is visible without an external reference; bias is not. All
  four authors could still be jointly biased on tacit items in either direction — deciding
  that requires a non-LLM reference leg, which the operator declined (ruling 2026-07-27:
  no human ε leg; steps 2–3 of the reader-profile plan dead unless revived).
- Fragility: n=60 decile items, one coder per item, p = 0.023 — a couple of coding flips
  cross 0.05. The coders are themselves LLMs coding a structural property; the reader-class
  residue relocates there (declared, not closed).

## Post-audit correction of mechanism status (same day → OQ-258)

The channel SORT stands (channel beats topic heat), but its MECHANISM is degenerate between
reader-position variance and **ε-referent ambiguity in the generation contract** — the two
predict the same p on the same items, and the referent is hardest to fix exactly where no
burden channel is stateable (the `none_apparent` 8:1 asymmetry is what under-specification
predicts most directly). Recon confirmed the contract never fixes the referent (prompt: one
table line; schema: bare number). Witnessed instance + pre-specified discriminator
(fix referent → re-author decile items → remeasure spread): **ISSUES.md OQ-258**. Do not cite
this audit as evidence for the reader-profile mechanism until OQ-258 discriminates.

## Postscript — OQ-258 discriminator VERDICT (2026-08-04)

The pre-specified discriminator ran (`audits/2026-08-03_oq258_referent_discriminator/`,
pre-registered `74e74e35`, null + fix arms over the 18 top-spread tacit/none_apparent items
× 4 legs): **referent ambiguity did NOT own this finding.** Old-contract redraw (Arm B) mean
spread 0.4633 vs referent-fixed (Arm A) 0.5167 — A ≈ B (Wilcoxon p=0.328), B elevated
(≥0.38 pinned boundary), pinned row 3. **The reader-position/channel-conditional reliability
reading of this audit SURVIVES its first real test and the instrument note above hardens.**
This audit may now be cited for the channel-legibility sort without the OQ-258 degeneracy
caveat; the residual mechanism finding (constraint-identity ambiguity: authors can score the
reading-as-constraint itself, witnessed kimi 0.82→0.02 flip under the fix) is tracked as
OQ-263.
