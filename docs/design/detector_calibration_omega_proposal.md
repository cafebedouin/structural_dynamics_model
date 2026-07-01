# Proposal (DRAFT — awaiting operator ruling): a `detector_calibration` omega

**Status:** draft for ruling. Nothing is wired or fired. Firing thresholds and the type
assignment are a declared seat (the operator's), not something the engine self-resolves.
Provenance: Elias-Thorne report review (2026-06-30) → web-Claude's "author the omega aimed at
the apparatus itself" → the apparatus-first recon in §6. R2 and R3 are ruled settled (operator,
2026-06-30); R1 and R4 remain open.

**Standing convention for this doc (adopt for every empirical claim below):** any claim about the
author×engine cross-tab must state, *in the same sentence*, which axis its sensitivity check varied
**and which it did not**. Four aggregate claims in this thread shipped on a check that skipped the
load-bearing axis (caught only by going back to substrate); this convention makes the untested axis
visible at claim time.

---

## 1. The question this makes askable — and its honest motivation

The reports carry four `AUTHORED RESOLUTION PROTOCOL` omegas per constraint
(`irreversibility_threshold`, `benchmark_detection_lag`, …), each a question the engine cannot
settle, named and handed back. None is aimed at the **apparatus**. This one is:

> Does the engine's extraction call track real (hidden) extraction better than chance, and what
> false-positive rate is acceptable for acting on a consequential extraction verdict?

**Motivation (corrected — do not restore the earlier framing).** The premise is NOT "the detector
over-calls / escalates." That directional claim was made and **retracted** (see Corrections log).
The honest premise is narrower and is exactly what Ω_E exists for:

> Engine and authors disagree at **77% of both-speak seats** (91/396 agree). The *direction* of that
> disagreement is **not** determinable from the cross-tab — it is ~symmetric under the engine's own
> extractive/functional axis (escalate 69 vs de-escalate ~67–75; *sensitivity varied the tangled_rope
> and naturalized tier placements, which is where direction actually moves — not scaffold, which does
> not move it*). Disagreement is real and large; which side is closer to truth is unknown and
> externally-witnessable-in-principle. That gap is the omega.

## 2. Typing — a PAIR, not one omega (R2, ruled: keep the pair)

Conflating the two is the "when to stop verifying" trap (`docs/omega_variables.md`): an Ω_P mistyped
as Ω_E lets the apparatus self-certify by fiat; an Ω_E mistyped as Ω_P invents a value-dispute where
a measurement would settle it.

- **Ω_E (empirical, `awaits-external-input`).** *Hit rate.* "Does the extraction detector beat chance
  on hidden extraction?" Resolution = an external validation study against labeled ground truth. The
  corpus has no such labels (authored `claimed_type` is a seat, not truth — seat theorem), so this is
  genuinely deferred, not computable now.
- **Ω_P (preference, value-decision).** *Acceptable FP rate.* "What false-positive rate is tolerable
  when acting on an extraction verdict, given the cost of a wrong call?" Resolution = a ruling by
  those bearing the cost. Already authored locally: `institutional_barrier_structure.pl` →
  `false_negative_vs_false_positive_tradeoff, preference`.

## 3. The firing condition — THREE typed transition classes, one consequence gate (R1)

The engine cannot *close* either limb (Slice C category error). It CAN detect the precondition and
**mint OPEN, with provenance**. All inputs exist per-constraint (verified 2026-06-30). Firing =
`(FC1 ∨ FC2) ∧ consequence-gate ∧ ¬already-covered` (the guard is §5/R4).

The three ways a both-speak seat can disagree, and how each is handled:

- **FC1 — boundary escalation.** author ∈ genuinely-functional `{rope, scaffold, mountain}`, engine ∈
  extractive `{snare, tangled_rope}`. The engine calls extraction where the author saw legitimate
  coordination. **Fires.**
- **FC2 — severity-within-extraction** (the largest single bucket, ~⅓ of the off-diagonal:
  `tangled_rope↔snare`). Both sides are extractive, so a boundary axis drops this on the floor as
  "lateral" — but author=`tangled_rope` (contested/hedged extraction) vs engine=`snare` (clean,
  confirmed extraction) is a real and arguably *sharper* calibration disagreement than boundary-
  crossing: is the extraction contested or settled? **Fires as its own condition** — not folded into
  FC1, not silently excluded as lateral.
- **naturalized-landing** (engine=`naturalized`). Does **NOT** fire here. `naturalized` is
  extractive-by-construction (`drl_core.pl:422-426`: `BaseEps > rope_epsilon_ceiling` compressed below
  the χ floor — "looks like an invariant from a below-threshold, no-exit seat"), so report_generator's
  extractive/functional binary (authored for `extraction_blindness`, where naturalized-as-appearance
  IS the signal) must not be reused verbatim to call →naturalized "de-escalation-toward-safe." It is
  already the domain of `extraction_blindness` + the false_summit signature (`dr_claim_mismatch/4`
  Type 1). **Routed there, not double-handled here** (R4 guard).

**Consequence gate** (keeps it from firing on every mismatch — narrowness is R1): the seat's
constraint must be hard-to-externally-check *and* consequential — `theater_ratio ≥ floor` (cover-story
machinery) ∨ coupling-masked (`coupling.boltzmann = non_compliant`) ∨ an authored no-exit (`trapped`)
victim bears the call.

Draft clause (illustrative; the `{}` type-sets and `TheaterFloor` are R1, not settled here):

```prolog
% MINTS an OPEN omega; never closes it. Names a boundary; does not rule on its own correctness.
detector_calibration_due(C, Class) :-
    both_speak_disagreement(C, Seat, AuthorT, EngineT),
    (   fc1_boundary_escalation(AuthorT, EngineT), Class = boundary
    ;   fc2_within_extraction(AuthorT, EngineT),   Class = severity     % tangled_rope <-> snare
    ),
    consequence_gate(C, Seat),
    \+ already_covered_by_false_summit(C, Seat).       % R4 three-branch guard
```

Resolution stays **authored** (the protocol text closing each limb), exactly like the existing four.
The engine supplies only the firing + the typed slots.

## 4. Why this is honest (the boundary it respects)

- It does **not** decide whether any extraction call is correct (Slice C / seat theorem).
- It does **not** read the Slice-A cross-tab as calibration: `divergence_rate` (0.77) is a two-*seat*
  disagreement rate, never a hit rate. Slice A *informs* the omega (where engine/authors diverge); it
  cannot resolve it.
- The only blind spot is upstream of Prolog: whether the omega was authored at all. This authors it.

## 5. Rulings

- **R1 — firing (OPEN).** The three-class structure above (FC1 boundary, FC2 within-extraction split
  out, naturalized routed away) + the consequence gate. Open sub-decisions: exact type-sets, the
  `TheaterFloor` value, and whether FC2 fires both directions (`tangled_rope→snare` and
  `snare→tangled_rope`) or only the "hardening" direction.
- **R2 — typing (RULED: keep the pair).** Ω_E hit-rate + Ω_P acceptable-FP-rate; do not collapse.
- **R3 — scope (RULED: narrow, and enforce in code not intent).** Ship as a **single named minter
  predicate** (`mint_detector_calibration_omega/N`) that is the *only* caller of the assert path — NOT
  a general `engine_mint_omega/…` facility with one current caller. The R3 boundary must be something
  the next agent hits **structurally** (there is no general minter to reuse), not a comment they have
  to remember was intended.
- **R4 — mint + wire (OPEN, held for R1).** Wire `detector_calibration_due/2` into the pipeline +
  reports and mint the OQ in ISSUES.md (Priority/Deps). **Three-branch overlap guard** — before
  minting, verify the transition is not already covered by: (1) `extraction_blindness` (cross-seat
  cover-story), (2) the naturalized/false_summit routing (FC-naturalized above), and (3) — if FC2
  ships — that the `tangled_rope↔snare` firing does not itself re-collide with (1) or (2). One branch
  per already-existing omega axis, so no gap is minted twice under two names.

## 6. Apparatus-first recon (what already exists — so this extends, not duplicates)

- **Computable agreement already runs:** per-seat `routing_sink:route_address/5` (MECE 7-address
  taxonomy; `no_route`=agree, `author_engine_divergence`=both-speak-disagree) + the Slice-A
  `author_engine_crosstab` in `routing_sink.json`.
- **`oracle_gap_analysis.py`** measures intra-orbit predictability — adjacent, NOT detector-vs-truth;
  do not cite it as calibration.
- **Authored detector-doubt already exists story-locally:** `press_reformation_causality.pl` authors
  `'…if technological_determinism is correct, this constraint mis-classifies; the snare classification
  should be weakened…'`. This **generalizes that convention** into a typed, apparatus-directed omega —
  not a new subsystem.
- Taxonomy already carries calibration-shaped examples: `Ω_P03 (acceptable FP rate for screening)`,
  `Misclassification cost (Ω_E)`.

---

## Corrections log (kept so the walked-back premises don't get re-imported)

1. **"Engine escalates 2.3–3.1×" — RETRACTED (2026-06-30).** Sensitivity varied `scaffold` (does not
   move direction) not `tangled_rope` (does). Under the engine's own extractive/functional axis the
   split is ~symmetric (69 vs 67–75). Direction is not a robust fact; 77% disagreement is.
2. **"Reuse report_generator's extractive/functional binary as the firing axis" — CORRECTED.** That
   binary was authored for `extraction_blindness` cover-story detection; reusing it verbatim mis-buckets
   `naturalized` (extractive-by-construction) as the safe direction. Firing now handles naturalized
   explicitly (§3) and routes it to the false-summit apparatus.
3. **"Fold `tangled_rope↔snare` into lateral/ignore" — CORRECTED.** It is the largest bucket and a
   real within-extraction severity disagreement; split out as FC2 (§3).
