# Proposal (DRAFT — awaiting operator ruling): a `detector_calibration` omega

**Status:** draft for ruling. Nothing here is wired or fired. The thresholds and
the type assignment are a declared seat (the operator's), not something the engine
self-resolves. Provenance: arose from the Elias-Thorne report review (2026-06-30),
web-Claude's "author the omega aimed at the apparatus itself" suggestion, and the
apparatus-first recon recorded below.

---

## 1. The question this makes askable

The reports already carry four `AUTHORED RESOLUTION PROTOCOL` omegas per constraint
(`irreversibility_threshold`, `benchmark_detection_lag`, …) — each a question the
engine *cannot* settle, named and handed back. None of them is aimed at the
**apparatus**. The missing one:

> Does the engine's snare/rope call track real (hidden) extraction better than chance,
> and what false-positive rate is acceptable for acting on a consequential snare call?

This is the same *shape* of move the authors already make four times per constraint —
just pointed at the detector instead of at the constraint.

## 2. The typing — it is a PAIR, not one omega (the correction to the naive proposal)

Conflating these is exactly the "when to stop verifying" trap (`docs/omega_variables.md`):
an Ω_P mistyped as Ω_E lets the apparatus self-certify by fiat; an Ω_E mistyped as Ω_P
invents a value-dispute where a measurement would settle it.

- **Ω_E (empirical, `awaits-external-input`).** *Hit rate.* "Does the snare/rope detector
  beat chance on hidden extraction?" Resolution operation = an **external validation study**
  against labeled ground-truth extraction. The corpus carries no such labels (the authored
  `claimed_type` is a seat, not truth — seat theorem), so this is genuinely deferred, not
  computable now.
- **Ω_P (preference, value-decision).** *Acceptable false-positive rate.* "What FP rate is
  tolerable when acting on a snare call, given the cost of a wrong extraction verdict?"
  Resolution operation = a **ruling by those bearing the cost**. The corpus already authors
  this shape locally: `institutional_barrier_structure.pl` →
  `false_negative_vs_false_positive_tradeoff, preference`.

## 3. What Prolog CAN do now — the computable firing condition (Slice B core)

The engine cannot *close* either limb (Slice C category error). It CAN detect the
**precondition** — the constraints where a calibration miss would be load-bearing AND
the call is hard to externally check — and **mint the omega OPEN**, with provenance.
All inputs already exist per-constraint (verified 2026-06-30: `claimed_type`/computed
type, `theater_ratio`, `coupling.boltzmann`, authored `victims` with exit mode).

Draft clause (illustrative — thresholds are R1 below, NOT settled here):

```prolog
% MINTS an OPEN omega; never closes it. The engine names a boundary, it does not
% rule on its own correctness.
detector_calibration_due(C) :-
    dr_type_dominant(C, T), member(T, [snare, rope]),     % a consequential extractive/coord call
    (   theater_ratio(C, TR), TR >= TheaterFloor          % cover-story machinery (hidden), OR
    ;   coupling(C, _, boltzmann(non_compliant), _)       % coupling-masked extraction, OR
    ;   authored_victim_trapped(C)                          % a no-exit victim bears a wrong call
    ).
```

Resolution stays **authored**: the protocol text (what study closes the Ω_E, what
ruling closes the Ω_P) is written by a human/essay-process, exactly like the existing
four. The engine supplies only the firing + the typed slots.

## 4. Why this is honest (the boundary it respects)

- It does **not** decide whether any snare call is correct (Slice C / seat theorem).
- It does **not** read the Slice-A agreement cross-tab as calibration: that
  `divergence_rate` (0.77) is a two-*seat* disagreement rate, never a hit rate. Slice A
  *informs* this omega (it shows where engine and authors diverge, e.g. tangled_rope→snare);
  it cannot resolve it.
- The blind spot, if any, is upstream of Prolog: in whether this omega has been authored
  at all. This proposal authors it.

## 5. The rulings the operator owns (do not self-resolve)

- **R1 — firing threshold.** Snare/rope only? Keep the theater∨coupling∨no-exit disjunction,
  or narrow it? `TheaterFloor` value? (A too-loose gate mints OPEN omegas everywhere — the
  "scope creep / Omegas everywhere" Ω_C in `omega_variables.md`.)
  - *Empirical input (Slice A, 2026-06-30) — CORRECTED same day:* an earlier draft here
    claimed the seat-divergence is **directional** (engine escalates 2.3–3.1× more than it
    de-escalates). **That claim was under-witnessed and is retracted.** The direction is entirely
    an artifact of where `tangled_rope` sits on the extraction axis — the load-bearing placement,
    which the original sensitivity check (varying only `scaffold`) failed to test. Under the
    **engine's own declared gap taxonomy** (`report_generator.pl:243-248`: extractive =
    {snare, tangled_rope}; functional = {rope, naturalized, scaffold, mountain}), the split is
    **symmetric — escalate 69 vs de-escalate 75 (0.92:1)**; the 106-seat `tangled_rope→snare`
    bucket that carried the "escalation" story is *lateral* there (both extractive). So the only
    robust facts are the **77% seat-disagreement** and its concentration at the `tangled_rope↔snare`
    boundary — NOT a direction. **Implication for R1:** do NOT key firing on "escalation direction"
    (it depends on a contested banding). Key it on the engine's own extractive/functional axis +
    the hidden-extraction shape, which is a declared engine seat rather than an imposed one.

### R1-refinement — `naturalized` is not the "safe" direction (do not reuse report_generator's binary verbatim)

report_generator's extractive/functional binary was authored for `extraction_blindness`
(cover-story detection), where `naturalized` belongs on the *functional* side precisely
because "reads legitimate at the seat" is the disguise being detected. **That binary must
NOT be reused verbatim as detector_calibration's firing axis** (the "binary built for one
purpose, reused as if it settles another" trap). Reason, from the code: `naturalized`'s
defining condition is `BaseEps > rope_epsilon_ceiling` (`drl_core.pl:422-426`) — HIGH real
extraction compressed below the χ floor by power-scaling. It is the ONE "functional"-bucket
type that is **extractive by construction** (operator: "looks like an invariant from a
below-threshold, no-exit seat" — appears-invariant, is-extractive). So:

- For calibration, transitions *into* `naturalized` (`tangled_rope→naturalized`,
  `snare→naturalized`) are **not** de-escalation-toward-safe; naturalized is
  appears-functional / is-extractive, its own cell.
- But the engine **already** treats naturalized-masking specially: `extraction_blindness`
  (cross-seat) + the false_summit signature / `dr_claim_mismatch/4` Type 1. So the clean
  ruling is to **route naturalized-landing disagreements to that existing false-summit
  apparatus** (via the R4-guard below) rather than add a redundant third tier inside
  detector_calibration. (Reclassifying the ~8 →naturalized seats out of "de-escalation"
  does not change the symmetric-direction finding: 69 vs ~67.)

### R4-guard — avoid double-minting with `omega_extraction_blindness`

`omega_extraction_blindness` (`report_generator.pl:285`) already fires on 56 constraints, keyed
on a DIFFERENT axis: *intra-engine, cross-seat* — an extractive-typed seat at LOWER power beside a
functional-typed seat at HIGHER power (the cover-story structure). `detector_calibration`'s axis is
*engine-vs-author*. They are largely disjoint (partial overlap only), but they CAN co-fire on the
same constraint. **R4 must gate `detector_calibration_due/1` to NOT mint where `extraction_blindness`
already fired** — a structural guard that prevents double-minting on one gap under two names without
needing an exact overlap count.
- **R2 — the Ω_E/Ω_P split.** Confirm the pair, or rule it a single type.
- **R3 — engine-minted vs authoring-convention.** Should the engine **auto-mint** this OPEN
  omega (a new pattern: engine-minted omega, vs today's per-story authored `omega_variable/3`),
  or should it stay a **template authors add** when relevant? This is the "5-arity omega_variable
  protocol vs typed template clause" fork the report graduation lines already name.
- **R4 — minting + wiring.** Whether to mint the OQ in ISSUES.md (with Priority/Deps) and wire
  `detector_calibration_due/1` into the pipeline + reports, once R1–R3 are ruled.

## 6. Apparatus-first recon (what already exists — so this extends, not duplicates)

- **Computable agreement** already runs: per-seat `routing_sink:route_address/5` (the MECE
  7-address taxonomy; `no_route`=agree, `author_engine_divergence`=both-speak-disagree) and now
  the Slice-A `author_engine_crosstab` in `routing_sink.json`.
- **`oracle_gap_analysis.py`** measures intra-orbit predictability — adjacent but NOT
  detector-vs-truth; do not cite it as calibration.
- **Authored detector-doubt already exists story-locally:** `press_reformation_causality.pl`
  authors `'…if technological_determinism is correct, this constraint mis-classifies; the snare
  classification should be weakened…'`. This proposal **generalizes that existing convention**
  into a typed, apparatus-directed omega — it is not a new subsystem.
- Taxonomy already contains calibration-shaped examples: `Ω_P03 (acceptable FP rate for
  screening)`, `Misclassification cost (Ω_E)` — so the vocabulary is in place.
