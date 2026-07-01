# OQ-197 (a)/(b) source cross-tab vs h1_band — 2026-07-01

**Question (feeds the OQ-197 (a)/(b) ruling, does NOT make it):** if the gap detector's
`extraction_blindness` were re-sourced from authored stakeholder seats (ruling **(a)**,
`gap_seat_source(stakeholder)`) to canonical `dr_type/3` seats (ruling **(b)**,
`gap_seat_source(canonical)`), would (b) fire on the same constraints `h1_band` already
fires on — i.e. would (b) just be `h1_band` under a different name?

**Method.** `report_generator:gap_status/3` evaluated under **both** sources per constraint
on the live `testsets/` corpus (n=119), joined to `h1_band` from
`outputs/pipeline_output.json`. Restricted to the **both-sources-determinate** subset —
constraints whose `gap_status` is `gap` or `no_gap` under *both* sources (excluding anything
`undetermined` under either), so coverage-completeness differences between the two typing
sources do not contaminate the h1-overlap signal (operator instruction 2026-07-01). Eligible
n=84. Reproduce: `crosstab.py` over `gap_sources.tsv` (raw per-constraint status pairs) +
`pipeline_output.json`. Full output: `crosstab_output.txt`.

## Findings

**1. Canonical (b) firing is EXACTLY coextensive with `h1_band>0` on the eligible subset.**

```
CANONICAL (b) firing × h1_band
                   pos   zero   null
        fire        58      0      0
      no_gap         0     26      0
```

58/58 fire↔h1>0, 26/26 no_gap↔h1==0, zero off-diagonal. This is near-definitional, not
coincidental: `h1_band` counts disagreement over the signature-resolved `dr_type` orbit, and
canonical `gap_status` fires on ≥2 canonical `dr_type` seats disagreeing across power — the
same construction read two ways. **Re-sourcing the gap omega to canonical (ruling (b)) would
make it a redundant recomputation of `h1_band`.**

**2. Stakeholder (a) firing is genuinely DISTINCT from `h1_band` — 3/84 divergences.**

```
STAKEHOLDER (a) firing × h1_band
                   pos   zero   null
        fire        56      1      0
      no_gap         2     25      0
```

The 3 divergences (identical to the 3 (a)/(b) disagreements):
- `press_reformation_causation__strategic_deployment`: (a)=gap, h1=zero — (a) sees an
  authored-stakeholder gap the canonical orbit does not.
- `actinide_replenishment_mechanism_flat_control`: (a)=no_gap, h1=pos — canonical orbit
  disagrees where authored stakeholders agree.
- `radiative_levitation_stratification`: (a)=no_gap, h1=pos — same shape.

(a) measures **authored-stakeholder** disagreement; `h1_band`/(b) measure **canonical-orbit**
disagreement. On 81/84 they coincide, but the 3 divergences are exactly where authored
commitment structure and canonical typing come apart — the signal (a) carries that
`h1_band` cannot.

## Reading (for the ruling writeup — not the ruling)

Strong evidence toward **(a)**: (b) collapses into `h1_band` (would be a duplicate — the
build-discipline "unwired ≠ worthless / duplicate = cruft" test lands on *duplicate*), while
(a) is a non-redundant reading (authored-stakeholder axis, distinct from the canonical orbit
`h1_band` already covers). The (a)/(b) ruling remains the operator's seat.

## Caveats / next

- **`testsets/` only, n=84 eligible; 3 absolute divergences.** The operator's instruction
  named `testsets/` + the twins. The twins (`testsets_haiku/`, `testsets_flash/`) need their
  own pipeline run for `h1_band` + a `gap_status` both-source pass under a `corpus_path`
  overlay (asserta, per Corpus Loading) before the (a)-is-distinct finding generalizes. Not
  yet run.
- The perfect (b)⟺h1 coincidence is definitional coextension (same orbit, two readings), so
  it should be read as "(b) is redundant," NOT as "(b) is independently confirmed by h1."
