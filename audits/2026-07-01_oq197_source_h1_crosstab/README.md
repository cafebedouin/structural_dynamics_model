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

## Twin extension (2026-07-01) — the finding REPLICATES at scale

Run on both twins via `corpus_path` overlay (`asserta`, per Corpus Loading; 960 constraints
each, twin-specific IDs confirm the overlay took effect, not the default `testsets`). `h1`
computed in Prolog (`grothendieck_cohomology:cohomological_obstruction/3` after
`corpus_cohomology/1`) — **positive-controlled against the pipeline `h1_band` on testsets: 0
mismatches / 119** (`h1_testsets_prolog_control.tsv`), so the probe is validated before the
twins where no pipeline `h1_band` exists. Full output: `twin_crosstab_output.txt`.

| corpus          | eligible | (b)↔h1>0 off-diagonal | (a)↔h1>0 off-diagonal |
|-----------------|----------|-----------------------|-----------------------|
| testsets        | 84       | **0**                 | 3                     |
| testsets_haiku  | 452      | **0**                 | 19                    |
| testsets_flash  | 661      | **0**                 | 14                    |
| **total**       | **1197** | **0**                 | **36**                |

- **Canonical (b) ≡ `h1_band>0` on ALL THREE corpora — 0 off-diagonal across 1,197 eligible
  constraints, zero exceptions.** The (b)=h1_band coextension is not a testsets small-n
  artifact; it is definitional and holds at scale.
- **Stakeholder (a) is consistently distinct from `h1_band`** — 36 divergences (~3.6% / 4.2% /
  2.1%), small but nonzero on every corpus. Since (b)≡h1 exactly, the (a)/(b) disagreements ARE
  the (a)/h1 divergences (19 and 14 on the twins). (a)'s independent authored-stakeholder
  signal generalizes across three independently-generated corpora.

## Reading (hardened) — for the ruling, not the ruling

The evidence for **(a)** is now robust: (b) is a pure duplicate of `h1_band` (0/1197 exceptions
→ "duplicate = cruft" per build-discipline), while (a) is a non-redundant reading distinct from
`h1_band` on 36/1197 constraints across three corpora. The (a)/(b) ruling remains the operator's
seat — now hardened past the testsets small-n.

## Caveats

- The perfect (b)⟺h1 coincidence is definitional coextension (same canonical `dr_type` orbit,
  two readings), so it means "(b) is redundant," NOT "(b) is independently confirmed by h1."
- The both-determinate restriction drops ~half of each twin (452/960, 661/960) — the excluded
  constraints are exactly the present-but-insufficient / different-coverage population OQ-197 is
  about, correctly held out of the overlap signal per the operator instruction.
- (a)'s divergence rate is a minority (~2–4%); the claim is that (a) is *irreducible to*
  `h1_band`, not that it diverges often. Even a few percent of genuine independent signal makes
  (a) a distinct measurement — which is the question the ruling turns on.
