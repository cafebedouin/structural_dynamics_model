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

**Provenance of these numbers (necessarily POST-fix).** The first canonical run used a buggy (b)
clause (`constraint_classification/3` with an unbound context, mode `+Context`) that matched **0
seats for every constraint** — so canonical could never clear the ≥2-typeable-seats threshold,
read all-`undetermined`, and could register NO fires at all. A detector that always reads
`undetermined`/`no_gap` cannot produce 58/58 fire↔positive matches; the numbers below therefore
could only have been produced after the fix that re-sourced (b) to `dr_type/3`. The buggy run's
all-`undetermined` output is itself the witness that the bug existed (and that the fix changed it).

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
disagreement. On 81/84 they coincide; the 3 divergences are where the two sources' inputs come
apart — establishing that (a) is **not reducible to** `h1_band`. **Scope:** this shows
non-redundancy, NOT that (a) is *correct* on those 3. The stakeholder facts are single-pass
authored input (unverified by default, like d-values and type claims this thread treats as
provisional); whether a divergence is real signal or authoring noise is a separate open
question (**OQ-199**), not settled by the cross-tab.

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

- **Canonical (b) ≡ `h1_band>0` on all three corpora — 0 off-diagonal across 1,197 eligible
  constraints, zero exceptions.** This is *definitional* (both read the same `dr_type` orbit via
  `standard_context_for_power/2`), so it CONFIRMS the (b)-wiring is correct — it is **not**
  independent empirical replication of anything.
- **Stakeholder (a) distinct from `h1_band`** — 36 divergences (3 / 19 / 14). **Corpus-independence
  caveat (correcting an earlier overclaim):** `testsets_haiku` and `testsets_flash` are TWINS —
  the same seed material through different model backends, **correlated, not independent draws**
  (recorded prior: closer to shared-source than to two independent draws agreeing). So the honest
  reading is **one independent corpus** (testsets, n=84, 3 distinct) **+ one correlated pair**
  (haiku+flash, 19+14) — the twins' 19 and 14 may share authoring quirks rather than being two
  separate confirmations. Read as "nonzero in one independent corpus and one correlated pair,"
  NOT "triple replication." Still a real non-redundancy finding; just not triple-strength.

## Reading (hardened) — for the ruling, not the ruling

On the **redundancy** question the evidence is solid: (b) is a pure duplicate of `h1_band`
(0/1197, definitional → "duplicate = cruft"), while (a) is demonstrably **not reducible** to
`h1_band` (36 divergences, present in the one independent corpus AND the correlated twin pair).
What the cross-tab establishes is **non-redundancy only.** It does NOT establish that (a)'s extra
signal is *reliable* — i.e. that the 36 divergences are cases where (a) is correct rather than
cases where the authored stakeholder facts are noisy/inconsistent/wrong. Seat-disagreement
direction is not detector accuracy. Ruling (a) answers "is the omega redundant?" (no); it does
NOT answer "is authored-stakeholder disagreement trustworthy signal rather than annotation
noise?" — that is **OQ-199**, open. The ruling remains the operator's seat.

## Caveats

- The perfect (b)⟺h1 coincidence is definitional coextension (same canonical `dr_type` orbit,
  two readings), so it means "(b) is redundant," NOT "(b) is independently confirmed by h1."
- The twins are correlated, not independent (above) — "three corpora" is one independent + one
  correlated pair.
- **Non-redundancy ≠ reliability.** The finding is that (a) is *irreducible to* `h1_band` as a
  *construction*; whether that irreducible signal is trustworthy (vs authoring noise) is
  unaddressed here — OQ-199. Do not let non-redundancy read as validation of the (a) signal.
- The both-determinate restriction drops ~half of each twin (452/960, 661/960) — the excluded
  constraints are exactly the present-but-insufficient / different-coverage population OQ-197 is
  about, correctly held out of the overlap signal per the operator instruction.
