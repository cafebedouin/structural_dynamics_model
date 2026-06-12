# OQ-105 per-row sweep — the PREDICTED bucket discharged

**Date:** 2026-06-11. **Substrate:** live corpus, 62 testset files (loader-witnessed),
commit `37ea069f`. **Status of OQ-105 after this audit:** still open — this audit converts
the entry's PREDICTED (unverified) bucket into witnessed counts; the fix-fork ruling
remains the operator's.

## Question

The OQ-105 entry's witnessed scope covered 3 of the misaligned constraints row-by-row
(2 flips landing ON substituted rows, 1 checked-negative); the remaining rows carried only a
mechanism-based prediction: "the endpoint-injection mechanism implies more of the 21 rows
date flips early wherever the scalar crosses a floor the local series does not; the per-row
sweep of the remaining rows has not been run." This audit runs that sweep: for EVERY
grid-misaligned suppression row, compare the type under the live scalar substitution against
the type under linear interpolation of the constraint's own authored series.

Note the discriminating test is sharper than the original witnessed claim: "the flip lands
ON a substituted row" does not by itself show the substitution SET the timing — the
interpolation counterfactual does.

## Method

`probe_row_sweep.pl` (default context, matches the probe5 altitude) and
`probe_row_sweep_allctx.pl` (all 156 product-site contexts, matches the OQ-110 flip-census
altitude). Per misaligned row: type under `classify_at_time/5` as-is (scalar substitution)
vs type under `classify_at_time_with_supp/7` fed the linearly interpolated local-series
value (same clause path, same SuppBacked=false). Outside the authored span, interpolation
clamps to the nearest endpoint.

**Controls (all per-process, all passed):**
- **C1 interp-identity:** interpolation evaluated AT all 215 authored series points returns
  the authored value — `215 authored points checked, 0 mismatches`.
- **C2 same-path:** re-deriving the substitution type through the counterfactual call
  (`classify_at_time_with_supp` with the scalar) equals `classify_at_time/5`'s type at all
  23 rows — `path_control_failures=0`.
- **C3 enumeration fires:** the misaligned-row enumeration reproduces all six
  probe5-witnessed substitution rows, and the census re-derives to exactly the OQ-110
  cross-read figures: **23 rows / 11 constraints**.

## Results

**Default context (`probe_row_sweep.out`):** `TOTAL misaligned_rows=23 host_constraints=11
divergent_rows=3 path_control_failures=0`. Divergent rows — all tangled_rope→snare,
substitution early-dating the snare:

| constraint | T | substituted | interpolated | type sub | type interp |
|---|---|---|---|---|---|
| agenda_conditioning | 10 | 0.72 | 0.58 | snare | tangled_rope |
| post_1998_convergence | 13 | 0.72 | 0.56 | snare | tangled_rope |
| technocratic_paradigm_vs_human_primacy | 9 | 0.62 | 0.58 | snare | tangled_rope |

**All 156 product contexts (`probe_row_sweep_allctx.out`):** `TOTAL cells(row x ctx)=3588
divergent_cells=181` (5.0%). Divergence concentrates in **4 of 23 rows** — the three above
plus `truth_democracy_disinformation` T=2 (22 non-default contexts; not divergent at the
default context). Per-row divergent-cell counts: 49 / 55 / 55 / 22. **Every one of the 181
divergent cells is the same type pair** (`sub=snare, interp=tangled_rope`; three sub/interp
value combinations, one mechanism): the substituted endpoint scalar sits at or above the
snare suppression floor (0.60) while the locally interpolated value sits below it. No other
floor and no other type transition fired anywhere in the sweep.

**19 of 23 rows are substitution-robust at every context** — the substituted type equals the
interpolated type in all 156 contexts.

**Refinement of the original witnessed bucket:** `substantive_employment_reading` T=9 — one
of the two original "flip lands ON a substituted row" witnesses — is NOT timing-distorted:
its interpolated value (0.62) also clears the 0.60 floor, so the flip dates the same under
either reading. Of the two original witnesses, only `post_1998_convergence` T=13 survives
the discriminating counterfactual.

## Interpretation for the fix fork

- The damage class is now fully enumerated, small, and single-mechanism: 4 rows / 4
  constraints, all snare-floor early-dating, on a corpus where the Backed gate already
  excludes every misaligned row from counted flips (OQ-110: 0 counted flips on or adjacent
  to a misaligned row).
- The exposure named in the OQ entry — flip timing in consumers of raw
  `classify_at_time`/`constraint_history` timelines that do not read the Backed bit —
  is therefore bounded to exactly these 4 rows on the current corpus.
- Interpolation as a read-side semantics is validated mechanically (C1) and everywhere it
  changes anything it moves the value toward the causally plausible side (no early snare);
  but it would buy correctness for exactly 4 rows of live data at the cost of a permanent
  third arm in the suppression read ladder.

## Artifacts

| file | role |
|---|---|
| `probe_row_sweep.pl` / `.out` / `.stderr` | default-context sweep + controls C1–C3 |
| `probe_row_sweep_allctx.pl` / `.out` / `.stderr` | 156-context sweep, census-altitude divergence |
