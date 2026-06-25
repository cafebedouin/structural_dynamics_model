# sheaf_status ↔ Arakelov ↔ MaxEnt: the pipeline ordering dependency

**Why this doc exists:** `sheaf_status`'s `undetermined / uncomputable_height` verdict (OQ-51) is
correct *only if* MaxEnt has run before `sheaf_status` is serialized. A pipeline reorder that
violates that would silently turn **every** `h1_band==0` constraint into `undetermined` — a
mass-misclassification that reads as "lots of undetermined," not as an error. This documents the
dependency, the hazard, and the positive control that makes the hazard fail loud.

## The dependency chain

```
sheaf_status/2 (sheaf_analysis.pl)
  route 2 (uncomputable_height) fires when:  arakelov_height/2 FAILS
arakelov_height/2 (arakelov_height.pl:100)
  -> raw_confidence_margin/3 (:47)
       -> maxent_classifier:maxent_distribution_raw/3        <-- populated by maxent_run/2
```

`arakelov_height` is a **pipeline diagnostic only** — its own module header says so
(`arakelov_height.pl:16-18`): *"Requires maxent_run/2 to have been called (populating maxent_dist/3
and maxent_dist_raw/3). Not available during individual testset execution."*

Consequences:

- **In a bare `[stack]` load** (no `maxent_run`), `maxent_distribution_raw` is empty →
  `raw_confidence_margin` fails for every context → `arakelov_height_pair`'s `findall` is empty →
  `arakelov_height/2` **fails for every constraint** (witnessed 2026-06-25: computes for **0/104**).
  So a bare-context probe of route 2 is an **artifact**: every `h1==0` constraint reads
  `uncomputable_height`. This is why `tests/test_sheaf_na.pl`'s live census reports route 2 as
  `[INDETERMINATE]` in a bare load and points to `pipeline_output.json` as authoritative, and why
  any future route-2 liveness census needs the arakelov-computable positive control.

- **In the pipeline** (MaxEnt populated before serialization), `arakelov_height` computes normally.
  Witnessed: all 28 `h1_band==0` constraints in `pipeline_output.json` carry real height values;
  route 2 is **live-dormant** (route 1 `insufficient_seats`=15, route 2 `uncomputable_height`=0).
  Route 2 is a genuine safety net for a constraint that lacks ε or MaxEnt *even in the pipeline*;
  the current corpus has none.

## The hazard (pipeline ordering inversion)

`sheaf_status` is computed inside `json_report.pl` during corpus serialization. Its route-2
correctness rests on an **implicit ordering**: `maxent_run` must populate `maxent_distribution_raw`
*before* `json_report` serializes `sheaf_status`. That holds today. But if the pipeline were ever
reordered so serialization ran first, `arakelov_height` would fail for *all* constraints and every
`h1_band==0` constraint would serialize as `undetermined / uncomputable_height` — silently, with no
error, looking like a corpus that happens to be mostly undetermined.

This is the OQ-51 N/A change interacting with a pre-existing dependency: before OQ-51, an arakelov
failure fell through to `genuine_sheaf` (Pattern-5, "genuine by absence"); after OQ-51 it routes to
`undetermined/uncomputable_height`. The OQ-51 change makes the failure *louder* (undetermined ≠
genuine), but the mass-undetermined shape could still pass unnoticed without an explicit control.

## The positive control (`w1_sheaf_join.py`, Control 2b)

`w1_sheaf_join.py` is the in-pipeline sheaf/H¹ invariant checker (it already `die()`s on the W1- and
H¹-vacuity analogues). Control 2b asserts:

> **When any `h1_band==0` constraint exists, at least one must be `genuine_sheaf` or
> `fragile_presheaf`** (i.e., have a *computable* height). If every `h1_band==0` constraint is
> `undetermined`, `arakelov_height` computed for none of them — the signature of MaxEnt not having
> run — and the join `die()`s loud rather than ship a corrupt mass-undetermined output.

Why "at least one genuine/fragile" is the right test: a real corpus with MaxEnt populated always
yields *some* computable height among its `h1==0` (ε-authored) constraints, so genuine/fragile is
non-empty. The all-undetermined shape is exactly and only the maxent-didn't-run signature. (It would
also fire on a pathological corpus where *every* `h1==0` constraint genuinely lacks ε — itself a
state worth halting on.)

**Two-sided witness (the instrument is itself a claim):**
- Positive: on the correct live `pipeline_output.json`, the control passes (exit 0) — 28 genuine
  present.
- Negative-shape: flipping all 28 genuine/fragile → undetermined (simulating maxent-not-run) makes
  the control `die()` loud (`MaxEnt positive control FAILED ... 28 undetermined`, exit 1); the
  output was then restored byte-identical.

## If the control fires

It means `sheaf_status` was serialized before MaxEnt populated `maxent_distribution_raw`. Check the
`run_pipeline.py` step order: the MaxEnt analysis must run **before** the corpus export that calls
`json_report` (`sheaf_status/2`). Do **not** "fix" it by widening the control or by treating the
undetermined verdicts as data — they are an artifact of the ordering, not the corpus.

## Cross-references

- OQ-51 (ISSUES.md) — the N/A rule and the two undetermined routes.
- `tests/test_sheaf_na.pl` — synthetic route-2 control + the bare-context arakelov caveat.
- `arakelov_height.pl:16-18` — the "pipeline diagnostic only" lifecycle.
- KNOWN_STATE 2026-06-25 (OQ-51 main build) — the `arakelov`-needs-MaxEnt tripwire.
