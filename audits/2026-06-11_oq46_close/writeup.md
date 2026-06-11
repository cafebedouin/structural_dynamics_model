# OQ-46 close — the scalar suppression fallback is sanctioned, not a stopgap (operator ruling 2026-06-11)

**Question.** OQ-46 held that the `classify_at_time` scalar `suppression_requirement` fallback
(`drl_composition.pl:215-216`, the OQ-41 row-23 fix) was a temporary STOPGAP, retirable once the
generation template authors a temporal series for every constraint. A read-only evidence pass was
run to enable the ruling; the operator ruled **accept the prompt's design** (option 1 below).

## Findings (each witnessed in `evidence/`)

**F1 — OQ-46's premise contradicts the live generation prompt.**
`prompts/constraint_story_generation_prompt_json.md:457` — in the prompt since **2026-05-30**
(commit `220739b8`, i.e. before the corpus reset, so the entire live corpus was generated under
it) — instructs: *"Do NOT author `suppression_requirement` measurements unless the story's
narrative specifically tracks enforcement-capacity change; a static enforcement picture is
already captured by `base_properties.suppression`."* The schema agrees: `measurements` is not
top-level-required and nothing mandates `suppression_requirement` among the metrics
(`schemas/constraint_story_schema.json`). The 7 scalar-only live constraints are
**prompt-conformant static-enforcement stories** (all physics/structural topics, scalar
suppression 0.01–0.35; two 2026-06-09 batches, 3 of them regenerated under the
"required-metrics schema" commit `fb9ad098` and still scalar-only) — not template failures.
Under the current prompt the OQ-46 wait-state ("once the remaining N author the series") never
terminates by design.

**F2 — live census (probe1, 3 per-process positive controls fired).**
48 `corpus_constraint/1` = 46 stories + 2 non-story `cs_axiom_contradiction/2` record files
(`employment_boundary_contradictions`, `human_dignity_ai_governance_contradictions` — zero
measurements; the source of the "48 files / 46 classified" gap; the `unknown` floor has zero
live load). Stories: 39 temporal / 7 scalar-only. All 39 temporal constraints also carry the
scalar (dual representation is universal).

**F3 — second live load OQ-46 never recorded: time-grid misalignment (probe2).**
Row-denominated: 209 rows = 162 temporal-branch + **21 alignment-gap** + 26 scalar-only.
The 21 gap rows live in **10 constraints that DO author a suppression series** — suppression is
sampled on a coarser grid than the other metrics (e.g. series at [0,6,12], other metrics also
at [3,9]). Universal series-authoring alone would NOT have retired the fallback.

**F4 — deletion counterfactual (probe3).** Deleting the scalar clause today changes
**16/46 timelines**: the 7 scalar-only collapse entirely (`[mountain]→[unknown]`,
`[scaffold]→[unknown]`), 9 series-authoring constraints gain interleaved unknowns = phantom
transitions in `drift_trajectory` (emitted in `pipeline_output.json`). Kernel registry T=0
exposure: only `substantive_employment_reading`, which has suppression at T=0 — unaffected.
Deletion also re-creates the exact premise-error the row-23 positive control caught in 2026-05
("`unknown` discards real authored data").

**F5 — the divergence risk is latent; the provenance machinery already exists.**
`snapshot_type/3` / `degradation_chain/3` have **zero consumers** (recursive grep with positive
control — 3 hits inside `transition_paths.pl` itself, none elsewhere), so the OQ-41 "split
reopens" concern is latent. And since the Type-A floor (2026-06-08), `classify_at_time/5`
carries `Backed=false` on every scalar-supplied snapshot, consumed by `temporal_residual`
(OQ-83) to exclude phantom flips — the provenance bit retirement was meant to buy is in place.

## Ruling (operator, 2026-06-11)

**Accept the prompt's design.** A scalar `base_properties.suppression` is the sanctioned
authoring for static-enforcement stories; the temporal series is authored only when enforcement
capacity itself changes. Consequently the `classify_at_time` scalar fallback is a **permanent,
sanctioned read path** (temporal measurement at T → authored scalar-as-constant → fail-closed
`unknown`), not a stopgap. `Backed=false` remains the per-snapshot provenance marker for
scalar-supplied rows. The alternatives (enforce series-always + grid alignment + regenerate;
or delete now) were declined — costs witnessed in F3/F4.

## Changes landed with this ruling

- `ISSUES.md` OQ-46 → **resolved** (compressed; ruling block kept — still-operative).
  Stale cross-refs at the OQ-33 note, OQ-40 suppression sub-split note, and OQ-41 row 23 updated.
- `prolog/drl_composition.pl` — STOPGAP comment block rewritten to sanctioned-fallback
  (comment-only; zero behavior change).
- `docs/technical/classify_at_time_wiring.md` §1 — updated with the ruling and live figures.
- `KNOWN_STATE.md` — dated entry.

## Evidence files

- `probe1_census.pl` / `probe1_output.txt` — constraint-denominated census, controls
  (unknown-floor, stopgap-scalar, temporal-branch — all FIRED), scalar-only constraints named
  with scalar values and authored temporal metrics, dual-representation count.
- `probe2_row_split.pl` / `probe2_output.txt` — row-denominated 162/21/26 split, alignment-gap
  constraints named with their suppression grids vs gap rows.
- `probe3_deletion_counterfactual.pl` / `probe3_output.txt` — per-constraint current vs
  counterfactual timelines, kernel-registry exposure, T=0 unknown set.

All probes run from `prolog/` in worktree `wt-oq46` (branch `oq46-ruling`, base `ba3687d6`),
live corpus 48 testsets (newest file 2026-06-10 21:06). Substrate caveat: counts are
staleness-laddered — recompute before citing; the corpus grows.
