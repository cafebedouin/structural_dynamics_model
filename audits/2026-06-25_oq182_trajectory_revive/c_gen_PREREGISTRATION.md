# C-gen pre-registration — haiku↔flash same-kernel family recovery (LIVE FALSIFIER)

**Locked 2026-06-26, BEFORE any clustering result is seen.** Do not move the bar
post-hoc (build_discipline rule #3 — silent axis-swap guard).

## Question
Cross-generation invariance: do the HAC structural families recover the same
groupings across the two model-twin corpora (`testsets_haiku/`, `testsets_flash/`)?
A real test that can fail — failure is the finding, not a nuisance.

## Substrate (confirmed by recon, not assumed)
- `testsets_haiku/` and `testsets_flash/`: **960 files each, all 960 basenames
  identical** (`comm -12` = 960). Constraint ids are 1:1 matched across legs.
- All 960 files in each leg declare `narrative_ontology:cs_kernel_id/2`.
- Corpora are loaded by overlaying `config:param(corpus_path, ...)` with **`asserta`**
  (CLAUDE.md Corpus Loading; plain `assertz` is silently ignored — the 44-vs-960 trap).
  Each run will confirm `corpus_loader:corpus_constraint/1` count = 960 (NOT the
  default 104) before trusting the partition.

## Procedure (no engine edits; `trajectory_enabled` stays 0)
`trajectory_run/2` is called directly (the flag is checked only at the Python
pipeline level, not inside the predicate — same posture as the C-null harness).
For each leg, independently:
1. overlay `corpus_path`, `load_all_testsets`, confirm `corpus_constraint/1` = 960
2. `context_profile_mining:trajectory_run(default_context, _)`
3. enumerate `family_assignment(Constraint, FamilyID)` → that leg's partition.

## Metric (locked)
**Headline:** Adjusted Rand Index (ARI) between the two legs' `family_assignment`
partitions, over the **shared clustered set** = constraints that receive a
`family_assignment` in BOTH legs. (Trajectory clustering drops constraints with no
computable trajectory; ARI is defined only over items present in both partitions.)
All shared constraints carry a `cs_kernel_id` (960/960), so "restricted to shared
cs_kernel_id constraints" is automatically satisfied by the shared-clustered set.

**Robustness check (reported, not the bar):** ARI restricted to constraints whose
`cs_kernel_id` has ≥2 readings co-clustered in both legs — guards against ARI
inflation from a trivial giant "unknown-shift" family both legs agree on. Cluster
size distributions for both legs are reported alongside.

## Pre-registered PASS bar (locked)
**Headline ARI ≥ 0.60 ⇒ PASS** ("families recover across generation").

## Near-miss band (resolution rule locked BEFORE the run)
**0.50 ≤ ARI < 0.60:** resolve by *within-kernel co-familiality preservation*.
For each `cs_kernel_id` with ≥2 readings co-clustered in both legs, a reading-pair
is "preserved" if it is same-family in haiku ⟺ same-family in flash. Let
PRES = (# preserved reading-pairs) / (# total such reading-pairs).
- **PRES ≥ 0.70** → "recovers within generation variance" (SOFT PASS); the
  inter-leg disagreement tracks real haiku-vs-flash structural differences, not
  clustering noise.
- **PRES < 0.70** → FAIL.

**ARI < 0.50 ⇒ FAIL** outright ("family structure is generation-expressive, not
kernel-recoverable; C-gen does not pass on this leg").

## Failure verdict (per plan A4)
A FAIL (or unresolved near-miss) is recorded as the finding; it does NOT auto-block
— it feeds the A4 operator stop, whose re-scope is the operator's seat. "Do not
flip, record, re-scope."
