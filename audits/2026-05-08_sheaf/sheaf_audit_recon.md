# Sheaf/Presheaf Boundary Audit: Reconnaissance

*Prepared for the 10-slice Tier-1 binary H¹ boundary test.*
*Generated: 2026-05-08*

---

## 1. Machinery Location and Observer-Site Agnosticism

### H¹ Cohomology

**File:** `prolog/grothendieck_cohomology.pl`
**Core predicates:** `cohomological_obstruction/3` (lines 154–165), `count_disagreeing_pairs/2` (lines 171–179), `orbit_vector/2` (lines 131–133)

`cohomological_obstruction(C, H0, H1)` calls `orbit_vector/1`, which in turn calls `constraint_indexing:site_contexts/1` and then `drl_core:dr_type/3` for each context. H¹ is the count of disagreeing context-pairs in the resulting type vector. This count is a pure function of the type vector — it is indifferent to site size and runs identically on 4-context, 156-context, or any N-context vector.

**Site control:** `constraint_indexing:site_contexts/1` (line 919) dispatches on `config:param(site_mode, Mode)`. Mode ∈ {canonical, product}. Adding `ten_slice` would require a new predicate and dispatch case.

**Binary gap:** On the canonical 4-point site, H¹ ∈ {0, 3, 4, 5, 6} (values 1 and 2 are combinatorially impossible). On larger sites, the minimum non-zero H¹ grows; the binary boundary H¹=0 vs H¹>0 is what the site-stability claim concerns.

**Observer-site verdict: AGNOSTIC.** H¹ computation is fully parameterized by site_contexts/1 and works on any N-element type vector.

### Arakelov Height

**File:** `prolog/arakelov_height.pl`
**Core predicate:** `arakelov_height_pair/3` (lines 100–111)

The formula `Height = ε × (raw_uncertainty + conditional_pressure)` is site-agnostic in structure. However, `raw_confidence_margin/3` (lines 47–53) calls `maxent_classifier:maxent_distribution_raw/3`, which is populated during the pipeline's MaxEnt run. MaxEnt distributions are generated for contexts in `measurement_layer:wasserstein_contexts/1` (line 73), which itself delegates to `site_contexts/1`.

**Blocker:** MaxEnt distributions are not available in the current pipeline run for 10-slice contexts. The `pipeline_output.json` `arakelov_height` fields are computed over canonical 4 contexts only. Computing Arakelov on the 10-slice site would require:
1. Adding `site_contexts_ten_slice/1` to `constraint_indexing.pl`
2. Re-running MaxEnt for those contexts
3. Re-running the full pipeline

**Observer-site verdict: DEFERRED.** Arakelov height for the 10-slice site is not tractable from existing data. The fragile/genuine sub-partition question (Q3) is deferred to a subsequent pass.

### Nash Distance

**File:** `python/game_theory_nash.py`
**Core function:** `compute_nash_distance(vec, labels)` (lines 91–126), `h1_from_vector(vec)` (lines 75–83)

Both functions are pure Python operating on lists. `compute_nash_distance` is O(n), site-agnostic, and works identically on any N-element type vector. The computation reads from `outputs/orbit_data.json` (canonical 4-context type vectors) for its default canonical-site run.

**Observer-site verdict: AGNOSTIC.** Nash distance is fully parameterizable by the type vector provided.

---

## 2. Ten-Slice Coverage in Existing Pipeline Data

The audit reads 10-slice type vectors from `pipeline_output.json`'s `classifications` field, which contains per-testset-context type assignments for each constraint. These come from `constraint_classification/3` Prolog facts authored in testset files.

**Coverage distribution:**

| n 10-slice contexts | Constraints |
|---|---|
| 0 | 8 (excluded) |
| 1 | 493 (excluded — no H¹ possible with 1 context) |
| 2 | 334 |
| 3 | 509 |
| 4 | 767 |
| 5 | 824 |
| 6 | 393 |
| 7 | 7 |
| **≥2 (working set)** | **2834** |

Maximum per-constraint coverage: 7 of 10 contexts. No constraint is classified at all 10 slices. The effective site is per-constraint variable.

**Data quality note:** 135 constraints have type disagreements at the same context across different testset files (138 total conflicts). These are constraints where two testset files declare different types for the same (P,T,E,S) tuple. The audit uses first-occurrence for deduplication. These 135 constraints (4.7% of working set) introduce noise but do not drive the main finding.

---

## 3. Canonical Site Drift

The 10-slice family differs from the canonical 4-point site in three structurally important ways:

**No canonical U3.** The canonical U3 = (institutional, generational, arbitrage, national) is absent. U_3_imm (immediate time), U_3_civ (civilizational time), and U_3_nat (immediate, national) are the 10-slice analogs.

**Scope divergence at analytical.** The canonical analytical context is (analytical, civilizational, analytical, **global**, scope_modifier=1.2). The 10-slice family's U_4 uses scope=**universal** (scope_modifier=1.0). This is the most consequential drift point for the binary boundary test.

**New power atom.** Two slices use P=organized (organized, org_nat), which is excluded from the product site due to limited canonical_d calibration. The code does have `canonical_d_organized` configured, and testsets include organized-power classifications.

**Overlap with canonical site.** Only U_4_glob (analytical, civilizational, analytical, global) = canonical U4. Three of four canonical contexts are not in the 10-slice family.

---

## 4. Predicted Crossing Source

**U_3_civ piton pattern (originally predicted):** The idea_site_exploration flow matrix shows 1171 rope→piton transitions at U_3_civ (institutional/civilizational/arbitrage/global). Constraints classified as rope at canonical U3 would see piton at U_3_civ, potentially producing H¹ > 0 on the 10-slice site. This was identified as the primary expected source of sheaf→presheaf crossings.

**Scope modifier at U_4 (discovered during recon):** scope_modifier(universal) = 1.0 (neutral — natural laws) vs scope_modifier(global) = 1.2 (amplified extraction). At the analytical observer position, χ = ε × f(d) × σ(S). With d(analytical) near 0, f(d) is strongly negative (institutional sign-flip territory). σ(universal) = 1.0 reduces χ magnitude less than σ(global) = 1.2. For constraints near the rope_chi_ceiling boundary, the scope shift can flip classification from tangled_rope/snare (at global) to mountain (at universal).

The product site explicitly excludes universal scope ("Excluded scope values: regional, continental, universal") to prevent this class of crossing. This exclusion was a design choice in the product-site construction.

**Audit prediction:** U_4 scope-modifier mechanism may produce more crossings than originally predicted. The recon flags this as an open question resolved by running the audit.

---

## 5. Audit Plan

**Inputs:**
- `outputs/pipeline_output.json`: h1_band (canonical H¹), classifications (10-slice types), arakelov_height
- `outputs/orbit_data.json`: canonical 4-context type vectors (note: may be stale if pipeline was not re-run after testset updates)

**Per-constraint:**
1. Collect 10-slice type vector from `classifications` field; deduplicate (first-occurrence); sanity-check conflicts
2. Skip if n < 2 → excluded count
3. Compute 10-slice H¹ (disagreeing pairs in type vector)
4. Compare binary: canonical_sheaf = (h1_band == 0), ten_slice_sheaf = (h1_ten == 0)
5. Flag crossings; identify driving slices (minority types in 10-slice orbit)
6. Compute Nash distance (canonical from orbit_data, 10-slice from 10-slice type vector)

**Aggregates:**
- Overall binary preservation rate
- Stratified by n_contexts (2, 3, 4, 5, 6, 7) — primary diagnostic for variable-subsite artifact
- Crossing characterization by driving slice, claimed_type, signature
- Nash: of canonical Nash-distance-1 constraints, how many become Nash-stable on 10-slice?

**Verdict criteria:**
- 0 crossings: boundary preserved in testset-covered positions; variable subsite limits inference
- < 5%: note and characterize
- ≥ 5%: full paper; identify failure mode, propose site-stability claim qualification

**Arakelov:** null for 10-slice; canonical values recorded from pipeline; fragile/genuine question deferred
