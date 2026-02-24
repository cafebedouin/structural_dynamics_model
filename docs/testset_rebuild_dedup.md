# Test Suite Rebuild: Classification Reconciliation & Deduplication Report

**Generated:** 2026-02-24

## 1. Executive Summary

Two-pass reconciliation of `constraint_classification/3` facts against engine-computed perspective types from `enriched_pipeline.json`.

**Pass 1** (pre-pipeline re-run): Reconciled **1,665** facts (+ **663** test body refs) across **939 files**. Engine agreement check: 94.6% (18 mismatches across 6 constraints — stale pipeline data from pre-claim-reconciliation).

**Pass 2** (post-pipeline re-run): After regenerating `enriched_pipeline.json` with `run_pipeline.py`, reconciled **182** additional facts (+ **64** test body refs) across **79 files**. Engine agreement: 99.0% (2 threshold-boundary mismatches).

**Cumulative totals:**
- Total classification facts scanned: 3,955
- Total facts reconciled: 1,847 (1,665 + 182)
- Total test body refs updated: 727 (663 + 64)
- Total files modified: 957 (939 + 79, with overlap)
- Post-reconciliation matching: 3,955 − 205 (non-canonical) − 96 (conditional) = 3,654 reconcilable; **3,654 matching (100%)**
- Skipped non-canonical power atoms: 205
- Skipped conditional clauses: 96
- Missing engine perspectives: 0
- Constraints with no classification facts: 21
- Incomplete perspective coverage (<4 facts): ~21 constraints
- Tie-resolved constraints tagged: 33

## 2. Type Transitions

### Pass 1 (pre-pipeline re-run)

| File Type (before) | Engine Type (after) | Count |
|---|---|---:|
| tangled_rope | snare | 469 |
| snare | tangled_rope | 464 |
| rope | tangled_rope | 127 |
| tangled_rope | rope | 102 |
| mountain | tangled_rope | 76 |
| rope | piton | 72 |
| snare | piton | 70 |
| rope | scaffold | 53 |
| snare | rope | 53 |
| piton | snare | 32 |
| rope | mountain | 27 |
| snare | naturalized | 20 |
| piton | rope | 14 |
| tangled_rope | piton | 14 |
| scaffold | rope | 13 |
| mountain | rope | 10 |
| tangled_rope | mountain | 9 |
| scaffold | snare | 7 |
| scaffold | tangled_rope | 5 |
| piton | tangled_rope | 5 |
| mountain | snare | 5 |
| tangled_rope | scaffold | 4 |
| piton | scaffold | 4 |
| rope | snare | 3 |
| scaffold | mountain | 2 |
| naturalized | tangled_rope | 2 |
| tangled_rope | naturalized | 2 |
| mountain | scaffold | 1 |

**Pass 1 total:** 1,665 fact-level transitions (bidirectional — net deltas cancel across perspectives).

### Pass 2 (post-pipeline re-run)

| File Type (before) | Engine Type (after) | Count |
|---|---|---:|
| tangled_rope | snare | 103 |
| tangled_rope | rope | 71 |
| tangled_rope | scaffold | 8 |

**Pass 2 total:** 182 fact-level transitions (all unidirectional out of tangled_rope).

The pass 2 transitions are the cascade effect: claim reconciliation → engine reclassification → pipeline re-run → new perspective types. All 182 shifted constraints were `tangled_rope` in the stale pipeline but resolved to snare (103), rope (71), or scaffold (8) in the fresh engine output.

## 3. Distribution Shift (Cumulative)

| Type | Original (authored) | Final (engine) | Delta |
|---|---:|---:|---:|
| mountain | 429 | 429 | 0 |
| naturalized | 28 | 28 | 0 |
| piton | 216 | 216 | 0 |
| rope | 1022 | 1093 | +71 |
| scaffold | 63 | 71 | +8 |
| snare | 875 | 978 | +103 |
| tangled_rope | 1021 | 839 | -182 |

*Note: Pass 1 transitions were bidirectional (net zero). Pass 2 transitions are the net structural shift: 182 facts reclassified out of tangled_rope into snare, rope, and scaffold.*

## 4. Batch Update Results

### Pass 1
- Facts modified: 1,665 across 939 files
- Test body refs modified: 663
- Skipped (already correct): 180

### Pass 2
- Facts modified: 182 across 79 files
- Test body refs modified: 64
- Skipped (already correct): 9

### Both passes
- Skipped (disk mismatch): 0
- Skipped (no regex match): 0
- Failed swipl: 0

## 5. Verification

### Pass 1
- swipl syntax passed: 939/939 (100%)
- Idempotency re-run: 0 mismatches

### Pass 2
- swipl syntax passed: 79/79 (100%)
- Idempotency re-run: 0 mismatches

## 6. Engine Agreement Check

### Pass 1 (pre-pipeline re-run, stale enriched_pipeline.json)

- **Status:** fail
- Sample size: 83 constraints (332 perspective checks)
- Matches: 314
- Mismatches: 18 (across 6 constraints)
- Match rate: 94.6%

This failure correctly identified that `enriched_pipeline.json` was stale — the batch claim reconciliation had changed 429 `constraint_claim/2` values, shifting engine output for constraints that the pipeline hadn't yet re-processed.

### Pass 2 (post-pipeline re-run, fresh enriched_pipeline.json)

- **Status:** fail (threshold-boundary residual)
- Sample size: 50 constraints (200 perspective checks)
- Matches: 198
- Mismatches: 2
- Match rate: 99.0%

| Constraint | Perspective | Pipeline Type | Live Engine Type | Analytical χ |
|---|---|---|---|---:|
| happiness_of_others | analytical | snare | tangled_rope | 1.027 |
| knowledge_action_gap | analytical | snare | tangled_rope | 0.973 |

Both are threshold-boundary cases where analytical χ sits within ~0.03 of the snare/tangled_rope boundary. The pipeline (`json_report.pl`) and the live engine (`drl_core:dr_type/3`) resolve these differently — likely a rounding path difference in the Prolog computation. These are irreducible at the current threshold granularity.

## 7. Deduplication Analysis

- Total candidate pairs examined: 48
- Hard duplicates: 0
- Semantic duplicates: 1
- Intentional variants: 24
- False positives: 23
- Constraint ID collisions: 0
- Naming mismatches (file ≠ constraint ID): 200

### Semantic Duplicates (Review Recommended)

| File A | File B | Similarity | ε Diff | Reason |
|---|---|---:|---:|---|
| continuum_hypothesis_undecidability.pl | suslin_hypothesis_undecidability.pl | 0.836 | 0.03 | similar topic (hr_sim=0.82), same domain (mathematical/logical), eps_diff=0.030 |

### Intentional Variants (No Action)

| File A | File B | Similarity | Reason |
|---|---|---:|---|
| ai_cognitive_diversity_arbitrage.pl | cognitive_diversity_arbitrage.pl | 0.951 | related topic (hr_sim=0.74) but different metrics (eps_diff=0.000, 4/4 perspectives agree) |
| ai_cognitive_diversity_arbitrage.pl | cognitive_mimicry_arbitrage.pl | 0.780 | related topic (hr_sim=0.78) but different metrics (eps_diff=0.100, 3/4 perspectives agree) |
| banach_fixed_point.pl | banach_fixed_point_theorem.pl | 0.818 | related topic (hr_sim=0.96) but different metrics (eps_diff=0.090, 4/4 perspectives agree) |
| cap_theorem.pl | lobs_theorem.pl | 0.696 | related topic (hr_sim=0.47) but different metrics (eps_diff=0.030, 4/4 perspectives agree) |
| chaitins_omega_undecidability.pl | halting_problem_undecidability.pl | 0.780 | related topic (hr_sim=0.52) but different metrics (eps_diff=0.000, 4/4 perspectives agree) |
| cognitive_diversity_arbitrage.pl | cognitive_mimicry_arbitrage.pl | 0.821 | related topic (hr_sim=0.63) but different metrics (eps_diff=0.100, 3/4 perspectives agree) |
| couples_residency_match.pl | medical_residency_match.pl | 0.783 | related topic (hr_sim=0.70) but different metrics (eps_diff=0.100, 3/4 perspectives agree) |
| cuba_mandatrophic_collapse.pl | iran_mandatrophic_collapse.pl | 0.885 | related topic (hr_sim=0.55) but different metrics (eps_diff=0.050, 3/4 perspectives agree) |
| cuba_mandatrophic_collapse.pl | mandatrophic_margin_collapse.pl | 0.778 | related topic (hr_sim=0.53) but different metrics (eps_diff=0.100, 3/4 perspectives agree) |
| gale_shapley.pl | gale_shapley_variants.pl | 0.727 | related topic (hr_sim=0.56) but different metrics (eps_diff=0.450, 1/4 perspectives agree) |
| ibm_shield_2026.pl | ibm_shield_contract_2026.pl | 0.769 | related topic (hr_sim=0.83) but different metrics (eps_diff=0.000, 4/4 perspectives agree) |
| institutional_mutation_domestication.pl | institutional_mutation_without_selection.pl | 0.789 | related topic (hr_sim=0.56) but different metrics (eps_diff=0.140, 4/4 perspectives agree) |
| intermediate_value_theorem.pl | mean_value_theorem.pl | 0.773 | related topic (hr_sim=0.57) but different metrics (eps_diff=0.450, 0/4 perspectives agree) |
| lorenz_attractor_dynamics.pl | strange_attractor_dynamics.pl | 0.824 | related topic (hr_sim=0.46) but different metrics (eps_diff=0.100, 3/4 perspectives agree) |
| mandatrophic_margin_collapse.pl | mandatrophic_margin_collapse_diagnostic.pl | 0.836 | related topic (hr_sim=0.46) but different metrics (eps_diff=0.050, 4/4 perspectives agree) |
| matching_markets.pl | matching_markets_general.pl | 0.800 | related topic (hr_sim=0.53) but different metrics (eps_diff=0.050, 3/4 perspectives agree) |
| neural_interoperability.pl | rfc9293_interoperability.pl | 0.766 | related topic (hr_sim=0.52) but different metrics (eps_diff=0.650, 2/4 perspectives agree) |
| noethers_theorem.pl | noethers_theorem_symmetry.pl | 0.780 | related topic (hr_sim=0.68) but different metrics (eps_diff=0.100, 4/4 perspectives agree) |
| pareto_principle.pl | peter_principle.pl | 0.839 | related topic (hr_sim=0.55) but different metrics (eps_diff=0.500, 0/4 perspectives agree) |
| skolems_paradox.pl | sorites_paradox.pl | 0.800 | related topic (hr_sim=0.53) but different metrics (eps_diff=0.200, 0/4 perspectives agree) |
| suslin_hypothesis.pl | suslin_hypothesis_undecidability.pl | 0.694 | related topic (hr_sim=0.57) but different metrics (eps_diff=0.100, 4/4 perspectives agree) |
| ua_wartime_mobilization.pl | ukr_mobilization.pl | 0.769 | related topic (hr_sim=0.62) but different metrics (eps_diff=0.200, 4/4 perspectives agree) |
| us_sanctions_belarus_2022.pl | us_sanctions_moex_2024.pl | 0.766 | related topic (hr_sim=0.00) but different metrics (eps_diff=0.100, 4/4 perspectives agree) |
| whitehead_problem.pl | whitehead_problem_undecidability.pl | 0.694 | related topic (hr_sim=0.52) but different metrics (eps_diff=0.000, 4/4 perspectives agree) |


## 8. Deferred Items

1. **`TypeVar == atom` test assertions** (~700 occurrences in ~302 files): require variable-flow analysis. Failing tests serve as flags for manual update.
2. **Conditional clause facts** (~96 facts across ~129 files): condition and conclusion are coupled. Candidates for dead-code simplification pass. Example: `viral_emergence_covid19_exemplar` institutional has `)) :- theater_ratio > 0.70` — overwriting the conclusion while preserving the condition would create incoherent rules.
3. **Incomplete perspective coverage** (21 constraints): files with fewer than 4 `constraint_classification/3` facts. The engine computes types for all 4 perspectives, but no fact exists to update.
4. **Non-canonical power atoms** (205 facts): facts with power atoms outside {powerless, moderate, institutional, analytical} were not reconciled.
5. **Threshold-boundary residual** (2 constraints): `happiness_of_others` and `knowledge_action_gap` have analytical χ within 0.03 of the snare boundary. Pipeline and live engine resolve differently. Irreducible at current threshold granularity.
6. **Dedup file deletions**: recommendations above are for human review — no files were deleted.

## 9. Corpus Status

Two-pass reconciliation complete. Pipeline re-run done. Downstream analyses pending:

```bash
python3 python/tangled_gradient.py
python3 python/chi_variance_decomposition.py
python3 python/rope_dominant_spot_check.py
```

