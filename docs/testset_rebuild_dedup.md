# Test Suite Rebuild: Classification Reconciliation & Deduplication Report

**Generated:** 2026-02-24
**Mode:** DRY RUN — no files were modified

## 1. Executive Summary

Reconciled **0** `constraint_classification/3` facts (+ 0 test body references) across 1151 constraints to match engine-computed perspective types.

- Total classification facts scanned: 3955
- Already matching engine: 3654 (92.4%)
- Mismatched (updated): 0
- Skipped non-canonical power atoms: 205
- Skipped conditional clauses: 96
- Missing engine perspectives: 0
- Constraints with no classification facts: 21
- Tie-resolved constraints tagged: 33

## 2. Type Transitions

No transitions — all facts already matched engine output.

## 3. Distribution Shift

| Type | Before (file) | After (engine) | Delta |
|---|---:|---:|---:|
| mountain | 429 | 429 | 0 |
| naturalized | 28 | 28 | 0 |
| piton | 216 | 216 | 0 |
| rope | 1093 | 1093 | 0 |
| scaffold | 71 | 71 | 0 |
| snare | 973 | 973 | 0 |
| tangled_rope | 844 | 844 | 0 |

## 4. Batch Update Results

- Would modify (facts): 0
- Would modify (test body): 0
- Skipped (already correct): 0
- Skipped (disk mismatch): 0
- Skipped (no regex match): 0
- Failed swipl: 0

## 5. Verification

*(skipped in dry-run mode)*

## 6. Engine Agreement Check

*(skipped in dry-run mode)*

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
2. **Conditional clause facts** (~129 files): condition and conclusion are coupled. Candidates for dead-code simplification pass.
3. **Non-canonical power atoms**: facts with power atoms outside {powerless, moderate, institutional, analytical} were not reconciled.
4. **Dedup file deletions**: recommendations above are for human review — no files were deleted.

## 9. Pipeline Re-run Readiness

After accepting dedup recommendations and addressing deferred items:

```bash
python3 python/run_pipeline.py
python3 python/tangled_gradient.py
python3 python/chi_variance_decomposition.py
python3 python/rope_dominant_spot_check.py
```

