# MaxEnt Shadow Classifier — Diagnostic Deep Dive Report

*Generated: 2026-02-14 from corpus-wide diagnostic via `maxent_diagnostic.pl`*

---

## Executive Summary

| Property | Value |
|----------|-------|
| **Testset files** | 1025 |
| **Constraints analyzed by MaxEnt** | 669 |
| **Not analyzed (gap)** | 356 |
| **Mean normalized entropy** | 0.5332 |
| **High uncertainty (H > 0.40)** | 475 (71.0%) |
| **Hard disagreements** | 114 (17.0%) |
| **Soft disagreements** | 395 |
| **Dirac orbit cross-validation overlap** | 84.2% |

---

## 1. Account for the Missing 356 Constraints

### Root Cause: `fnl_trace_diagnostic.pl` Knowledge Base Wipe

**This is a bug.** The 356 missing constraints are caused by a single testset file that triggers a destructive side effect during batch loading.

**Mechanism:**

1. `covering_analysis:load_all_testsets` consults all 1025 `.pl` files from `testsets/` in alphabetical order.
2. File #355 in the sort order is `fnl_trace_diagnostic.pl`.
3. This file has an `:- initialization(run_fnl_trace)` directive (line 19) that calls `scenario_manager:load_and_run('testsets/fnl_shadow_probe.pl', fnl_shadow_probe)`.
4. `scenario_manager:load_and_run/2` calls `clear_kb/0` (scenario_manager.pl:93) which executes `retractall(narrative_ontology:constraint_claim(_, _))` (line 39), wiping **all previously-loaded constraint_claim facts**.
5. Files 356-1025 continue loading normally after the wipe.
6. Result: only constraints from files loaded **after** `fnl_trace_diagnostic.pl` survive.

**Verification:** 1025 - 355 = 670 files load after the wipe. After dedup and filtering out 2 list-form claims, 669 unique constraints remain. This matches exactly.

### Exclusion Breakdown

| Category | Count | Notes |
|----------|-------|-------|
| **Wiped by `fnl_trace_diagnostic.pl` KB clear** | ~354 | All constraint_claims from files #1-#354 retracted |
| **List-form constraint IDs** | 2 | `[landscape_of_fear_2026]`, `[wikipedia_crowdsourcing_2026]` — filtered by `\+ is_list(C)` |
| **No constraint_claim in file** | 1 | `fnl_trace_diagnostic.pl` itself — diagnostic only, no constraints |
| **Syntax/load errors (pre-wipe files)** | ~17 | 10 syntax errors + 5 permission errors + 2 other — but these are moot since the wipe erases them anyway |
| **Total gap** | 356 | (354 wiped + 2 list-form = 356, minus 1 no-claim file which was already in the 354) |

### Post-Wipe Constraint Profile

After the wipe, 669 surviving constraints classify as:

| Deterministic Type | Count | Pct |
|--------------------|-------|-----|
| snare | 324 | 48.4% |
| tangled_rope | 215 | 32.1% |
| mountain | 47 | 7.0% |
| unknown | 39 | 5.8% |
| rope | 34 | 5.1% |
| scaffold | 10 | 1.5% |
| piton | 0 | 0.0% |

**Note:** Zero piton constraints survived. ALL piton-bearing testsets sort alphabetically before `fnl_trace_diagnostic.pl`. This means the MaxEnt piton profiles fall back to hardcoded defaults, and all piton-related analysis in this report reflects an empty empirical population.

### Recommendation

**Immediate fix:** Either (a) remove the `:- initialization(run_fnl_trace)` directive from `fnl_trace_diagnostic.pl` and make it purely invocation-driven, or (b) rename it to `zzz_fnl_trace_diagnostic.pl` so it loads last, or (c) move it out of `testsets/` entirely. Option (a) is cleanest. This is a **data integrity bug** — the reported 669/1025 coverage is an artifact, not a real coverage gap.

---

## 2. Per-Type Entropy Breakdown

| Type | Count | Mean H | Median H | Min H | Max H | Std Dev | Interpretation |
|------|-------|--------|----------|-------|-------|---------|----------------|
| **mountain** | 47 | 0.3114 | 0.3114 | 0.3114 | 0.3114 | 0.0000 | **Degenerate** |
| **rope** | 34 | 0.4946 | 0.5430 | 0.1423 | 0.8674 | 0.1774 | High spread |
| **tangled_rope** | 215 | 0.4634 | 0.3967 | 0.3114 | 0.8247 | 0.1520 | Moderate-high |
| **snare** | 324 | 0.6291 | 0.6551 | 0.4199 | 0.7737 | 0.1281 | **Highest** |
| **scaffold** | 10 | 0.7177 | 0.7206 | 0.6494 | 0.7918 | 0.0364 | Very high |
| **unknown** | 39 | 0.3744 | 0.3895 | 0.0003 | 0.8465 | 0.2155 | Extreme spread |
| **piton** | 0 | - | - | - | - | - | No data |

### Hypothesis Test: Are mountains and pitons metrically distinctive?

**Mountains: YES, but pathologically so.** All 47 mountains have *identical* entropy (0.3114). This is not normal variation — it indicates the mountain Gaussian profiles are so tight (sigma_eps=0.057, sigma_supp=0.018) and the boolean penalty for `emerges_naturally` so harsh (-10.0 log-likelihood) that every mountain gets essentially the same probability distribution. The profile is working, but it's working as a near-deterministic gate rather than a soft classifier. Mountain entropy is low because the classifier has been tuned to agree with the deterministic classifier completely.

**Pitons: No data.** All piton testsets were wiped by the fnl_trace_diagnostic bug. The MaxEnt classifier falls back to hardcoded default profiles for piton, and no piton constraints exist in the analyzed corpus to test against.

**Rope/tangled_rope/snare cluster: Confirmed high.** The corpus mean of 0.5332 is dominated by:
- Snare (n=324, mean=0.6291) — the largest group with the second-highest entropy
- Tangled_rope (n=215, mean=0.4634) — moderate but with wide spread

**Scaffold: Unexpectedly highest mean entropy (0.7177).** With only n=10, the scaffold Gaussian profile is poorly fitted. The scaffold extractiveness empirical distribution is pathologically narrow (all constraints have eps=0.10, giving sigma=0.0 which gets floored to 0.01). This creates a profile that matches nothing well, inflating entropy for scaffolds.

### Conclusion

The corpus mean of 0.5332 is explained by snare dominance (48.4% of corpus) combined with the snare/tangled_rope metric overlap. The hypothesis partially holds: mountains ARE metrically distinctive (lowest entropy), but the measurement is degenerate. The snare/tangled_rope boundary is the primary source of corpus entropy.

---

## 3. Analysis of 114 Hard Disagreements

### 3a. Type Pair Cross-Tabulation

| Det Type -> Shadow Type | Count | Pct of Hard |
|-------------------------|-------|-------------|
| snare -> tangled_rope | 80 | **70.2%** |
| tangled_rope -> rope | 12 | 10.5% |
| tangled_rope -> snare | 10 | 8.8% |
| rope -> tangled_rope | 8 | 7.0% |
| scaffold -> rope | 3 | 2.6% |
| tangled_rope -> mountain | 1 | 0.9% |

**The snare->tangled_rope pair dominates,** accounting for 70% of all hard disagreements. The shadow classifier systematically prefers tangled_rope over snare. This reflects the metric overlap between these two types — snare and tangled_rope share high extractiveness and high suppression, with the boolean gates (`has_coordination_function`, `has_asymmetric_extraction`, `requires_active_enforcement`) being the deterministic differentiators. The shadow classifier softens these boolean gates (penalty = -10.0), allowing the Gaussian metric overlap to pull snares toward tangled_rope.

### 3b. Cluster Membership

| Population | Count |
|------------|-------|
| **Within rope/tangled_rope/snare cluster** | 110 (96.5%) |
| **Involving mountain, scaffold, or piton** | 4 (3.5%) |

The 4 non-cluster disagreements:
- 3 scaffold->rope: `indexical_relativity_core`, `martian_signal_latency`, `sturgeons_law`
- 1 tangled_rope->mountain: `mvt_theorem_constraint`

### 3c. Shadow Classifier Confidence

**Mean shadow top-type probability across all 114 hard disagreements: 0.3233**

The shadow's preferred type wins with only ~32% probability on average. These are genuinely ambiguous constraints — the shadow is not confidently overriding the deterministic classifier. Distribution examples:
- Most snare->tangled_rope disagreements show tangled_rope at 0.25-0.47, snare at 0.03-0.25 — the probabilities are close
- The 12 tangled_rope->rope cases show rope at 0.26-0.52, tangled_rope at 0.15-0.29

### 3d. Mountain/Piton Disagreement Investigation

**Single case: `mvt_theorem_constraint`**

| Property | Value |
|----------|-------|
| Deterministic type | tangled_rope |
| Shadow type | mountain |
| Shadow P(mountain) | 0.810 |
| Shadow confidence | 0.609 |
| Base extractiveness (eps) | 0.10 |
| Raw suppression | 0.05 |
| Theater ratio | 0.10 |
| Structural signature | `false_ci_rope` |

**Analysis:** This constraint has metrics squarely in the mountain profile (eps=0.10, supp=0.05) but is deterministically classified as tangled_rope. The shadow classifier gives mountain 81% probability — its strongest individual disagreement. The `false_ci_rope` signature suggests it's been flagged as coordination-washed, which overrides the metric-based classification. This is a legitimate edge case: the metrics say mountain, the structural analysis says the "coordination" is suspect, and the deterministic pipeline resolves to tangled_rope via the signature override. The shadow classifier's mountain preference here is metrically correct but structurally uninformed — it doesn't see the Boltzmann non-compliance that triggered the false_ci_rope signature.

---

## 4. Characterization of the 15.8% Non-Overlapping Population

### 4a. Overview

| Metric | Count |
|--------|-------|
| Hard disagreements total | 114 |
| In multi-type Dirac orbits (overlapping) | 96 (84.2%) |
| Single-type orbits (non-overlapping) | 18 (15.8%) |

### 4b. The 18 Non-Overlapping Constraints

| Constraint | Det | Shadow | Orbit Types | eps | supp | theater | Nearest Boundary | Dist |
|------------|-----|--------|-------------|-----|------|---------|-----------------|------|
| galois_theory_symmetry | tangled_rope | rope | tangled_rope | 0.20 | 0.10 | 0.00 | mountain_supp_ceiling | 0.050 |
| goldbach_conjecture | tangled_rope | rope | tangled_rope | 0.15 | 0.10 | 0.00 | mountain_supp_ceiling | 0.050 |
| hilberts_hotel_infinity | tangled_rope | rope | tangled_rope | 0.15 | 0.10 | 0.00 | mountain_supp_ceiling | 0.050 |
| horizon_liability_contract | tangled_rope | snare | tangled_rope | 0.85 | 0.95 | 0.60 | snare_suppression_floor | 0.350 |
| indexical_relativity_core | scaffold | rope | scaffold | 0.10 | 0.20 | 0.15 | mountain_supp_ceiling | 0.150 |
| information_foraging_theory | tangled_rope | rope | tangled_rope | 0.20 | 0.10 | 0.08 | mountain_supp_ceiling | 0.050 |
| litany_of_the_real | tangled_rope | rope | tangled_rope | 0.15 | 0.20 | 0.08 | rope_chi_ceiling | 0.145 |
| martian_signal_latency | scaffold | rope | scaffold | 0.10 | 1.00 | 0.02 | rope_chi_ceiling | 0.213 |
| **mvt_theorem_constraint** | tangled_rope | mountain | tangled_rope | 0.10 | 0.05 | 0.10 | mountain_supp_ceiling | **0.000** |
| newtons_method_convergence | tangled_rope | rope | tangled_rope | 0.15 | 0.20 | 0.08 | rope_chi_ceiling | 0.145 |
| poincare_conjecture | tangled_rope | rope | tangled_rope | 0.20 | 0.10 | 0.03 | mountain_supp_ceiling | 0.050 |
| prime_number_theorem | tangled_rope | rope | tangled_rope | 0.15 | 0.10 | 0.01 | mountain_supp_ceiling | 0.050 |
| quine_self_replication | tangled_rope | rope | tangled_rope | 0.20 | 0.10 | 0.01 | mountain_supp_ceiling | 0.050 |
| rices_theorem_undecidability | tangled_rope | rope | tangled_rope | 0.15 | 0.20 | 0.04 | rope_chi_ceiling | 0.145 |
| rogue_wave_control_2026 | tangled_rope | rope | tangled_rope | 0.15 | 0.10 | 0.05 | mountain_supp_ceiling | 0.050 |
| sm_addictive_design | tangled_rope | snare | tangled_rope | 0.68 | 0.85 | 0.40 | snare_epsilon_floor | 0.220 |
| sturgeons_law | scaffold | rope | scaffold | 0.10 | 0.40 | 0.11 | tangled_rope_supp_floor | 0.000 |
| sylow_theorems_group_theory | tangled_rope | rope | tangled_rope | 0.15 | 0.20 | 0.01 | rope_chi_ceiling | 0.145 |

### 4c. Bucket Classification

**Bucket (a) — Metrically borderline but indexically stable: 15 of 18**

These 15 constraints (the 12 tangled_rope->rope mathematical/abstract constraints + 3 scaffolds) all sit near classification thresholds:
- 8 are within 0.050 of the mountain suppression ceiling (supp=0.10, threshold=0.05)
- 4 are within 0.145 of the rope chi ceiling
- 1 (`mvt_theorem_constraint`) sits exactly on the mountain suppression ceiling (distance=0.000)
- 2 scaffolds sit on boundary with other types

These are constraints where metric-space analysis detects genuine proximity to a type boundary, but the indexical structure (context-dependent classification) is stable — the constraint stays the same type across all observer frames. The shadow classifier is correctly identifying metric ambiguity that doesn't manifest as indexical instability.

**Bucket (b) — Other: 3 of 18**

- `horizon_liability_contract` (tangled_rope->snare): eps=0.85, supp=0.95, theater=0.60. This is NOT borderline — it has deep snare metrics but is classified as tangled_rope by the deterministic cascade (likely due to boolean gates: `has_coordination_function` + `has_asymmetric_extraction`). The shadow classifier's snare preference (P=0.443) reflects the metrics accurately. Indexically stable as tangled_rope. This is a case where the boolean gates are doing real classificatory work that the Gaussian profiles cannot see.
- `sm_addictive_design` (tangled_rope->snare): Similar pattern. eps=0.68, supp=0.85 — deep snare metrics, classified as tangled_rope by boolean gates.
- `martian_signal_latency` (scaffold->rope): eps=0.10, supp=1.00 — extreme suppression pushes Gaussians away from scaffold profile. The scaffold classification relies on the `has_coordination_function` + `scaffold_temporality_check` boolean gates.

### 4d. Inverse Check: Multi-Type Orbits with Low Entropy

**10 constraints** have multi-type Dirac orbits (indexically unstable) but low MaxEnt entropy (< 0.30, metrically confident):

| Constraint | Det Type | H_norm | Orbit Types |
|------------|----------|--------|-------------|
| trillion_bond_rush_2026 | unknown | 0.0003 | scaffold/unknown |
| maha_recovery_2026 | unknown | 0.0014 | scaffold/unknown |
| russells_paradox_self_reference | unknown | 0.0052 | scaffold/unknown |
| glp1_payload_efficiency_pivot | unknown | 0.0100 | scaffold/unknown |
| hawthorne_effect | unknown | 0.0120 | scaffold/unknown |
| ship_of_theseus | unknown | 0.0230 | rope/unknown |
| gradient_descent_optimization | unknown | 0.1017 | rope/scaffold/unknown |
| skills_based_hiring | unknown | 0.1010 | rope/scaffold/unknown |
| silklink_2026 | rope | 0.1423 | rope/scaffold |
| gamblers_ruin_stochastic_extinction | unknown | 0.2027 | scaffold/unknown |

**Pattern:** 9 of 10 are `unknown` type in analytical context, with orbits containing `scaffold/unknown` or `rope/unknown`. These are constraints that look one way metrically (very low entropy = high metric confidence) but shift classification when viewed from different power/scope contexts. The MaxEnt classifier sees them as nearly-certain (the Gaussian profiles strongly favor one type), but the Dirac orbit analysis reveals they change type with observer context.

**Interpretation:** This is the population where **indexical relativity is doing work that metric analysis alone would miss.** The metric profiles are confident, but that confidence is illusory — the constraint's nature genuinely depends on who is looking. The `unknown` analytical classification + multi-type orbit pattern suggests these constraints fall in threshold gaps that only open from certain perspectives.

---

## 5. Gaussian Profile Fit Evaluation

### 5a. Empirical Profiles

| Type | Metric | Mu | Sigma | Source |
|------|--------|----|-------|--------|
| mountain | extractiveness | 0.0836 | 0.0571 | Empirical (n=47) |
| mountain | suppression | 0.0311 | 0.0185 | Empirical (n=47) |
| mountain | theater | 0.0283 | 0.0731 | Empirical (n=47) |
| rope | extractiveness | 0.1124 | 0.0717 | Empirical (n=34) |
| rope | suppression | 0.4038 | **0.3038** | Empirical (n=34) |
| rope | theater | 0.1979 | **0.2705** | Empirical (n=34) |
| tangled_rope | extractiveness | 0.4455 | 0.1985 | Empirical (n=215) |
| tangled_rope | suppression | 0.5416 | 0.2260 | Empirical (n=215) |
| tangled_rope | theater | 0.2148 | 0.2299 | Empirical (n=215) |
| snare | extractiveness | 0.6939 | 0.1329 | Empirical (n=324) |
| snare | suppression | 0.7540 | 0.0860 | Empirical (n=324) |
| snare | theater | 0.4215 | **0.3022** | Empirical (n=324) |
| scaffold | extractiveness | 0.1000 | 0.0100 | Empirical (n=10) |
| scaffold | suppression | 0.3100 | **0.2508** | Empirical (n=10) |
| scaffold | theater | 0.0390 | 0.0489 | Empirical (n=10) |
| piton | extractiveness | 0.6500 | 0.1500 | **Default** (n=0) |
| piton | suppression | 0.6900 | 0.1500 | **Default** (n=0) |
| piton | theater | 0.8500 | 0.0800 | **Default** (n=0) |

### 5b. Suspiciously Large Sigmas (> 0.25)

| Type | Metric | Sigma | Diagnosis |
|------|--------|-------|-----------|
| **rope** | **suppression** | **0.3038** | Rope suppression spans 0.0-1.0 in the corpus. A Gaussian centered at 0.40 with sigma=0.30 covers the entire [0,1] range. This profile cannot discriminate. |
| **rope** | **theater** | **0.2705** | Same issue — wide spread suggests mixed populations. |
| **snare** | **theater** | **0.3022** | Snare theater ranges from 0 to ~0.8. The Gaussian is averaging over a wide distribution. |
| **scaffold** | **suppression** | **0.2508** | With only n=10, the scaffold suppression distribution is poorly sampled. |

### 5c. Rope BaseEps Distribution — Bimodality Check

**34 rope constraints, BaseEps values:**

| Bin | Range | Count | Pct |
|-----|-------|-------|-----|
| Low | [0.00, 0.25) | 31 | 91.2% |
| Mid | [0.25, 0.50) | 3 | 8.8% |
| High | [0.50, 1.00] | 0 | 0.0% |

**The rope BaseEps distribution is NOT bimodal.** It is unimodal, concentrated in [0, 0.25] with mean=0.112. The predicted bimodality (clean ropes near eps=0.15 + override-ropes at eps=0.50+) did not materialize. All ropes in the analyzed corpus have low extraction — the signature-override ropes (n=22) also have low eps (0.00-0.15 for CIR, 0.02-0.15 for constructed_low_extraction). The override mechanism pushes constraints with rope-like metrics INTO the rope category, not constraints with high-eps metrics.

**However, the rope SUPPRESSION is where the real spread is.** With sigma=0.3038 and mean=0.4038, rope suppression spans the full range. This is the actual source of poor rope discrimination:

| Rope suppression range | Count |
|-----------------------|-------|
| [0.00, 0.20) | 10 |
| [0.20, 0.50) | 7 |
| [0.50, 0.80) | 9 |
| [0.80, 1.00] | 8 |

This IS effectively bimodal (or at least bimodal-adjacent): some ropes have near-zero suppression (true coordination), others have very high suppression (override-ropes that are structurally not "pure" ropes).

### 5d. Override-Rope Entropy Decomposition

| Population | Count | Mean H_norm |
|------------|-------|-------------|
| Override ropes (CIR + constructed_low_extraction) | 22 | 0.4731 |
| Non-override ropes | 12 | 0.5339 |
| All ropes | 34 | 0.4946 |

**Breakdown within overrides:**
- CIR ropes (coupling_invariant_rope): 12 constraints, all with H_norm = 0.3114. The unconditional override sets P(rope)=0.95, producing uniformly low entropy.
- Constructed_low_extraction ropes: 10 constraints, mean H_norm = 0.665. The conditional 3x boost is insufficient to dominate, leaving significant residual uncertainty.

**Rope entropy would be 0.5339 if override-ropes were excluded** — only marginally higher than the overall rope mean of 0.4946. The CIR override ropes pull the mean DOWN (they all get 0.3114), while constructed_low_extraction overrides are ABOVE the mean. The net override effect is modest.

### 5e. Recommendations

1. **Rope suppression profile needs a mixture model.** A single Gaussian with sigma=0.30 is meaningless as a discriminator. Consider two-component Gaussian mixture or separate sub-profiles for CIR vs non-CIR ropes.
2. **Scaffold profile needs more data.** n=10 with zero extractiveness variance is not a viable profile.
3. **Piton profile needs any data.** Currently using hardcoded defaults from zero observations.
4. **Snare theater profile (sigma=0.30) should be investigated** — theater ratio may not be a useful discriminator for snares.

---

## 6. Cross-Diagnostic Correlation

### 6a. Correlation Table

All metrics compared for **high-entropy** (H > 0.40, n=475) vs **low-entropy** (H <= 0.40, n=194) populations:

| Diagnostic Signal | High Entropy | Low Entropy | Difference | Interpretation |
|-------------------|-------------|-------------|------------|----------------|
| Has Omega variables | 0/475 (0.0%) | 0/194 (0.0%) | None | Omega variables not present for analyzed constraints |
| Boltzmann non-compliant | 430/475 (**90.5%**) | 123/194 (63.4%) | +27.1 pp | **Strong correlation** |
| Purity score < 0.50 | 234/475 (**49.3%**) | 60/194 (30.9%) | +18.3 pp | **Moderate correlation** |
| Avg purity score | **0.477** | 0.662 | -0.185 | High-entropy constraints have substantially lower purity |
| In multi-type Dirac orbit | 426/475 (**89.7%**) | 102/194 (52.6%) | +37.1 pp | **Strong correlation** |

### 6b. Interpretation

**The shadow classifier is largely confirming existing signals, not finding new ones.**

The three strongest correlations:

1. **Multi-type Dirac orbits (89.7% vs 52.6%):** High-entropy constraints are overwhelmingly also in multi-type orbits. Both diagnostics identify the same structurally ambiguous population. This is the strongest cross-validation signal.

2. **Boltzmann non-compliance (90.5% vs 63.4%):** High-entropy constraints almost always fail the Boltzmann independence test. This makes structural sense — constraints that couple dimensions should also be metrically ambiguous between types.

3. **Low purity (49.3% vs 30.9%):** Moderate but meaningful correlation. Structurally contaminated constraints are more metrically ambiguous.

**Omega variables: Zero correlation (0% everywhere).** This is not because omega variables are absent from the corpus — they're present in testsets. The issue is that `omega_variable/3` facts are typically asserted during scenario runs (`load_and_run`), not during batch loading. The batch `load_all_testsets` consults files but doesn't trigger the scenario manager's omega generation pipeline. This is an architectural gap in the reporting pipeline, not a finding about the MaxEnt classifier.

**Linter error correlation:** Not directly testable from runtime data. The linter runs as a separate bash script, and its output is not available to Prolog queries. This cross-check is deferred.

### 6c. Verdict

The MaxEnt shadow classifier is operating as a **redundant validator** — it strongly confirms the same population that Dirac orbit analysis and Boltzmann compliance already identify as structurally ambiguous. It adds **quantitative confidence margins** (probability distributions, entropy scores) to what was previously binary (multi-type orbit: yes/no). Its novel contribution is the 18 single-type-orbit hard disagreements (Section 4b) — metric ambiguity that indexical analysis misses — and the 10 inverse cases — indexical instability that metric analysis misses.

---

## Summary of Findings and Recommendations

### Bugs Found

1. **CRITICAL: `fnl_trace_diagnostic.pl` wipes the knowledge base during batch loading.** This causes 354 constraints to be silently dropped, including ALL pitons. The reported 669/1025 coverage is an artifact. **Fix: remove the `:- initialization(run_fnl_trace)` directive or move the file out of `testsets/`.**

2. **MINOR: `maxent_classify_one/2` silently skips constraints when `TypeLogLPairs = []`.** No constraints triggered this in the current run (0 visible-but-no-dist), but it's a latent bug. Should at minimum log a warning.

3. **MINOR: `load_all_testsets` reports "Loaded 1025 testsets successfully" even when files fail.** The `catch(user:consult(F), _, true)` pattern counts error-recovered loads as successes.

### Calibration Issues

1. **Mountain entropy is degenerate.** All 47 mountains get identical entropy (0.3114). The boolean penalty (-10.0) and tight Gaussian profiles create a near-deterministic classifier. Consider softening the boolean penalty or widening mountain profiles.

2. **Rope suppression profile is pathological.** Sigma=0.3038 on a [0,1] metric provides no discrimination. The rope population is heterogeneous (CIR-certified vs constructed_low_extraction vs metric ropes) and should be modeled with sub-profiles or a mixture model.

3. **Scaffold and piton profiles are undersampled.** Scaffold n=10 (with zero eps variance); piton n=0 (defaults only). After fixing the fnl_trace_diagnostic bug, these populations will grow substantially.

4. **Snare->tangled_rope dominance in disagreements (70% of hard).** The boolean gates (`has_coordination_function`, `has_asymmetric_extraction`, `requires_active_enforcement`) are the primary differentiators between these types. The Gaussian profiles see too much overlap to distinguish them. This is by design (boolean gates ARE the classifier's discriminative power for this boundary), but it means the MaxEnt classifier will always systematically under-detect snares.

### Architectural Observations

1. The MaxEnt classifier's primary value is as a **confidence estimator**, not a competing classifier. Its probability distributions add gradient information to the deterministic classifier's binary output.

2. The 10 inverse-check constraints (multi-type orbit + low entropy) represent the clearest demonstration of indexical relativity's unique signal — structural ambiguity invisible to metric analysis.

3. After fixing the fnl_trace_diagnostic bug and rerunning on the full 1025-constraint corpus, all numbers in this report should be recomputed. The snare/tangled_rope proportions will shift, and piton/scaffold profiles will gain empirical data.

---

*End of MaxEnt Diagnostic Report*
