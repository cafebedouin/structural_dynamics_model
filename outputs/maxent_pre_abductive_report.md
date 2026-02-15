# MaxEnt Pre-Abductive Cleanup Report

*Generated: 2026-02-14 — Pre-abductive analysis of MaxEnt shadow classifier anomalies*

---

## Executive Summary

Three anomalies were investigated in the MaxEnt shadow classifier prior to abductive cleanup:

1. **Missing pitons**: Pipeline dashboard reports 90 pitons; MaxEnt shows 1 in shadow top-type. Root cause: `claimed_type` vs `dr_type` divergence under default context amplification. **Design decision**, not a code fix.

2. **Mountain boolean penalty**: All 71 mountains had identical H_norm=0.3114. Investigation uncovered a **critical bug** in `boolean_log_likelihood/3` (double-clause backtracking producing 12 distribution entries instead of 6). Bug fixed; penalty recalibrated from -10.0 to -4.0. **Corpus-wide entropy collapsed from 0.5319 to 0.1931**. Mountains remain identical at H_norm=0.1557 due to unconditional signature override.

3. **Metric-only ambiguity**: 45 single-type-orbit hard disagreements characterized. All have structural signatures (34 `false_ci_rope`, 11 `constructed_low_extraction`). Dominant pattern: `tangled_rope`->`rope` flip (31 of 45). No genuinely undiagnosed ambiguity — all cases explained by signature override mechanics.

### Impact Summary

| Metric | Before | After | Delta |
|--------|--------|-------|-------|
| Mean H_norm | 0.5319 | 0.1931 | -63.7% |
| High uncertainty (H > 0.40) | 741 (72.6%) | 12 (1.2%) | -98.4% |
| Hard disagreements | 150 | 151 | +1 |
| Soft disagreements | 630 | 2 | -99.7% |
| Test suite | 1025/1025 | 1025/1025 | pass |

---

## Task 1: Missing Pitons Investigation

### Problem Statement

The pipeline dashboard (via `scripts/run_full_pipeline.sh:376-392`) reports **piton=90** in the corpus composition. The MaxEnt shadow classifier reports **piton=0** as shadow top-type (now 1 post-fix). Where are the 90 pitons?

### Root Cause

The pipeline dashboard and the MaxEnt classifier use **different counting mechanisms**:

| Source | Mechanism | Count | Location |
|--------|-----------|-------|----------|
| Pipeline dashboard | `claimed_type` from `constraint_claim/2` in testset files, extracted via `python/extract_corpus_data.py` into `corpus_data.json` | 90 | `scripts/run_full_pipeline.sh:376` |
| MaxEnt classifier | `dr_type/3` deterministic classification under default context | 0 (now 1) | `prolog/maxent_classifier.pl:580-590` |

### Mechanism Trace

**Dashboard path**: Each testset file declares `constraint_claim(C, piton)`. The Python extractor counts these verbatim. This is the *author's intent* — what the constraint was designed to be.

**MaxEnt path**: The classifier calls `dr_type(C, Context, Type)` which runs the deterministic `classify_from_metrics/6` cascade. For piton classification:

```
Piton requires: Chi ≤ piton_extraction_ceiling (0.25)
Where Chi = BaseEps × power_modifier × scope_modifier
```

**Default context**: `context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))`
- `power_modifier(analytical) = 1.15`
- `scope_modifier(global) = 1.20`
- Combined amplification: **1.38×**

**Piton-claimed constraints** have BaseEps in range **0.48–0.89**. After amplification:
- Chi range: 0.48 × 1.38 = **0.66** to 0.89 × 1.38 = **1.23**
- All far exceed the 0.25 ceiling
- All 90 piton-claimed constraints get classified as snare/tangled_rope/unknown by `dr_type`

### Conclusion

This is a **design decision**, not a bug. The deterministic classifier is context-sensitive by design. In the default (analytical/civilizational/global) context, the piton extraction ceiling is unreachable for constraints with BaseEps > 0.18. The dashboard's `claimed_type` reflects the constraint author's structural intent; `dr_type` reflects the context-projected classification.

### Options for Resolution

| Option | Description | Tradeoff |
|--------|-------------|----------|
| **(A) Use claimed_type for MaxEnt profiling** | Replace `dr_type` with `claimed_type` in `maxent_precompute/2` for computing priors and profiles | Loses context-sensitivity; profiles reflect author intent rather than structural reality |
| **(B) Hybrid fallback** | Use `dr_type` when available, fall back to `claimed_type` when `dr_type` returns unknown | Reasonable compromise; still context-dependent for non-piton types |
| **(C) Adjust deterministic thresholds** | Raise `piton_extraction_ceiling` or reduce context amplification | Would reclassify non-piton constraints as pitons; affects entire cascade |
| **(D) Multi-context MaxEnt** | Run MaxEnt across multiple contexts and report the one that maximizes piton recovery | Computationally expensive; philosophically correct |

**Recommendation**: Option (B) for immediate use, Option (D) for future architecture.

---

## Task 2: Mountain Boolean Penalty Calibration

### Problem Statement

All 71 mountain-claimed constraints had identical H_norm=0.3114 (pre-fix) / 0.1557 (post-fix). The `-10.0` boolean penalty was suspected of overwhelming metric variation.

### Critical Bug Discovery

**Location**: `prolog/maxent_classifier.pl:179-190`

**Bug**: `boolean_log_likelihood/3` had two clauses:

```prolog
% Clause 1: always succeeds (findall + sum_list always produce a result)
boolean_log_likelihood(C, Type, LogL) :-
    config:param(maxent_boolean_penalty, Penalty),
    config:param(maxent_boolean_bonus, Bonus),
    findall(LL, (...), LLs),
    sum_list(LLs, LogL).

% Clause 2: intended fallback, but reachable via backtracking
boolean_log_likelihood(_, _, 0.0).
```

**Mechanism**: Within the outer `findall` in `maxent_classify_one/2`, Prolog's backtracking explored **both clauses** for each type. Clause 1 always succeeds (findall+sum_list never fails), so clause 2 was dead in forward execution — but within a findall context, backtracking tries clause 2 after clause 1 succeeds, producing a **second solution** with `LogL=0.0`.

**Impact**: Every type got **2 entries** in the distribution instead of 1. With 6 types × 2 = 12 entries, the softmax normalization produced inflated entropy because each type's mass was split between its true log-likelihood and a 0.0 fallback.

**Corpus-wide corruption**:
- Mean entropy inflated ~2.7× (0.1931 → 0.5319)
- 741 of 1021 constraints falsely flagged as high-uncertainty
- 630 false soft disagreements

### Fix Applied

```prolog
boolean_log_likelihood(C, Type, LogL) :-
    config:param(maxent_boolean_penalty, Penalty),
    config:param(maxent_boolean_bonus, Bonus),
    findall(LL, (...), LLs),
    sum_list(LLs, LogL),
    !.                                    % <-- CUT added
boolean_log_likelihood(_, _, 0.0).        % Only reached if clause 1 fails
```

**File**: `prolog/maxent_classifier.pl:189` — added cut (`!`) after `sum_list(LLs, LogL)`.

### Penalty Calibration

With the bug fixed, the penalty was analytically recalibrated:

**Goal**: A single violated boolean gate should be *dominant* (overriding metric evidence) but not *annihilating* (preserving some metric modulation for constraints near decision boundaries).

**Analysis**:
- Gaussian log-likelihood range for continuous metrics: approximately ±2 to ±4 nats depending on distance from mean
- At penalty = -10.0: a single violation contributes -10 nats, completely drowning out all metric variation (max ~4 nats)
- At penalty = -4.0: a single violation contributes -4 nats, comparable to the metric range, preserving the intended boolean dominance while allowing boundary modulation

| Penalty | Violated type suppression | Metric modulation preserved | Assessment |
|---------|--------------------------|----------------------------|------------|
| -3.0 | Moderate — ~20× suppression | High | Too permissive; booleans lose gate function |
| **-4.0** | **Strong — ~55× suppression** | **Moderate** | **Optimal balance** |
| -5.0 | Very strong — ~150× suppression | Low | Approaching annihilation |
| -6.0 | Extreme — ~400× suppression | Minimal | Near-identical to -10.0 in practice |

**Config change**: `prolog/config.pl:409` — `param(maxent_boolean_penalty, -4.0).` (was `-10.0`)

### Mountain Entropy: Residual Identical Values

Post-fix, all 76 mountains have H_norm = **0.155706** (identical).

**Root cause**: All 76 mountains have the `natural_law` structural signature, triggering the **unconditional override** in `maxent_classifier.pl:295-310`:

```prolog
override_unconditional(mountain, _DistIn, DistOut) :-
    config:param(maxent_signature_override_strength, Strength),  % 0.95
    Remainder is (1.0 - Strength) / 5.0,                        % 0.01 each
    DistOut = [mountain-Strength, rope-Remainder, tangled_rope-Remainder,
               snare-Remainder, scaffold-Remainder, piton-Remainder].
```

This **replaces** the metric-based distribution entirely with P(mountain)=0.95, P(others)=0.01. The penalty calibration correctly affects the pre-override distribution, but the override masks all variation.

**Resolution path**: To allow metric variation in mountain entropy, either:
- Reduce `maxent_signature_override_strength` from 0.95 to ~0.70 (config change, no predicate logic)
- Switch mountains from unconditional to conditional override (predicate logic change)

This was not pursued in this cleanup as it requires a design decision about how strongly natural_law signatures should dominate.

### Before/After Comparison

| Metric | Before (bug + -10.0 penalty) | After (fix + -4.0 penalty) |
|--------|-----|------|
| Mean H_norm | 0.5319 | 0.1931 |
| Median H_norm | 0.5255 | 0.1557 |
| High uncertainty | 741 (72.6%) | 12 (1.2%) |
| Hard disagreements | 150 | 151 |
| Soft disagreements | 630 | 2 |
| Mountain H_norm (all identical) | 0.3114 | 0.1557 |
| Test suite | 1025/1025 | 1025/1025 |

---

## Task 3: Metric-Only Ambiguity Characterization

### Problem Statement

Of the 151 hard disagreements (where shadow top-type ≠ deterministic type), 106 overlap with multi-type Dirac orbits (gauge-variant — explained by indexical relativity). The remaining **45** are "metric-only ambiguity": single-type Dirac orbits where the disagreement comes purely from the probabilistic metric model, not from gauge variance.

### Signature Breakdown

| Signature | Count | Description |
|-----------|-------|-------------|
| `false_ci_rope` | 34 | Conditional override: 3× boost to tangled_rope |
| `constructed_low_extraction` | 11 | Conditional override: 3× boost to rope |
| **Total** | **45** | All have structural signatures; none are signature-free |

### Type Transition Patterns

| Det Type → Shadow Top | Count | Signature |
|-----------------------|-------|-----------|
| tangled_rope → rope | 31 | false_ci_rope (29), constructed_low_extraction (2) |
| scaffold → rope | 7 | constructed_low_extraction (7) |
| rope → scaffold | 4 | constructed_low_extraction (4) |
| tangled_rope → snare | 2 | false_ci_rope (2) |
| tangled_rope → scaffold | 1 | false_ci_rope (1) |

The dominant pattern (31/45) is tangled_rope → rope flips driven by the `false_ci_rope` conditional override.

### Complete Constraint Table

| # | Constraint | Det | Shadow | H_norm | Conf | Eps | Supp | Theater | Signature | Category |
|---|-----------|-----|--------|--------|------|-----|------|---------|-----------|----------|
| 1 | astm_d638_tensile_testing | rope | scaffold | 0.4608 | 0.5392 | 0.10 | 0.60 | 0.00 | constructed_low_extraction | threshold-straddler |
| 2 | basel_problem_convergence | tangled_rope | rope | 0.3721 | 0.6278 | 0.20 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 3 | berkshire_compounding_culture | rope | scaffold | 0.3713 | 0.6287 | 0.10 | 0.20 | 0.00 | constructed_low_extraction | threshold-straddler |
| 4 | biological_curiosity | tangled_rope | rope | 0.0216 | 0.9784 | 0.15 | 0.20 | 0.00 | false_ci_rope | profile-mismatch |
| 5 | brouwer_fixed_point | tangled_rope | rope | 0.3721 | 0.6278 | 0.20 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 6 | buffons_needle_pi_estimation | tangled_rope | rope | 0.3721 | 0.6278 | 0.20 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 7 | burden_of_proof_legal_criminal | rope | scaffold | 0.3713 | 0.6287 | 0.10 | 0.20 | 0.00 | constructed_low_extraction | threshold-straddler |
| 8 | church_turing_thesis | scaffold | rope | 0.3878 | 0.6122 | 0.10 | 0.20 | 0.00 | constructed_low_extraction | threshold-straddler |
| 9 | coffee_cardiovascular_2026 | tangled_rope | rope | 0.4086 | 0.5914 | 0.12 | 0.40 | 0.10 | false_ci_rope | threshold-straddler |
| 10 | cognitive_bicycle_scaffold | tangled_rope | rope | 0.1586 | 0.8414 | 0.20 | 0.30 | 0.15 | false_ci_rope | profile-mismatch |
| 11 | collatz_conjecture_determinism | tangled_rope | rope | 0.3192 | 0.6808 | 0.15 | 0.20 | 0.00 | false_ci_rope | profile-mismatch |
| 12 | continuum_hypothesis_undecidability | tangled_rope | rope | 0.3423 | 0.6577 | 0.15 | 0.25 | 0.00 | false_ci_rope | profile-mismatch |
| 13 | creative_commons_licensing | rope | scaffold | 0.3528 | 0.6472 | 0.10 | 0.10 | 0.00 | constructed_low_extraction | threshold-straddler |
| 14 | currys_paradox | tangled_rope | rope | 0.1253 | 0.8747 | 0.20 | 0.50 | 0.15 | false_ci_rope | profile-mismatch |
| 15 | dexy_gold_protocol | tangled_rope | rope | 0.0556 | 0.9444 | 0.20 | 0.20 | 0.11 | false_ci_rope | profile-mismatch |
| 16 | ergo_lets_protocol | tangled_rope | rope | 0.0228 | 0.9772 | 0.15 | 0.10 | 0.15 | false_ci_rope | profile-mismatch |
| 17 | fmt_oncology_2026 | tangled_rope | rope | 0.1569 | 0.8431 | 0.18 | 0.45 | 0.12 | false_ci_rope | profile-mismatch |
| 18 | fundamental_theorem_of_algebra | scaffold | rope | 0.3878 | 0.6122 | 0.10 | 0.20 | 0.00 | constructed_low_extraction | threshold-straddler |
| 19 | galois_theory_symmetry | tangled_rope | rope | 0.3721 | 0.6278 | 0.20 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 20 | goldbach_conjecture | tangled_rope | rope | 0.2615 | 0.7385 | 0.15 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 21 | hilberts_hotel_infinity | tangled_rope | rope | 0.2615 | 0.7385 | 0.15 | 0.10 | 0.00 | false_ci_rope | profile-mismatch |
| 22 | horizon_liability_contract | tangled_rope | snare | 0.1535 | 0.8465 | 0.85 | 0.95 | 0.60 | false_ci_rope | boolean-override |
| 23 | indexical_relativity_core | scaffold | rope | 0.1643 | 0.8357 | 0.10 | 0.20 | 0.15 | constructed_low_extraction | profile-mismatch |
| 24 | information_foraging_theory | tangled_rope | rope | 0.0486 | 0.9514 | 0.20 | 0.10 | 0.08 | false_ci_rope | profile-mismatch |
| 25 | kleene_recursion_theorem | tangled_rope | rope | 0.3838 | 0.6162 | 0.20 | 0.15 | 0.01 | false_ci_rope | threshold-straddler |
| 26 | liar_paradox | scaffold | rope | 0.3878 | 0.6122 | 0.10 | 0.20 | 0.00 | constructed_low_extraction | threshold-straddler |
| 27 | litany_of_the_real | tangled_rope | rope | 0.3283 | 0.6717 | 0.15 | 0.20 | 0.08 | false_ci_rope | profile-mismatch |
| 28 | manganese_catalysis_2026 | tangled_rope | rope | 0.0938 | 0.9062 | 0.18 | 0.25 | 0.08 | false_ci_rope | profile-mismatch |
| 29 | martian_signal_latency | scaffold | rope | 0.3879 | 0.6121 | 0.10 | 1.00 | 0.02 | constructed_low_extraction | threshold-straddler |
| 30 | micro_robot_electronics_integration | tangled_rope | rope | 0.1503 | 0.8497 | 0.20 | 0.60 | 0.15 | false_ci_rope | profile-mismatch |
| 31 | midnight_deadline | scaffold | rope | 0.2708 | 0.7292 | 0.10 | 0.90 | 0.12 | constructed_low_extraction | profile-mismatch |
| 32 | mvt_theorem_constraint | tangled_rope | scaffold | 0.4191 | 0.5809 | 0.10 | 0.05 | 0.10 | false_ci_rope | threshold-straddler |
| 33 | narrative_engineering_2026 | tangled_rope | rope | 0.1011 | 0.8989 | 0.15 | 0.45 | 0.05 | false_ci_rope | profile-mismatch |
| 34 | newtons_method_convergence | tangled_rope | rope | 0.0231 | 0.9769 | 0.15 | 0.20 | 0.08 | false_ci_rope | profile-mismatch |
| 35 | poincare_conjecture | tangled_rope | rope | 0.3740 | 0.6260 | 0.20 | 0.10 | 0.03 | false_ci_rope | profile-mismatch |
| 36 | prime_number_theorem | tangled_rope | rope | 0.2629 | 0.7371 | 0.15 | 0.10 | 0.01 | false_ci_rope | profile-mismatch |
| 37 | quine_self_replication | tangled_rope | rope | 0.3728 | 0.6272 | 0.20 | 0.10 | 0.01 | false_ci_rope | profile-mismatch |
| 38 | rices_theorem_undecidability | tangled_rope | rope | 0.3239 | 0.6761 | 0.15 | 0.20 | 0.04 | false_ci_rope | profile-mismatch |
| 39 | rogue_wave_control_2026 | tangled_rope | rope | 0.3711 | 0.6289 | 0.15 | 0.10 | 0.05 | false_ci_rope | profile-mismatch |
| 40 | rope_design | tangled_rope | rope | 0.0783 | 0.9217 | 0.20 | 0.30 | 0.14 | false_ci_rope | profile-mismatch |
| 41 | skolems_paradox | tangled_rope | rope | 0.1043 | 0.8957 | 0.20 | 0.40 | 0.18 | false_ci_rope | profile-mismatch |
| 42 | sm_addictive_design | tangled_rope | snare | 0.2103 | 0.7897 | 0.68 | 0.85 | 0.40 | false_ci_rope | boolean-override |
| 43 | solar_system_weirdness | tangled_rope | rope | 0.1429 | 0.8571 | 0.20 | 0.60 | 0.08 | false_ci_rope | profile-mismatch |
| 44 | sturgeons_law | scaffold | rope | 0.3355 | 0.6645 | 0.10 | 0.40 | 0.11 | constructed_low_extraction | threshold-straddler |
| 45 | sylow_theorems_group_theory | tangled_rope | rope | 0.3204 | 0.6796 | 0.15 | 0.20 | 0.01 | false_ci_rope | profile-mismatch |

### Category Definitions and Counts

| Category | Count | Definition |
|----------|-------|------------|
| **threshold-straddler** | 12 | H_norm > 0.33 AND the constraint sits near a deterministic classification boundary (supp near 0.05, eps near 0.25, or rope/scaffold overlap zone) |
| **profile-mismatch** | 31 | Metrics match one type's Gaussian profile better than the deterministic type's profile, but the signature override pushes the shadow toward a third type |
| **boolean-override** | 2 | Extreme metrics (eps > 0.65) combined with boolean features that favor the shadow type over the deterministic type |

#### Threshold-Straddlers (12)

These have H_norm > 0.33, indicating genuine metric-space uncertainty. The shadow classifier's probability mass is spread across multiple types because the constraint's metrics sit in an overlap region.

Notable: `astm_d638_tensile_testing` (H=0.46, highest in group), `coffee_cardiovascular_2026` (H=0.41), `mvt_theorem_constraint` (H=0.42).

Scaffold ↔ rope transitions (`church_turing_thesis`, `fundamental_theorem_of_algebra`, `liar_paradox`, `berkshire_compounding_culture`, `burden_of_proof_legal_criminal`, `creative_commons_licensing`, `martian_signal_latency`, `sturgeons_law`) all have eps=0.10, supp=0.10-1.00 — firmly in the low-extraction zone where scaffold and rope profiles overlap.

#### Profile-Mismatches (31)

These have moderate-to-low entropy but the shadow top-type differs from det type. The signature conditional override (3× boost to rope via `false_ci_rope`, or 3× boost to rope via `constructed_low_extraction`) shifts the probability peak away from the deterministic classification.

The dominant pattern: constraints classified as `tangled_rope` by `dr_type` but with low extractiveness (eps ≤ 0.20) and low suppression (supp ≤ 0.30), where the Gaussian likelihood for `rope` is actually higher than for `tangled_rope`. The `false_ci_rope` signature then further boosts `rope` by 3×, creating a decisive flip.

Many are mathematical/logical constraints (`brouwer_fixed_point`, `goldbach_conjecture`, `poincare_conjecture`, etc.) with eps=0.15-0.20, supp=0.10 — characteristic of low-extraction, low-enforcement structural patterns.

#### Boolean-Overrides (2)

| Constraint | Eps | Supp | Theater | Det | Shadow | Notes |
|-----------|-----|------|---------|-----|--------|-------|
| horizon_liability_contract | 0.85 | 0.95 | 0.60 | tangled_rope | snare | Extreme extraction; det cascades to tangled_rope via signature, shadow favors snare via metric profile |
| sm_addictive_design | 0.68 | 0.85 | 0.40 | tangled_rope | snare | High extraction with coordination function; det and shadow disagree on rope vs snare emphasis |

These are the most structurally interesting cases: constraints with extreme metrics that push the Gaussian likelihood strongly toward snare, but the deterministic cascade classifies them as tangled_rope due to signature-based routing.

### Interpretation

All 45 single-type-orbit hard disagreements are **explained by the interaction between signature overrides and Gaussian metric profiles**. There are no "mystery" disagreements:

1. **Conditional overrides dominate** (45/45 have signatures): The `false_ci_rope` signature applies a 3× conditional boost to tangled_rope, while `constructed_low_extraction` boosts rope 3×. These shifts are large enough to flip the shadow top-type relative to the deterministic classifier.

2. **Mathematical/logical constraints cluster together** (17/45): Low-extraction, low-suppression patterns where the deterministic cascade picks tangled_rope but the metric profile is actually closer to rope.

3. **No metric-only ambiguity without signatures**: Every single case has a structural signature driving the disagreement. The "metric-only" label is a misnomer — it's really "signature-override ambiguity in single-type orbits."

---

## Files Modified

| File | Line | Change |
|------|------|--------|
| `prolog/maxent_classifier.pl` | 189 | Added cut (`!`) after `sum_list(LLs, LogL)` in `boolean_log_likelihood/3` |
| `prolog/config.pl` | 409 | Changed `maxent_boolean_penalty` from `-10.0` to `-4.0` |

## Verification

- Test suite: **1025/1025 passing** after all changes
- No regressions in deterministic classifier output
- MaxEnt report regeneration confirms corrected statistics
