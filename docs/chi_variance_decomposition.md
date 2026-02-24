# Chi Variance Decomposition Analysis

*Generated 2026-02-24 14:13 by `python/chi_variance_decomposition.py`*

---

## 1. Executive Summary

Analyzed **502** tangled_rope constraints (402 genuinely perspectival).

**Variance driver**: f(d) (power sigmoid). Of total f(d)+scope variance, f(d) accounts for 94.8% and scope for 5.2%. (Negative interaction means Var_fd + Var_scope > Var_total.)

**Sweep stability**: At σ(global)=1.0, GP fraction = 55.8% (baseline at σ=1.2: 80.1%).

GP drops below 80% at σ(global) = 1.0.

Chi overrides detected: **7** constraints (tolerance = 0.01).


Discovered scope mapping: {'powerless': 0.8, 'moderate': 1.0, 'institutional': 1.0, 'analytical': 1.2}


## 2. Variance Decomposition

### 2.1 Methodology

Chi for each constraint-perspective pair: `χ = ε × f(d) × σ(S)`

Counterfactual variants:

```
χ_full(C, U)       = ε(C) × f_d(C, U) × scope_mod(U)      -- actual
χ_fd_only(C, U)    = ε(C) × f_d(C, U) × 1.0               -- scope neutralized
χ_scope_only(C, U) = ε(C) × mean(f_d) × scope_mod(U)      -- f(d) neutralized
```

Var_total = Var(χ_full), Var_fd = Var(χ_fd_only), Var_scope = Var(χ_scope_only), Var_interaction = Var_total - Var_fd - Var_scope

**Note on negative interaction**: When f(d) and scope variations are anti-correlated across perspectives (high f(d) pairs with low scope, and vice versa), their product has less variance than the sum of individual variances. This makes Var_interaction negative and fd_fraction + scope_fraction > 1.0. This is expected, not an error — it means f(d) and scope partially cancel each other.

### 2.2 Full Population (N=502)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.084490 | 0.080084 | 0.047135 | 0.065424 | 0.089592 |
| var_fd | 0.085450 | 0.081043 | 0.047751 | 0.066207 | 0.090664 |
| var_scope | 0.004645 | 0.004294 | 0.002606 | 0.003659 | 0.004804 |
| var_interaction | -0.005605 | -0.005253 | 0.003191 | -0.005876 | -0.004291 |
| fd_fraction | 1.009481 | 1.011965 | 0.035554 | 1.011964 | 1.011966 |
| scope_fraction | 0.064945 | 0.053623 | 0.129229 | 0.053623 | 0.053623 |

### 2.3 Genuinely Perspectival Subset (N=402)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.097301 | 0.089592 | 0.043532 | 0.068238 | 0.089592 |
| var_fd | 0.098410 | 0.090664 | 0.044125 | 0.069054 | 0.090664 |
| var_scope | 0.005246 | 0.004804 | 0.002318 | 0.003659 | 0.004804 |
| var_interaction | -0.006355 | -0.005876 | 0.002965 | -0.005876 | -0.004476 |
| fd_fraction | 1.010058 | 1.011965 | 0.036629 | 1.011964 | 1.011966 |
| scope_fraction | 0.054466 | 0.053623 | 0.011177 | 0.053623 | 0.053623 |

### 2.4 By Subtype

| Subtype | N | Mean fd_frac | Mean scope_frac | Mean interaction |
| :--- | ---: | ---: | ---: | ---: |
| genuinely_perspectival | 402 | 1.0101 | 0.0545 | -0.006355 |
| structurally_ambiguous | 62 | 1.0089 | 0.0900 | -0.003027 |
| rope_dominant | 36 | 1.0131 | 0.0536 | -0.001179 |
| snare_dominant | 2 | 0.8468 | 1.5995 | -0.014523 |

### 2.5 Variance Share Distribution

Distribution of f(d) share = Var_fd / (Var_fd + Var_scope), which is always in [0, 1] and avoids the >1.0 artifact from negative interaction:

```
  0.0-0.1 | # (0)
  0.1-0.2 | # (0)
  0.2-0.3 | # (1)
  0.3-0.4 | # (1)
  0.4-0.5 | # (0)
  0.5-0.6 | # (3)
  0.6-0.7 | # (0)
  0.7-0.8 | # (0)
  0.8-0.9 | # (1)
  0.9-1.0 | ######################################## (496)
```

f(d) share: mean=0.9443, median=0.9497, min=0.2454, max=0.9585

### 2.6 Dominance Classification

Mutually exclusive: classified by which source has the larger fraction (must also exceed 0.6).

| Category | Count | % |
| :--- | ---: | ---: |
| f(d)-dominated | 499 | 99.4000 |
| scope-dominated | 2 | 0.4000 |
| balanced | 1 | 0.2000 |

### 2.7 Chi Overrides

7 constraints have Chi values that differ from `ε × f(d) × σ(S)` by more than 0.01.

These are constraints where manual overrides or rounding effects produce non-multiplicative Chi. The variance decomposition uses actual Chi for Var_total but multiplicative formula for counterfactuals.

| Constraint | Max Discrepancy |
| :--- | ---: |
| trojan_war_spoils | 0.566885 |
| google_universal_commerce_protocol | 0.294780 |
| canada_germany_ai_pact | 0.272105 |
| eu_mercosur_trade_agreement | 0.272105 |
| indo_german_defense_pact | 0.272105 |
| polar_bear_biobanking | 0.272105 |
| shield_east_fortification | 0.272105 |

## 3. Scope Modifier Sensitivity Sweep

### 3.1 Methodology

Sweep σ(global) from 1.0 to 1.5 (step 0.05), holding other scopes at their data values. Parallel sweep σ(local) from 0.5 to 1.0.

Global scope perspectives (from data): ['analytical']

Local scope perspectives (from data): ['powerless']

At each value: recompute χ = ε × f(d) × σ_swept, compute gradient, reclassify subtypes. **7 Chi overrides are replaced** by the multiplicative formula during sweep.

### 3.2 Global Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1.00 | 57 | 2 | 280 | 163 | 55.8 |
| 1.05 | 54 | 2 | 376 | 70 | 74.9 |
| 1.10 | 54 | 2 | 381 | 65 | 75.9 |
| 1.15 | 36 | 2 | 391 | 73 | 77.9 |
| 1.20 | 36 | 2 | 401 | 63 | 79.9 |
| 1.25 | 33 | 2 | 440 | 27 | 87.6 |
| 1.30 | 22 | 2 | 440 | 38 | 87.6 |
| 1.35 | 22 | 2 | 443 | 35 | 88.2 |
| 1.40 | 20 | 2 | 443 | 37 | 88.2 |
| 1.45 | 20 | 2 | 461 | 19 | 91.8 |
| 1.50 | 20 | 2 | 461 | 19 | 91.8 |

### 3.3 Local Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 0.50 | 36 | 1 | 404 | 61 | 80.5 |
| 0.55 | 36 | 1 | 404 | 61 | 80.5 |
| 0.60 | 36 | 1 | 402 | 63 | 80.1 |
| 0.65 | 36 | 2 | 401 | 63 | 79.9 |
| 0.70 | 36 | 2 | 401 | 63 | 79.9 |
| 0.75 | 36 | 2 | 401 | 63 | 79.9 |
| 0.80 | 36 | 2 | 401 | 63 | 79.9 |
| 0.85 | 36 | 2 | 401 | 63 | 79.9 |
| 0.90 | 36 | 2 | 401 | 63 | 79.9 |
| 0.95 | 36 | 2 | 401 | 63 | 79.9 |
| 1.00 | 36 | 2 | 402 | 62 | 80.1 |

### 3.4 Phase Transitions

| Threshold | σ(global) at crossing | σ(local) at crossing |
| :--- | ---: | ---: |
| GP < 80% | 1.00 | 0.65 |
| GP < 70% | 1.00 | never |
| GP < 60% | 1.00 | never |
| GP < 50% | never | never |

**Snare growth > 5%**: global sweep = never, local sweep = never

**No single subtype > 50%**: global sweep = never, local sweep = never

**Max GP count change in one step**: global = 96 (σ 1.00→1.05), local = 2 (σ 0.55→0.60)

### 3.5 Stability Assessment

At σ(global)=1.0, GP = 55.8%. The finding is **moderately scope-dependent** — GP remains a majority but scope amplification contributes meaningfully.


## 4. Dominant Divergence Pair Analysis

### 4.1 All Perspective Pairs (N=402 genuinely perspectival)

| Pair | Mean |Δχ| | Median |Δχ| | Std |
| :--- | ---: | ---: | ---: |
| institutional-analytical | 0.793446 | 0.776701 | 0.159477 |
| moderate-institutional | 0.644658 | 0.631810 | 0.131141 |
| powerless-institutional | 0.629411 | 0.621026 | 0.131364 |
| powerless-analytical | 0.165012 | 0.155675 | 0.055298 |
| moderate-analytical | 0.148788 | 0.144891 | 0.030612 |
| powerless-moderate | 0.017073 | 0.010784 | 0.044539 |

**Dominant pair**: institutional-analytical

### 4.2 Institutional↔Analytical Decomposition

N = 402

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | -0.793446 | -0.776701 | 0.159477 |
| delta_due_to_fd | -0.731276 | -0.716236 | 0.148108 |
| delta_due_to_scope | -0.062170 | -0.060465 | 0.012383 |
| delta_interaction | 0.000000 | 0.000000 | 0.000000 |

f(d) dominates in 402/402 = 100.0% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.3 Powerless↔Institutional Decomposition

N = 402

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | 0.628434 | 0.621026 | 0.135961 |
| delta_due_to_fd | 0.708359 | 0.693425 | 0.142418 |
| delta_due_to_scope | -0.074397 | -0.072399 | 0.014735 |
| delta_interaction | -0.005527 | 0.000000 | 0.043657 |

f(d) dominates in 402/402 = 100.0% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.4 Counterfactual: Powerless at scope_mod=1.0

If powerless had scope_mod=1.0 instead of 0.8:

| Pair | Original Mean |Δχ| | Counterfactual Mean |Δχ| |
| :--- | ---: | ---: |
| institutional-analytical | 0.793446 | 0.793446 |
| powerless-institutional | 0.629411 | 0.787065 |
| moderate-institutional | 0.644658 | 0.644658 |
| moderate-analytical | 0.148788 | 0.148788 |
| powerless-moderate | 0.017073 | 0.142407 |
| powerless-analytical | 0.165012 | 0.006381 |

**Counterfactual dominant pair**: institutional-analytical

The dominant pair **does not change** under the counterfactual. institutional-analytical remains dominant even without scope penalty on powerless.


## 5. Calibration Implications

Based on the three analyses above:

1. **Variance driver**: See Section 2 for f(d) vs scope decomposition

2. **Sweep stability**: See Section 3.5 for robustness assessment

3. **Pair structure**: See Section 4 for what drives institutional↔analytical dominance


## 6. Data Sources

- `outputs/enriched_pipeline.json` — perspective_chi components

- `outputs/tangled_gradient_data.json` — subtype classifications

- `prolog/config.pl` — scope modifiers and gradient boundaries

- `docs/tangled_gradient_analysis.md` — gradient analysis background

