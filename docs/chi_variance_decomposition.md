# Chi Variance Decomposition Analysis

*Generated 2026-03-14 02:42 by `python/chi_variance_decomposition.py`*

---

## 1. Executive Summary

Analyzed **2221** tangled_rope constraints (2001 genuinely perspectival).

**Variance driver**: f(d) (power sigmoid). Of total f(d)+scope variance, f(d) accounts for 93.7% and scope for 6.3%. (Negative interaction means Var_fd + Var_scope > Var_total.)

**Sweep stability**: At σ(global)=1.0, GP fraction = 84.4% (baseline at σ=1.2: 90.1%).

GP stays above 80% across the full σ(global) sweep range.

Chi overrides detected: **1141** constraints (tolerance = 0.01).


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

### 2.2 Full Population (N=2221)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.078497 | 0.083879 | 0.026325 | 0.063405 | 0.099632 |
| var_fd | 0.077213 | 0.084416 | 0.029398 | 0.055466 | 0.100824 |
| var_scope | 0.005219 | 0.005343 | 0.001384 | 0.004399 | 0.005596 |
| var_interaction | -0.003935 | -0.005364 | 0.005526 | -0.006535 | -0.002883 |
| fd_fraction | 0.959515 | 1.011804 | 0.160334 | 0.990215 | 1.011966 |
| scope_fraction | 0.094742 | 0.053624 | 0.176781 | 0.053319 | 0.071144 |

### 2.3 Genuinely Perspectival Subset (N=2001)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.083425 | 0.089592 | 0.022472 | 0.073298 | 0.100200 |
| var_fd | 0.081956 | 0.089777 | 0.026680 | 0.073700 | 0.100824 |
| var_scope | 0.005451 | 0.005343 | 0.001047 | 0.005036 | 0.005675 |
| var_interaction | -0.003982 | -0.005798 | 0.005784 | -0.006535 | -0.004476 |
| fd_fraction | 0.955475 | 1.011449 | 0.166985 | 0.990215 | 1.011966 |
| scope_fraction | 0.078217 | 0.053624 | 0.060403 | 0.053319 | 0.071144 |

### 2.4 By Subtype

| Subtype | N | Mean fd_frac | Mean scope_frac | Mean interaction |
| :--- | ---: | ---: | ---: | ---: |
| genuinely_perspectival | 2001 | 0.9555 | 0.0782 | -0.003982 |
| structurally_ambiguous | 199 | 1.0017 | 0.1824 | -0.003347 |
| rope_dominant | 12 | 1.0414 | 0.0598 | -0.002514 |
| snare_dominant | 9 | 0.8167 | 1.8765 | -0.008422 |

### 2.5 Variance Share Distribution

Distribution of f(d) share = Var_fd / (Var_fd + Var_scope), which is always in [0, 1] and avoids the >1.0 artifact from negative interaction:

```
  0.0-0.1 | # (0)
  0.1-0.2 | # (0)
  0.2-0.3 | # (22)
  0.3-0.4 | # (25)
  0.4-0.5 | # (14)
  0.5-0.6 | # (19)
  0.6-0.7 | # (52)
  0.7-0.8 | # (34)
  0.8-0.9 | ##### (243)
  0.9-1.0 | ######################################## (1812)
```

f(d) share: mean=0.9093, median=0.9497, min=0.2450, max=0.9808

### 2.6 Dominance Classification

Mutually exclusive: classified by which source has the larger fraction (must also exceed 0.6).

| Category | Count | % |
| :--- | ---: | ---: |
| f(d)-dominated | 2100 | 94.6000 |
| scope-dominated | 18 | 0.8000 |
| balanced | 103 | 4.6000 |

### 2.7 Chi Overrides

1141 constraints have Chi values that differ from `ε × f(d) × σ(S)` by more than 0.01.

These are constraints where manual overrides or rounding effects produce non-multiplicative Chi. The variance decomposition uses actual Chi for Var_total but multiplicative formula for counterfactuals.

Top 10 by discrepancy (of 1141):

| Constraint | Max Discrepancy |
| :--- | ---: |
| climate_target_one_point_five | 0.664453 |
| battery_supply_chain_security | 0.579296 |
| credentialing_gatekeeping | 0.579296 |
| nem_transmission_bottleneck | 0.540605 |
| open_source_software_volunteer_model | 0.529342 |
| intellectual_property_lock_in | 0.495190 |
| labor_deskilling_dynamics | 0.495190 |
| maintenance_capacity_shortfall | 0.495190 |
| pharmaceutical_supply_chain_traceability | 0.495190 |
| publication_bias_psychology | 0.495190 |

## 3. Scope Modifier Sensitivity Sweep

### 3.1 Methodology

Sweep σ(global) from 1.0 to 1.5 (step 0.05), holding other scopes at their data values. Parallel sweep σ(local) from 0.5 to 1.0.

Global scope perspectives (from data): ['analytical']

Local scope perspectives (from data): ['powerless']

At each value: recompute χ = ε × f(d) × σ_swept, compute gradient, reclassify subtypes. **1141 Chi overrides are replaced** by the multiplicative formula during sweep.

### 3.2 Global Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1.00 | 188 | 34 | 1874 | 125 | 84.4 |
| 1.05 | 26 | 34 | 1912 | 249 | 86.1 |
| 1.10 | 23 | 34 | 1912 | 252 | 86.1 |
| 1.15 | 12 | 34 | 1912 | 263 | 86.1 |
| 1.20 | 12 | 34 | 1915 | 260 | 86.2 |
| 1.25 | 2 | 34 | 1916 | 269 | 86.3 |
| 1.30 | 2 | 34 | 1916 | 269 | 86.3 |
| 1.35 | 2 | 34 | 2080 | 105 | 93.7 |
| 1.40 | 2 | 34 | 2082 | 103 | 93.7 |
| 1.45 | 2 | 34 | 2092 | 93 | 94.2 |
| 1.50 | 2 | 34 | 2093 | 92 | 94.2 |

### 3.3 Local Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 0.50 | 12 | 0 | 2030 | 179 | 91.4 |
| 0.55 | 12 | 0 | 2024 | 185 | 91.1 |
| 0.60 | 12 | 0 | 1929 | 280 | 86.9 |
| 0.65 | 12 | 0 | 1916 | 293 | 86.3 |
| 0.70 | 12 | 3 | 1916 | 290 | 86.3 |
| 0.75 | 12 | 34 | 1916 | 259 | 86.3 |
| 0.80 | 12 | 34 | 1915 | 260 | 86.2 |
| 0.85 | 12 | 41 | 1915 | 253 | 86.2 |
| 0.90 | 12 | 41 | 1915 | 253 | 86.2 |
| 0.95 | 12 | 41 | 1915 | 253 | 86.2 |
| 1.00 | 12 | 41 | 1916 | 252 | 86.3 |

### 3.4 Phase Transitions

| Threshold | σ(global) at crossing | σ(local) at crossing |
| :--- | ---: | ---: |
| GP < 80% | never | never |
| GP < 70% | never | never |
| GP < 60% | never | never |
| GP < 50% | never | never |

**Snare growth > 5%**: global sweep = never, local sweep = never

**No single subtype > 50%**: global sweep = never, local sweep = never

**Max GP count change in one step**: global = 164 (σ 1.30→1.35), local = 95 (σ 0.55→0.60)

### 3.5 Stability Assessment

At σ(global)=1.0 (scope neutralized with national), GP = 84.4%. **The 88% genuinely perspectival finding is robust** — it is driven primarily by f(d) and structural properties, not scope amplification.


## 4. Dominant Divergence Pair Analysis

### 4.1 All Perspective Pairs (N=2001 genuinely perspectival)

| Pair | Mean |Δχ| | Median |Δχ| | Std |
| :--- | ---: | ---: | ---: |
| institutional-analytical | 0.727660 | 0.770695 | 0.144650 |
| moderate-institutional | 0.573703 | 0.613546 | 0.141895 |
| powerless-institutional | 0.405810 | 0.344175 | 0.183163 |
| powerless-analytical | 0.344690 | 0.441964 | 0.167097 |
| powerless-moderate | 0.196324 | 0.304976 | 0.161380 |
| moderate-analytical | 0.155050 | 0.152795 | 0.039016 |

**Dominant pair**: institutional-analytical

### 4.2 Institutional↔Analytical Decomposition

N = 2001

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | -0.727660 | -0.770695 | 0.144650 |
| delta_due_to_fd | -0.658526 | -0.706165 | 0.157117 |
| delta_due_to_scope | -0.069133 | -0.063763 | 0.014793 |
| delta_interaction | -0.000000 | 0.000000 | 0.000000 |

f(d) dominates in 1970/2001 = 98.5% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.3 Powerless↔Institutional Decomposition

N = 2001

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | 0.383420 | 0.344175 | 0.226317 |
| delta_due_to_fd | 0.649796 | 0.693425 | 0.129557 |
| delta_due_to_scope | -0.081467 | -0.076349 | 0.014924 |
| delta_interaction | -0.184909 | -0.294780 | 0.163551 |

f(d) dominates in 1989/2001 = 99.4% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.4 Counterfactual: Powerless at scope_mod=1.0

If powerless had scope_mod=1.0 instead of 0.8:

| Pair | Original Mean |Δχ| | Counterfactual Mean |Δχ| |
| :--- | ---: | ---: |
| institutional-analytical | 0.727660 | 0.727660 |
| powerless-institutional | 0.405810 | 0.721996 |
| moderate-institutional | 0.573703 | 0.573703 |
| moderate-analytical | 0.155050 | 0.155050 |
| powerless-moderate | 0.196324 | 0.148732 |
| powerless-analytical | 0.344690 | 0.007329 |

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

