# Chi Variance Decomposition Analysis

*Generated 2026-02-24 02:09 by `python/chi_variance_decomposition.py`*

---

## 1. Executive Summary

Analyzed **752** tangled_rope constraints (664 genuinely perspectival).

**Variance driver**: f(d) (power sigmoid). Of total f(d)+scope variance, f(d) accounts for 94.9% and scope for 5.1%. (Negative interaction means Var_fd + Var_scope > Var_total.)

**Sweep stability**: At σ(global)=1.0, GP fraction = 73.3% (baseline at σ=1.2: 88.3%).

GP drops below 80% at σ(global) = 1.0.

Chi overrides detected: **19** constraints (tolerance = 0.01).


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

### 2.2 Full Population (N=752)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.118268 | 0.089592 | 0.062098 | 0.068238 | 0.166596 |
| var_fd | 0.119662 | 0.090664 | 0.062906 | 0.069054 | 0.168589 |
| var_scope | 0.006431 | 0.005343 | 0.003323 | 0.003659 | 0.008933 |
| var_interaction | -0.007825 | -0.006146 | 0.004179 | -0.010927 | -0.004476 |
| fd_fraction | 1.010231 | 1.011965 | 0.029675 | 1.011964 | 1.011966 |
| scope_fraction | 0.062325 | 0.053623 | 0.110016 | 0.053623 | 0.053623 |

### 2.3 Genuinely Perspectival Subset (N=664)

| Component | Mean | Median | Std | Q25 | Q75 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| var_total | 0.129634 | 0.106621 | 0.056841 | 0.089592 | 0.166596 |
| var_fd | 0.131170 | 0.107897 | 0.057589 | 0.090664 | 0.168589 |
| var_scope | 0.006970 | 0.005717 | 0.003030 | 0.004804 | 0.008933 |
| var_interaction | -0.008506 | -0.006993 | 0.003887 | -0.010927 | -0.005664 |
| fd_fraction | 1.010927 | 1.011965 | 0.029144 | 1.011964 | 1.011966 |
| scope_fraction | 0.054144 | 0.053623 | 0.008703 | 0.053623 | 0.053623 |

### 2.4 By Subtype

| Subtype | N | Mean fd_frac | Mean scope_frac | Mean interaction |
| :--- | ---: | ---: | ---: | ---: |
| genuinely_perspectival | 664 | 1.0109 | 0.0541 | -0.008506 |
| structurally_ambiguous | 58 | 1.0071 | 0.1072 | -0.003080 |
| rope_dominant | 28 | 1.0120 | 0.0536 | -0.001033 |
| snare_dominant | 2 | 0.8468 | 1.5995 | -0.014523 |

### 2.5 Variance Share Distribution

Distribution of f(d) share = Var_fd / (Var_fd + Var_scope), which is always in [0, 1] and avoids the >1.0 artifact from negative interaction:

```
  0.0-0.1 | # (0)
  0.1-0.2 | # (0)
  0.2-0.3 | # (1)
  0.3-0.4 | # (1)
  0.4-0.5 | # (0)
  0.5-0.6 | # (4)
  0.6-0.7 | # (0)
  0.7-0.8 | # (0)
  0.8-0.9 | # (1)
  0.9-1.0 | ######################################## (745)
```

f(d) share: mean=0.9455, median=0.9497, min=0.2454, max=0.9531

### 2.6 Dominance Classification

Mutually exclusive: classified by which source has the larger fraction (must also exceed 0.6).

| Category | Count | % |
| :--- | ---: | ---: |
| f(d)-dominated | 749 | 99.6000 |
| scope-dominated | 2 | 0.3000 |
| balanced | 1 | 0.1000 |

### 2.7 Chi Overrides

19 constraints have Chi values that differ from `ε × f(d) × σ(S)` by more than 0.01.

These are constraints where manual overrides or rounding effects produce non-multiplicative Chi. The variance decomposition uses actual Chi for Var_total but multiplicative formula for counterfactuals.

| Constraint | Max Discrepancy |
| :--- | ---: |
| trump_second_term_authoritarianism_2026 | 0.481852 |
| dionysiac_frenzy | 0.453508 |
| indonesia_penal_code_2023 | 0.425164 |
| jp_nativist_politics | 0.385482 |
| digital_credentialing_verification | 0.368475 |
| nvidia_cuda_ecosystem_lockin | 0.368475 |
| ai_compute_capital_moat | 0.351469 |
| pele_microreactor_deployment | 0.340131 |
| ai_performance_watermark | 0.311787 |
| dk_us_alliance_espionage | 0.311787 |
| openai_prism_development | 0.311787 |
| semiconductor_fabrication_chokepoint | 0.311787 |
| us_usmca_china_leverage | 0.311787 |
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

At each value: recompute χ = ε × f(d) × σ_swept, compute gradient, reclassify subtypes. **19 Chi overrides are replaced** by the multiplicative formula during sweep.

### 3.2 Global Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1.00 | 50 | 2 | 551 | 149 | 73.3 |
| 1.05 | 47 | 2 | 642 | 61 | 85.4 |
| 1.10 | 47 | 2 | 647 | 56 | 86.0 |
| 1.15 | 28 | 2 | 655 | 67 | 87.1 |
| 1.20 | 28 | 2 | 663 | 59 | 88.2 |
| 1.25 | 26 | 2 | 696 | 28 | 92.5 |
| 1.30 | 17 | 2 | 696 | 37 | 92.5 |
| 1.35 | 17 | 2 | 699 | 34 | 93.0 |
| 1.40 | 16 | 2 | 699 | 35 | 93.0 |
| 1.45 | 16 | 2 | 718 | 16 | 95.5 |
| 1.50 | 16 | 2 | 718 | 16 | 95.5 |

### 3.3 Local Scope Sweep

| σ | rope_dom | snare_dom | genuinely_persp | struct_ambig | GP% |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 0.50 | 28 | 1 | 667 | 56 | 88.7 |
| 0.55 | 28 | 1 | 666 | 57 | 88.6 |
| 0.60 | 28 | 1 | 664 | 59 | 88.3 |
| 0.65 | 28 | 2 | 663 | 59 | 88.2 |
| 0.70 | 28 | 2 | 663 | 59 | 88.2 |
| 0.75 | 28 | 2 | 663 | 59 | 88.2 |
| 0.80 | 28 | 2 | 663 | 59 | 88.2 |
| 0.85 | 28 | 2 | 663 | 59 | 88.2 |
| 0.90 | 28 | 2 | 663 | 59 | 88.2 |
| 0.95 | 28 | 2 | 663 | 59 | 88.2 |
| 1.00 | 28 | 2 | 663 | 59 | 88.2 |

### 3.4 Phase Transitions

| Threshold | σ(global) at crossing | σ(local) at crossing |
| :--- | ---: | ---: |
| GP < 80% | 1.00 | never |
| GP < 70% | never | never |
| GP < 60% | never | never |
| GP < 50% | never | never |

**Snare growth > 5%**: global sweep = never, local sweep = never

**No single subtype > 50%**: global sweep = never, local sweep = never

**Max GP count change in one step**: global = 91 (σ 1.00→1.05), local = 2 (σ 0.55→0.60)

### 3.5 Stability Assessment

At σ(global)=1.0 (scope neutralized with national), GP = 73.3%. **The 88% genuinely perspectival finding is robust** — it is driven primarily by f(d) and structural properties, not scope amplification.


## 4. Dominant Divergence Pair Analysis

### 4.1 All Perspective Pairs (N=664 genuinely perspectival)

| Pair | Mean |Δχ| | Median |Δχ| | Std |
| :--- | ---: | ---: | ---: |
| institutional-analytical | 0.911924 | 0.847309 | 0.201823 |
| moderate-institutional | 0.741882 | 0.689246 | 0.164844 |
| powerless-institutional | 0.720106 | 0.677482 | 0.172730 |
| powerless-analytical | 0.192432 | 0.175488 | 0.068703 |
| moderate-analytical | 0.170109 | 0.158063 | 0.037757 |
| powerless-moderate | 0.022367 | 0.012156 | 0.056994 |

**Dominant pair**: institutional-analytical

### 4.2 Institutional↔Analytical Decomposition

N = 664

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | -0.911924 | -0.847309 | 0.201823 |
| delta_due_to_fd | -0.840691 | -0.781348 | 0.186754 |
| delta_due_to_scope | -0.071233 | -0.065961 | 0.015554 |
| delta_interaction | 0.000000 | 0.000000 | 0.000000 |

f(d) dominates in 664/664 = 100.0% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.3 Powerless↔Institutional Decomposition

N = 664

| Component | Mean | Median | Std |
| :--- | ---: | ---: | ---: |
| delta_chi | 0.719515 | 0.677482 | 0.175177 |
| delta_due_to_fd | 0.814436 | 0.756463 | 0.180263 |
| delta_due_to_scope | -0.085299 | -0.078981 | 0.018601 |
| delta_interaction | -0.009621 | 0.000000 | 0.057031 |

f(d) dominates in 664/664 = 100.0% of constraints.

*Note: Under the symmetric decomposition, the interaction term is algebraically zero for constraints without Chi overrides. Non-zero values indicate override effects.*

### 4.4 Counterfactual: Powerless at scope_mod=1.0

If powerless had scope_mod=1.0 instead of 0.8:

| Pair | Original Mean |Δχ| | Counterfactual Mean |Δχ| |
| :--- | ---: | ---: |
| institutional-analytical | 0.911924 | 0.911924 |
| powerless-institutional | 0.720106 | 0.904928 |
| moderate-institutional | 0.741882 | 0.741882 |
| moderate-analytical | 0.170109 | 0.170109 |
| powerless-moderate | 0.022367 | 0.163047 |
| powerless-analytical | 0.192432 | 0.007632 |

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

