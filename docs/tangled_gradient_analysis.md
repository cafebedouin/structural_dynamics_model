# Tangled Gradient Analysis

**Generated:** 2026-02-24 13:53

---

## 1. Executive Summary

Analyzed **506** tangled_rope constraints (defined by `claimed_type == 'tangled_rope'`).

Each constraint's position on the rope–snare continuum is measured via a gradient vector computed per-perspective. The primary axis (Chi) varies by observer; epsilon and suppression are observer-invariant.

### Subtype distribution

| Subtype | Count | % | Interpretation |
| :--- | ---: | ---: | :--- |
| rope_dominant | 40 | 7.9% | Complicated rope — coordination primary |
| snare_dominant | 2 | 0.4% | Disguised snare — extraction primary |
| genuinely_perspectival | 402 | 79.4% | True tangled_rope — framework validation |
| structurally_ambiguous | 62 | 12.3% | Coordination and extraction inseparable |
| unclassifiable | 0 | 0.0% | Missing Chi data |
| **Total** | **506** | **100%** | |

## 2. Data Sources

- **Population**: `claimed_type == 'tangled_rope'` from `enriched_pipeline.json`
- **Chi values**: `perspective_chi` field (Prolog export, Part 2 of this audit arc)
- **Thresholds**: `prolog/config.pl` via `shared.loader.read_config()`

### Gradient boundaries

| Dimension | Rope ceiling | Snare floor | Gap |
| :--- | ---: | ---: | ---: |
| Chi | 0.35 | 0.66 | 0.31 |
| Epsilon | 0.45 | 0.46 | 0.01 |
| Suppression | 0.16 | 0.6 | 0.44 |

## 3. Gradient Decomposition

For each constraint C and perspective U_i:

```
g_chi(C, U_i) = (Chi(C, U_i) - rope_chi_ceiling) / (snare_chi_floor - rope_chi_ceiling)
g_epsilon(C)  = (epsilon(C) - rope_eps_ceiling) / (snare_eps_floor - rope_eps_ceiling)
g_suppression(C) = (supp(C) - rope_supp_ceiling) / (snare_supp_floor - rope_supp_ceiling)
```

Values are NOT clipped: g < 0.0 means below rope boundary; g > 1.0 means above snare boundary.

**Composite variants:**

1. **Chi-dominant** (0.6 / 0.2 / 0.2) — primary reporting scalar
2. **Chi-only** (g_chi alone) — most interpretable; only perspectival dimension with meaningful tangled zone
3. **Equal-weight** (1/3 each) — for comparison

Components are clamped to [-1, 2] before compositing to prevent g_epsilon from dominating (its denominator is only 0.01).

## 4. Subtype Classification

Based on clamped Chi-only gradient across 4 perspectives:

| Subtype | Criterion |
| :--- | :--- |
| rope_dominant | max(G_chi) < 0.30 |
| snare_dominant | min(G_chi) > 0.70 |
| genuinely_perspectival | min(G_chi) < 0.30 AND max(G_chi) > 0.70 |
| structurally_ambiguous | All else |

### rope_dominant exemplars (40 total)

- **`blackstone_carried_interest_taxation`**: Chi gradients = pow=-0.08, mod=-0.06, ins=-1.17, ana=0.20 → rope_dominant
- **`canada_goose_realignment_2026`**: Chi gradients = pow=-0.23, mod=-0.32, ins=-1.16, ana=-0.02 → rope_dominant
- **`china_africa_zero_tariff_2026`**: Chi gradients = pow=-0.01, mod=0.01, ins=-1.17, ana=0.29 → rope_dominant
- **`climate_target_one_point_five`**: Chi gradients = pow=-0.08, mod=-0.06, ins=-1.17, ana=0.20 → rope_dominant
- **`coffee_cardiovascular_2026`**: Chi gradients = pow=-0.71, mod=-0.70, ins=-1.15, ana=-0.60 → rope_dominant

### snare_dominant exemplars (2 total)

- **`horizon_liability_contract`**: Chi gradients = pow=1.85, mod=1.90, ins=1.33, ana=2.63 → snare_dominant
- **`sm_addictive_design`**: Chi gradients = pow=1.26, mod=1.30, ins=1.48, ana=1.88 → snare_dominant

### genuinely_perspectival exemplars (402 total)

- **`absorbing_markov_chain_trap`**: Chi gradients = pow=0.80, mod=0.83, ins=-1.20, ana=1.30 → genuinely_perspectival
- **`abstraction_leakage`**: Chi gradients = pow=1.47, mod=1.51, ins=-1.23, ana=2.14 → genuinely_perspectival
- **`academic_peer_review_gatekeeping`**: Chi gradients = pow=1.50, mod=1.55, ins=-1.23, ana=2.19 → genuinely_perspectival
- **`access_arbitrage`**: Chi gradients = pow=0.45, mod=0.48, ins=-1.19, ana=0.86 → genuinely_perspectival
- **`adverse_possession`**: Chi gradients = pow=1.15, mod=1.19, ins=-1.22, ana=1.74 → genuinely_perspectival

### structurally_ambiguous exemplars (62 total)

- **`advice_as_dangerous_gift`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`alzheimers_levetiracetam`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`asce_7_22_seismic_design`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`availability_heuristic`**: Chi gradients = pow=0.27, mod=0.30, ins=-1.18, ana=0.64 → structurally_ambiguous
- **`comitatus_bond`**: Chi gradients = pow=0.27, mod=0.30, ins=-1.18, ana=0.64 → structurally_ambiguous

## 5. Perspectival Test

### Chi variance by subtype

| Subtype | Count | Mean variance | Gradient flips |
| :--- | ---: | ---: | ---: |
| rope_dominant | 40 | 0.1660 | 0 |
| snare_dominant | 2 | 0.1364 | 0 |
| genuinely_perspectival | 402 | 1.0125 | 402 |
| structurally_ambiguous | 62 | 0.4425 | 43 |

### Most common maximum-divergence pairs

| Pair | Frequency | Mean divergence |
| :--- | ---: | ---: |
| institutional↔analytical | 503 | 2.3356 |
| powerless↔analytical | 2 | 1.0233 |

**Epsilon invariance:** 506/506 constraints have identical epsilon across all perspectives (confirmed).

## 6. Cross-References

### PSI band × subtype

| Subtype | genuinely_tangled | rope_leaning | snare_leaning |
| :--- | ---: | ---: | ---: |
| rope_dominant | 8 | 30 | 2 |
| snare_dominant | 0 | 0 | 2 |
| genuinely_perspectival | 19 | 13 | 370 |
| structurally_ambiguous | 13 | 17 | 32 |

### H1 band × subtype

| Subtype | H1=0 | H1=3 | H1=4 | H1=5 |
| :--- | ---: | ---: | ---: | ---: |
| rope_dominant | 25 | 5 | 10 | 0 |
| snare_dominant | 2 | 0 | 0 | 0 |
| genuinely_perspectival | 2 | 221 | 0 | 179 |
| structurally_ambiguous | 2 | 60 | 0 | 0 |

### Signature × subtype

| Subtype | false_ci_rope | constructed_high_extraction | false_natural_law | constructed_low_extraction |
| :--- | ---: | ---: | ---: | ---: |
| rope_dominant | 32 | 0 | 6 | 2 |
| snare_dominant | 0 | 2 | 0 | 0 |
| genuinely_perspectival | 333 | 67 | 2 | 0 |
| structurally_ambiguous | 58 | 2 | 2 | 0 |

### Coalition type × subtype

| Subtype | analytical_dissent | institutional_dissent | other | split_field | uniform_tangled |
| :--- | ---: | ---: | ---: | ---: | ---: |
| rope_dominant | 0 | 12 | 5 | 0 | 23 |
| snare_dominant | 0 | 0 | 2 | 0 | 0 |
| genuinely_perspectival | 0 | 220 | 1 | 179 | 2 |
| structurally_ambiguous | 4 | 56 | 0 | 0 | 2 |

## 7. Implications

1. **Framework validation:** 402 genuinely_perspectival constraints (79.4%) demonstrate that observer perspective genuinely changes the rope↔snare classification — the core tangled_rope thesis.

2. **Population refinement:** 40 rope_dominant + 2 snare_dominant constraints (8.3%) could potentially be reclassified to their dominant type, reducing the tangled_rope population.

3. **Structural ambiguity:** 62 structurally_ambiguous constraints (12.3%) resist clean decomposition — extraction and coordination are genuinely intertwined.

