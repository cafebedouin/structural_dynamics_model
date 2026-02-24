# Tangled Gradient Analysis

**Generated:** 2026-02-24 01:22

---

## 1. Executive Summary

Analyzed **752** tangled_rope constraints (defined by `claimed_type == 'tangled_rope'`).

Each constraint's position on the rope–snare continuum is measured via a gradient vector computed per-perspective. The primary axis (Chi) varies by observer; epsilon and suppression are observer-invariant.

### Subtype distribution

| Subtype | Count | % | Interpretation |
| :--- | ---: | ---: | :--- |
| rope_dominant | 28 | 3.7% | Complicated rope — coordination primary |
| snare_dominant | 2 | 0.3% | Disguised snare — extraction primary |
| genuinely_perspectival | 664 | 88.3% | True tangled_rope — framework validation |
| structurally_ambiguous | 58 | 7.7% | Coordination and extraction inseparable |
| unclassifiable | 0 | 0.0% | Missing Chi data |
| **Total** | **752** | **100%** | |

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

### rope_dominant exemplars (28 total)

- **`blackstone_carried_interest_taxation`**: Chi gradients = pow=-0.08, mod=-0.06, ins=-1.17, ana=0.20 → rope_dominant
- **`boltzmann_universality_2026`**: Chi gradients = pow=-0.60, mod=-0.59, ins=-1.15, ana=-0.47 → rope_dominant
- **`china_africa_zero_tariff_2026`**: Chi gradients = pow=-0.01, mod=0.01, ins=-1.17, ana=0.29 → rope_dominant
- **`climate_target_one_point_five`**: Chi gradients = pow=-0.08, mod=-0.06, ins=-1.17, ana=0.20 → rope_dominant
- **`decentralized_infrastructure_rope`**: Chi gradients = pow=-0.85, mod=-0.84, ins=-1.14, ana=-0.78 → rope_dominant

### snare_dominant exemplars (2 total)

- **`horizon_liability_contract`**: Chi gradients = pow=1.85, mod=1.90, ins=1.33, ana=2.63 → snare_dominant
- **`sm_addictive_design`**: Chi gradients = pow=1.26, mod=1.30, ins=1.48, ana=1.88 → snare_dominant

### genuinely_perspectival exemplars (664 total)

- **`26usc469_real_estate_exemption`**: Chi gradients = pow=1.50, mod=1.55, ins=-1.23, ana=2.19 → genuinely_perspectival
- **`abstraction_boundary_overrun`**: Chi gradients = pow=1.71, mod=1.76, ins=-1.24, ana=2.45 → genuinely_perspectival
- **`abstraction_leakage`**: Chi gradients = pow=1.47, mod=1.51, ins=-1.23, ana=2.14 → genuinely_perspectival
- **`academic_peer_review_gatekeeping`**: Chi gradients = pow=1.50, mod=1.55, ins=-1.23, ana=2.19 → genuinely_perspectival
- **`academic_tenure_system`**: Chi gradients = pow=1.50, mod=1.55, ins=-1.23, ana=2.19 → genuinely_perspectival

### structurally_ambiguous exemplars (58 total)

- **`advice_as_dangerous_gift`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`alzheimers_levetiracetam`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`asce_7_22_seismic_design`**: Chi gradients = pow=0.10, mod=0.12, ins=-1.18, ana=0.42 → structurally_ambiguous
- **`availability_heuristic`**: Chi gradients = pow=0.27, mod=0.30, ins=-1.18, ana=0.64 → structurally_ambiguous
- **`comitatus_bond`**: Chi gradients = pow=0.27, mod=0.30, ins=-1.18, ana=0.64 → structurally_ambiguous

## 5. Perspectival Test

### Chi variance by subtype

| Subtype | Count | Mean variance | Gradient flips |
| :--- | ---: | ---: | ---: |
| rope_dominant | 28 | 0.1639 | 0 |
| snare_dominant | 2 | 0.1364 | 0 |
| genuinely_perspectival | 664 | 1.3490 | 664 |
| structurally_ambiguous | 58 | 0.4293 | 37 |

### Most common maximum-divergence pairs

| Pair | Frequency | Mean divergence |
| :--- | ---: | ---: |
| institutional↔analytical | 749 | 2.7714 |
| powerless↔analytical | 2 | 1.0233 |
| moderate↔institutional | 1 | 2.7792 |

**Epsilon invariance:** 752/752 constraints have identical epsilon across all perspectives (confirmed).

## 6. Cross-References

### PSI band × subtype

| Subtype | genuinely_tangled | rope_leaning | snare_leaning |
| :--- | ---: | ---: | ---: |
| rope_dominant | 6 | 21 | 1 |
| snare_dominant | 0 | 0 | 2 |
| genuinely_perspectival | 52 | 29 | 583 |
| structurally_ambiguous | 15 | 26 | 17 |

### H1 band × subtype

| Subtype | H1=0 | H1=3 | H1=4 | H1=5 | H1=6 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| rope_dominant | 12 | 10 | 6 | 0 | 0 |
| snare_dominant | 2 | 0 | 0 | 0 | 0 |
| genuinely_perspectival | 38 | 424 | 0 | 197 | 5 |
| structurally_ambiguous | 6 | 50 | 1 | 1 | 0 |

### Signature × subtype

| Subtype | false_ci_rope | false_natural_law | constructed_high_extraction | constructed_low_extraction |
| :--- | ---: | ---: | ---: | ---: |
| rope_dominant | 17 | 5 | 0 | 6 |
| snare_dominant | 2 | 0 | 0 | 0 |
| genuinely_perspectival | 619 | 38 | 7 | 0 |
| structurally_ambiguous | 51 | 6 | 1 | 0 |

### Coalition type × subtype

| Subtype | analytical_dissent | institutional_dissent | other | split_field | uniform_tangled |
| :--- | ---: | ---: | ---: | ---: | ---: |
| rope_dominant | 1 | 7 | 13 | 0 | 7 |
| snare_dominant | 0 | 0 | 0 | 0 | 2 |
| genuinely_perspectival | 0 | 423 | 1 | 202 | 38 |
| structurally_ambiguous | 3 | 47 | 1 | 1 | 6 |

## 7. Implications

1. **Framework validation:** 664 genuinely_perspectival constraints (88.3%) demonstrate that observer perspective genuinely changes the rope↔snare classification — the core tangled_rope thesis.

2. **Population refinement:** 28 rope_dominant + 2 snare_dominant constraints (4.0%) could potentially be reclassified to their dominant type, reducing the tangled_rope population.

3. **Structural ambiguity:** 58 structurally_ambiguous constraints (7.7%) resist clean decomposition — extraction and coordination are genuinely intertwined.

