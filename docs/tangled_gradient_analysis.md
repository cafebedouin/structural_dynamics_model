# Tangled Gradient Analysis

**Generated:** 2026-03-14 02:42

---

## 1. Executive Summary

Analyzed **2221** tangled_rope constraints (defined by `claimed_type == 'tangled_rope'`).

Each constraint's position on the rope–snare continuum is measured via a gradient vector computed per-perspective. The primary axis (Chi) varies by observer; epsilon and suppression are observer-invariant.

### Subtype distribution

| Subtype | Count | % | Interpretation |
| :--- | ---: | ---: | :--- |
| rope_dominant | 12 | 0.5% | Complicated rope — coordination primary |
| snare_dominant | 9 | 0.4% | Disguised snare — extraction primary |
| genuinely_perspectival | 2001 | 90.1% | True tangled_rope — framework validation |
| structurally_ambiguous | 199 | 9.0% | Coordination and extraction inseparable |
| unclassifiable | 0 | 0.0% | Missing Chi data |
| **Total** | **2221** | **100%** | |

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

### rope_dominant exemplars (12 total)

- **`automatic_enrollment_defaults`**: Chi gradients = pow=-0.02, mod=0.01, ins=-1.17, ana=0.29 → rope_dominant
- **`cancer_chronotherapy_timing`**: Chi gradients = pow=0.01, mod=-0.25, ins=-1.17, ana=0.29 → rope_dominant
- **`evolutionary_knowledge`**: Chi gradients = pow=-0.01, mod=0.01, ins=-1.17, ana=-0.77 → rope_dominant
- **`gravitational_wave_source_localization`**: Chi gradients = pow=-0.01, mod=0.01, ins=-1.11, ana=0.29 → rope_dominant
- **`grief_coordination_mechanism`**: Chi gradients = pow=-0.01, mod=0.01, ins=-1.17, ana=0.29 → rope_dominant

### snare_dominant exemplars (9 total)

- **`ai_safety_verification`**: Chi gradients = pow=0.90, mod=0.94, ins=1.18, ana=1.43 → snare_dominant
- **`backwards_compatibility_ratchet`**: Chi gradients = pow=0.90, mod=0.94, ins=0.76, ana=1.43 → snare_dominant
- **`fragile_middle_layer_collapse`**: Chi gradients = pow=0.90, mod=0.94, ins=1.01, ana=1.43 → snare_dominant
- **`iaea_verification_asymmetry`**: Chi gradients = pow=0.90, mod=0.94, ins=0.76, ana=1.43 → snare_dominant
- **`institutional_evidence_disclosure_asymmetry`**: Chi gradients = pow=0.90, mod=0.94, ins=1.10, ana=1.43 → snare_dominant

### genuinely_perspectival exemplars (2001 total)

- **`a_level_grading_inflation`**: Chi gradients = pow=-0.16, mod=1.01, ins=-1.21, ana=1.43 → genuinely_perspectival
- **`abstraction_boundary_overrun`**: Chi gradients = pow=0.69, mod=0.17, ins=-1.20, ana=1.17 → genuinely_perspectival
- **`academic_citation_metrics_as_career_incentive`**: Chi gradients = pow=-0.26, mod=0.73, ins=-1.25, ana=1.17 → genuinely_perspectival
- **`academic_fashion_modernism_2026`**: Chi gradients = pow=-0.16, mod=0.94, ins=-1.21, ana=1.43 → genuinely_perspectival
- **`academic_journal_peer_review_gatekeeping`**: Chi gradients = pow=-0.22, mod=0.80, ins=-1.20, ana=1.26 → genuinely_perspectival

### structurally_ambiguous exemplars (199 total)

- **`ad_fus_coordination`**: Chi gradients = pow=0.20, mod=0.23, ins=-0.93, ana=0.55 → structurally_ambiguous
- **`algorithmic_labor_control`**: Chi gradients = pow=1.04, mod=1.08, ins=0.42, ana=1.61 → structurally_ambiguous
- **`algorithmic_management_escalation`**: Chi gradients = pow=0.90, mod=0.94, ins=0.64, ana=1.43 → structurally_ambiguous
- **`attribution_ambiguity_triplet_sc`**: Chi gradients = pow=0.20, mod=0.23, ins=-1.18, ana=0.55 → structurally_ambiguous
- **`autonomous_spacecraft_navigation`**: Chi gradients = pow=0.20, mod=0.23, ins=-1.18, ana=0.55 → structurally_ambiguous

## 5. Perspectival Test

### Chi variance by subtype

| Subtype | Count | Mean variance | Gradient flips |
| :--- | ---: | ---: | ---: |
| rope_dominant | 12 | 0.2655 | 0 |
| snare_dominant | 9 | 0.0537 | 0 |
| genuinely_perspectival | 2001 | 0.8681 | 2001 |
| structurally_ambiguous | 199 | 0.3690 | 176 |

### Most common maximum-divergence pairs

| Pair | Frequency | Mean divergence |
| :--- | ---: | ---: |
| institutional↔analytical | 2069 | 2.3531 |
| powerless↔analytical | 146 | 1.5123 |
| moderate↔institutional | 6 | 1.9360 |

**Epsilon invariance:** 2221/2221 constraints have identical epsilon across all perspectives (confirmed).

## 6. Cross-References

### PSI band × subtype

| Subtype | genuinely_tangled | rope_leaning | snare_leaning |
| :--- | ---: | ---: | ---: |
| rope_dominant | 9 | 3 | 0 |
| snare_dominant | 8 | 0 | 1 |
| genuinely_perspectival | 1439 | 418 | 144 |
| structurally_ambiguous | 164 | 33 | 2 |

### H1 band × subtype

| Subtype | H1=0 | H1=3 | H1=4 | H1=5 | H1=6 |
| :--- | ---: | ---: | ---: | ---: | ---: |
| rope_dominant | 9 | 3 | 0 | 0 | 0 |
| snare_dominant | 7 | 1 | 1 | 0 | 0 |
| genuinely_perspectival | 1431 | 175 | 7 | 253 | 135 |
| structurally_ambiguous | 168 | 30 | 1 | 0 | 0 |

### Signature × subtype

| Subtype | false_natural_law | false_ci_rope |
| :--- | ---: | ---: |
| rope_dominant | 9 | 3 |
| snare_dominant | 7 | 2 |
| genuinely_perspectival | 1426 | 575 |
| structurally_ambiguous | 163 | 36 |

### Coalition type × subtype

| Subtype | analytical_dissent | institutional_dissent | other | split_field | uniform_tangled |
| :--- | ---: | ---: | ---: | ---: | ---: |
| rope_dominant | 0 | 3 | 0 | 0 | 9 |
| snare_dominant | 2 | 0 | 0 | 0 | 7 |
| genuinely_perspectival | 1 | 79 | 102 | 388 | 1431 |
| structurally_ambiguous | 4 | 27 | 0 | 0 | 168 |

## 7. Implications

1. **Framework validation:** 2001 genuinely_perspectival constraints (90.1%) demonstrate that observer perspective genuinely changes the rope↔snare classification — the core tangled_rope thesis.

2. **Population refinement:** 12 rope_dominant + 9 snare_dominant constraints (0.9%) could potentially be reclassified to their dominant type, reducing the tangled_rope population.

3. **Structural ambiguity:** 199 structurally_ambiguous constraints (9.0%) resist clean decomposition — extraction and coordination are genuinely intertwined.

