
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================
# Giant Component Analysis: Erdos-Renyi Phase Transition

*Investigates whether the constraint network exhibits a phase transition*  
*in connected component structure as coupling threshold varies.*

---

## Phase 1: Network Topology at Default Threshold

**Context**: analytical/global (default)  
**Coupling threshold**: 0.500

### Network Summary

| Metric | Value |
|--------|-------|
| Total nodes (constraints) | 3380 |
| Connected nodes (degree > 0) | 3251 |
| Isolated nodes (degree 0) | 129 |
| Edges | 25757 |
| Graph density | 0.004510 |
| Average degree | 15.24 |
| Connected components | 193 |
| E-R critical edge count (n/2) | 1690.0 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 3380 |
| Min | 0 |
| Q1 | 2 |
| Median | 4 |
| Q3 | 15 |
| Max | 208 |
| Mean | 15.24 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 129 |
| 1 | 552 |
| 2-3 | 858 |
| 4-6 | 470 |
| 7-10 | 316 |
| 11-20 | 402 |
| 21+ | 653 |

### Connected Components

**193 components** found.

**Largest component**: 3014 nodes (89.2% of network)

**Giant component detected.** The largest component contains >50% of all nodes.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 3014 | 0.892 |
| 2 | 13 | 0.004 |
| 3 | 12 | 0.004 |
| 4 | 11 | 0.003 |
| 5 | 7 | 0.002 |
| 6 | 7 | 0.002 |
| 7 | 6 | 0.002 |
| 8 | 4 | 0.001 |
| 9 | 4 | 0.001 |
| 10 | 4 | 0.001 |
| 11 | 4 | 0.001 |
| 12 | 4 | 0.001 |
| 13 | 4 | 0.001 |
| 14 | 4 | 0.001 |
| 15 | 4 | 0.001 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 430 | 0.127 |
| rope | 114 | 0.034 |
| scaffold | 56 | 0.017 |
| tangled_rope | 491 | 0.145 |
| piton | 1 | 0.000 |
| snare | 2267 | 0.671 |
| naturalized | 2 | 0.001 |
| unknown | 19 | 0.006 |

### Purity Landscape

#### Intrinsic Purity (3328/3380 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.341 |
| Median | 0.362 |
| Q3 | 0.500 |
| Max | 1.000 |
| Mean | 0.474 |

#### Effective Purity (3328/3380 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.000 |
| Q1 | 0.312 |
| Median | 0.341 |
| Q3 | 0.384 |
| Max | 1.000 |
| Mean | 0.410 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 546 | 434 | 112 |
| Borderline (0.50 - 0.70) | 288 | 117 | 171 |
| Warning (0.30 - 0.50) | 2427 | 2316 | 111 |
| Degraded (< 0.30) | 67 | 461 | -394 |

**557 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| algorithmic_feed_substitution | snare | 208 | 1.00 | 0.213 | 208.00 |
| algorithmic_recommendation_opacity | snare | 190 | 1.00 | 0.310 | 190.00 |
| human_ai_epistemic_dependency | snare | 182 | 1.00 | 0.270 | 182.00 |
| algorithmic_filter_bubbles | snare | 180 | 1.00 | 0.267 | 180.00 |
| algorithmic_information_curation | snare | 178 | 1.00 | 0.310 | 178.00 |
| algorithmic_content_amplification | snare | 176 | 1.00 | 0.310 | 176.00 |
| ai_religion_regulation | snare | 163 | 1.00 | 0.289 | 163.00 |
| digital_paternalism_asymmetry | snare | 162 | 1.00 | 0.180 | 162.00 |
| establishment_hegemony_2026 | snare | 153 | 1.00 | 0.000 | 153.00 |
| identity_stack_incompatibility | snare | 150 | 1.00 | 0.201 | 150.00 |
| artist_economic_precarity | snare | 148 | 1.00 | 0.201 | 148.00 |
| content_moderation_scalability | snare | 142 | 1.00 | 0.180 | 142.00 |
| behavioral_data_surveillance | snare | 139 | 1.00 | 0.000 | 139.00 |
| platform_algorithmic_capture | snare | 138 | 1.00 | 0.044 | 138.00 |
| algorithmic_attention_capture | snare | 134 | 1.00 | 0.214 | 134.00 |
| digital_ownership_erosion | snare | 133 | 1.00 | 0.268 | 133.00 |
| facebook_content_moderation_opacity | snare | 132 | 1.00 | 0.295 | 132.00 |
| algorithmic_opacity_extraction | snare | 131 | 1.00 | 0.302 | 131.00 |
| data_extraction_surveillance | snare | 130 | 1.00 | 0.000 | 130.00 |
| value_alignment_drift | snare | 129 | 1.00 | 0.230 | 129.00 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 25757 | 193 | 3014 | 0.892 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**Giant component size**: 3014 nodes (89.2% of network)

### Giant Component Composition

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 207 | 0.069 |
| rope | 108 | 0.036 |
| scaffold | 36 | 0.012 |
| tangled_rope | 447 | 0.148 |
| piton | 1 | 0.000 |
| snare | 2197 | 0.729 |
| naturalized | 2 | 0.001 |
| unknown | 16 | 0.005 |

#### Purity Within Giant Component

- Coverage: intrinsic 2989/3014 scorable, effective 2989/3014 scorable
- **Intrinsic**: min=0.271, median=0.358, max=1.000, mean=0.440
- **Effective**: min=0.000, median=0.338, max=1.000, mean=0.369

- **Active contamination sources** (intrinsic purity < 0.50): 2400
- **Sound constraints** (effective purity >= 0.70): 214

### Contamination Sources (Super-spreaders in Giant Component)

**200 contamination-capable nodes** in the giant component.

| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |
|------------|------|-----------------|------------|------------|-----------|
| algorithmic_feed_substitution | snare | 208 | 1.00 | 0.213 | 208.00 |
| algorithmic_recommendation_opacity | snare | 190 | 1.00 | 0.310 | 190.00 |
| human_ai_epistemic_dependency | snare | 182 | 1.00 | 0.270 | 182.00 |
| algorithmic_filter_bubbles | snare | 180 | 1.00 | 0.267 | 180.00 |
| algorithmic_information_curation | snare | 178 | 1.00 | 0.310 | 178.00 |
| algorithmic_content_amplification | snare | 176 | 1.00 | 0.310 | 176.00 |
| ai_religion_regulation | snare | 163 | 1.00 | 0.289 | 163.00 |
| digital_paternalism_asymmetry | snare | 162 | 1.00 | 0.180 | 162.00 |
| establishment_hegemony_2026 | snare | 153 | 1.00 | 0.000 | 153.00 |
| identity_stack_incompatibility | snare | 150 | 1.00 | 0.201 | 150.00 |
| artist_economic_precarity | snare | 148 | 1.00 | 0.201 | 148.00 |
| content_moderation_scalability | snare | 142 | 1.00 | 0.180 | 142.00 |
| behavioral_data_surveillance | snare | 139 | 1.00 | 0.000 | 139.00 |
| platform_algorithmic_capture | snare | 138 | 1.00 | 0.044 | 138.00 |
| algorithmic_attention_capture | snare | 134 | 1.00 | 0.214 | 134.00 |
| digital_ownership_erosion | snare | 133 | 1.00 | 0.268 | 133.00 |
| facebook_content_moderation_opacity | snare | 132 | 1.00 | 0.295 | 132.00 |
| algorithmic_opacity_extraction | snare | 131 | 1.00 | 0.302 | 131.00 |
| data_extraction_surveillance | snare | 130 | 1.00 | 0.000 | 130.00 |
| value_alignment_drift | snare | 129 | 1.00 | 0.230 | 129.00 |

### Multi-hop Contamination Simulation

Simulating contamination propagation beyond the current one-hop model.
Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.

**2387 active contamination sources** (type strength >= 0.5, purity < 0.50)

*Showing top 50 of 155 sources by contamination potential.*

| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |
|--------|------|--------|-------|-------|-------|-------------|
| algorithmic_feed_substitution | snare | 0.341 | 208 | 672 | 1093 | 1973 |
| algorithmic_recommendation_opacity | snare | 0.312 | 190 | 646 | 1113 | 1949 |
| human_ai_epistemic_dependency | snare | 0.329 | 182 | 659 | 1097 | 1938 |
| algorithmic_filter_bubbles | snare | 0.329 | 180 | 653 | 1103 | 1936 |
| algorithmic_information_curation | snare | 0.312 | 178 | 644 | 1107 | 1929 |
| algorithmic_content_amplification | snare | 0.312 | 176 | 642 | 1106 | 1924 |
| ai_religion_regulation | snare | 0.321 | 163 | 769 | 1055 | 1987 |
| digital_paternalism_asymmetry | snare | 0.353 | 162 | 781 | 1056 | 1999 |
| establishment_hegemony_2026 | snare | 0.429 | 153 | 765 | 1127 | 2045 |
| identity_stack_incompatibility | snare | 0.350 | 150 | 759 | 1056 | 1965 |
| artist_economic_precarity | snare | 0.350 | 148 | 599 | 1042 | 1789 |
| content_moderation_scalability | snare | 0.353 | 142 | 749 | 1116 | 2007 |
| behavioral_data_surveillance | snare | 0.429 | 139 | 595 | 1137 | 1871 |
| platform_algorithmic_capture | snare | 0.378 | 138 | 592 | 1064 | 1794 |
| algorithmic_attention_capture | snare | 0.354 | 134 | 568 | 1080 | 1782 |
| digital_ownership_erosion | snare | 0.341 | 133 | 578 | 1055 | 1766 |
| facebook_content_moderation_opacity | snare | 0.321 | 132 | 736 | 1119 | 1987 |
| algorithmic_opacity_extraction | snare | 0.321 | 131 | 648 | 1060 | 1839 |
| data_extraction_surveillance | snare | 0.467 | 130 | 572 | 1120 | 1822 |
| value_alignment_drift | snare | 0.350 | 129 | 586 | 1111 | 1826 |
| online_harassment_norm_enforcement | snare | 0.329 | 128 | 727 | 1124 | 1979 |
| web_app_friction_tax | snare | 0.352 | 127 | 553 | 1076 | 1756 |
| algorithmic_engagement_loop | snare | 0.325 | 126 | 586 | 1093 | 1805 |
| algorithmic_labor_control | snare | 0.354 | 125 | 620 | 1053 | 1798 |
| dark_patterns_manipulation | snare | 0.341 | 124 | 555 | 1069 | 1748 |
| data_extraction_regimes | snare | 0.382 | 123 | 545 | 1069 | 1737 |
| algorithmic_content_filtering | snare | 0.321 | 120 | 574 | 1077 | 1771 |
| algorithmic_content_control | snare | 0.329 | 119 | 556 | 1077 | 1752 |
| algorithmic_curation_opacity | snare | 0.299 | 118 | 528 | 1057 | 1703 |
| entertainment_platform_licensing | snare | 0.353 | 117 | 553 | 1034 | 1704 |
| consumer_price_discrimination | snare | 0.341 | 115 | 526 | 1060 | 1701 |
| attention_economy_extraction | snare | 0.354 | 114 | 595 | 1116 | 1825 |
| attention_allocation_problem | snare | 0.350 | 113 | 551 | 1083 | 1747 |
| hypercompression_of_time_horizons | snare | 0.378 | 112 | 563 | 1067 | 1742 |
| algorithmic_amplification | snare | 0.329 | 110 | 526 | 1038 | 1674 |
| commercial_fishery_quota_systems | snare | 0.390 | 109 | 748 | 1091 | 1948 |
| algorithmic_cultural_curation | snare | 0.312 | 108 | 525 | 1044 | 1677 |
| algorithmic_curation | snare | 0.329 | 107 | 513 | 1046 | 1666 |
| algorithmic_fairness_verification | snare | 0.362 | 106 | 517 | 1049 | 1672 |
| algorithmic_addiction_capture_u16 | snare | 0.354 | 105 | 512 | 1040 | 1657 |
| algorithmic_accountability_gap | snare | 0.321 | 104 | 525 | 1050 | 1679 |
| algorithmic_amplification_of_extremism | snare | 0.366 | 103 | 512 | 1043 | 1658 |
| algorithmic_management_escalation | snare | 0.341 | 102 | 509 | 1044 | 1655 |
| institutional_truth_capacity | snare | 0.312 | 99 | 552 | 1120 | 1771 |
| drinking_water_contamination_legacy | snare | 0.354 | 98 | 740 | 1131 | 1969 |
| climate_change_mitigation | snare | 0.341 | 97 | 587 | 1141 | 1825 |
| sovereign_debt_sustainability | snare | 0.366 | 94 | 591 | 1106 | 1791 |
| ai_model_training_data_asymmetry | snare | 0.341 | 92 | 579 | 1112 | 1783 |
| data_laundering_pipeline | snare | 0.378 | 91 | 540 | 1129 | 1760 |
| attention_scarcity_rent_seeking | snare | 0.312 | 88 | 533 | 1127 | 1748 |

**Total unique nodes reached** within 3 hops of any source: 532 (17.7% of giant component)

### Sound Constraint Exposure to Contamination

**214 sound constraints** (effective purity >= 0.70) in the giant component.

*Showing first 50 of 214 sound constraints.*

| Sound Constraint | Eff Purity | Nearest Source | Distance | Would Cross Threshold? |
|------------------|------------|----------------|----------|----------------------|
| absorbing_markov_chains | 0.960 | nearby_source | 1 | ~ |
| ackermann_function_bounds | 0.976 | nearby_source | 5 | ~ |
| ackermann_function_computability | 0.960 | nearby_source | 3 | ~ |
| age_related_capacity_erosion | 0.976 | nearby_source | 1 | ~ |
| algebraic_closure_property | 0.976 | nearby_source | 2 | ~ |
| algorithmic_information_theory | 0.960 | nearby_source | 3 | ~ |
| algorithmic_randomness | 0.936 | nearby_source | 3 | ~ |
| angular_momentum_conservation | 0.976 | nearby_source | 9 | ~ |
| arithmetical_hierarchy_ordering | 0.960 | nearby_source | 3 | ~ |
| atrophied_optimization_piton | 0.711 | nearby_source | 1 | ~ |
| attention_as_capturable_resource | 0.936 | nearby_source | 2 | ~ |
| axiom_of_choice_consequence | 0.960 | nearby_source | 4 | ~ |
| axiom_of_choice_dependency | 0.960 | nearby_source | 6 | ~ |
| axiom_of_choice_independence | 0.960 | nearby_source | 5 | ~ |
| banach_contraction_principle | 0.976 | nearby_source | 4 | ~ |
| banach_fixed_point_theorem | 0.960 | nearby_source | 3 | ~ |
| banach_tarski_paradox | 0.976 | nearby_source | 5 | ~ |
| barcode_standardization | 0.808 | nearby_source | 1 | ~ |
| battery_chemistry_limitations | 0.936 | nearby_source | 2 | ~ |
| bgs_eigenvector_thermalization | 0.728 | nearby_source | 3 | ~ |
| bgs_spectral_universality | 0.976 | nearby_source | 3 | ~ |
| bifurcation_analysis | 0.726 | nearby_source | 3 | ~ |
| bilateral_science_innovation_partnership | 0.753 | nearby_source | 1 | ~ |
| biodiversity_collapse_threshold | 1.000 | nearby_source | 1 | ~ |
| birthday_paradox_collison | 0.976 | nearby_source | 1 | ~ |
| boltzmann_universality_2026 | 0.960 | nearby_source | 3 | ~ |
| brazil_hiv_vtn_elimination | 0.706 | nearby_source | 1 | ~ |
| brouwer_fixed_point | 0.976 | nearby_source | 3 | ~ |
| burali_forte_paradox | 0.960 | nearby_source | 5 | ~ |
| busy_beaver_function | 0.976 | nearby_source | 4 | ~ |
| busy_beaver_noncomputability | 0.960 | nearby_source | 4 | ~ |
| cantor_diagonal_argument | 0.976 | nearby_source | 4 | ~ |
| cantors_diagonal_argument | 0.976 | nearby_source | 5 | ~ |
| capability_velocity_mismatch | 0.976 | nearby_source | 1 | ~ |
| career_duration_compression | 1.000 | nearby_source | 1 | ~ |
| categorical_instrument_blindness | 0.857 | nearby_source | 1 | ~ |
| causality_constraint | 0.960 | nearby_source | 7 | ~ |
| cellular_automaton_universality | 0.960 | nearby_source | 2 | ~ |
| chaitins_omega_undecidability | 0.960 | nearby_source | 3 | ~ |
| chaos_theory_determinism | 0.936 | nearby_source | 3 | ~ |
| chaos_theory_foundation | 0.960 | nearby_source | 3 | ~ |
| chaotic_dynamical_systems_predictability | 0.960 | nearby_source | 2 | ~ |
| chip_control_efficacy | 0.722 | nearby_source | 1 | ~ |
| chip_design_complexity_scaling | 0.801 | nearby_source | 2 | ~ |
| church_turing_thesis | 0.960 | nearby_source | 4 | ~ |
| circuit_layout_verification | 0.840 | nearby_source | 5 | ~ |
| climate_model_initialization | 0.727 | nearby_source | 1 | ~ |
| climate_model_paleoclimate_dependency | 0.760 | nearby_source | 1 | ~ |
| clinical_authority_topology | 0.968 | nearby_source | 1 | ~ |
| collatz_conjecture_determinism | 0.976 | nearby_source | 3 | ~ |

**Hop distance summary**:
- Within 1 hop of a contamination source: 70/214 sound constraints
- Within 2 hops: 103/214
- Within 3 hops: 132/214

### Contamination Collapse Analysis

At what contamination settings would sound constraints in the giant component collapse into the degraded zone?

Current settings: cap=0.30, attenuation=0.50
Sound constraints in giant component: 214

**Purity coverage**: 2989 of 3014 giant-component members have a numeric effective purity; 25 excluded (members with no numeric effective purity).

Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at 0.50):

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 225 | 118 | 2215 | 431 |
| 0.20 | 215 | 96 | 2222 | 456 |
| 0.30 | 214 | 92 | 2224 | 459 |
| 0.40 | 214 | 92 | 2224 | 459 |
| 0.50 | 214 | 92 | 2224 | 459 |
| 0.60 | 214 | 92 | 2224 | 459 |
| 0.70 | 214 | 92 | 2224 | 459 |
| 0.80 | 214 | 92 | 2224 | 459 |
| 0.90 | 214 | 92 | 2224 | 459 |
| 1.00 | 214 | 92 | 2224 | 459 |

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 25757 edges, 193 components, largest = 3014 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 430 |
| rope | 2456 | 527 | 114 |
| scaffold | 91 | 83 | 56 |
| tangled_rope | 135 | 2262 | 491 |
| piton | 26 | 2 | 1 |
| snare | 6 | 470 | 2267 |
| naturalized | 664 | 7 | 2 |
| unknown | 2 | 29 | 19 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 6 | 26 | 135 | 91 | 258 |
| Moderate/National | 470 | 2 | 2262 | 83 | 2817 |
| Analytical/Global (default) | 2267 | 1 | 491 | 56 | 2815 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 25757, 193, 3014, 0.892).
```

---

## Provenance split (OQ-193)

*Pooled topology counts within-kernel reading-plurality (sibling `affects_constraint` edges) as coupling. The stratum strips explicit same-kernel sibling edges (retract-recompute) to expose cross-kernel structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine topology — this is a presentation split only, no engine-behavior change.*

**Sibling edges stripped**: 0  
**same_kernel_edges_surviving**: 0 (dedup-resurfaced 0, never-stripped 0)  
**Positive control**: ok — raw `affects_constraint` dropped by exactly 0.

| Stratum | Edges | Components | Giant size | Giant fraction |
|---------|-------|------------|------------|----------------|
| Pooled | 25757 | 193 | 3014 | 0.892 |
| Cross-kernel | 25757 | 193 | 3014 | 0.892 |

---

*End of giant component analysis*
