
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
| Total nodes (constraints) | 996 |
| Connected nodes (degree > 0) | 994 |
| Isolated nodes (degree 0) | 2 |
| Edges | 1829 |
| Graph density | 0.003691 |
| Average degree | 3.67 |
| Connected components | 89 |
| E-R critical edge count (n/2) | 498.0 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 996 |
| Min | 0 |
| Q1 | 2 |
| Median | 3 |
| Q3 | 4 |
| Max | 23 |
| Mean | 3.67 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 2 |
| 1 | 38 |
| 2-3 | 612 |
| 4-6 | 228 |
| 7-10 | 79 |
| 11-20 | 36 |
| 21+ | 1 |

### Connected Components

**89 components** found.

**Largest component**: 662 nodes (66.5% of network)

**Giant component detected.** The largest component contains >50% of all nodes.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 662 | 0.665 |
| 2 | 15 | 0.015 |
| 3 | 12 | 0.012 |
| 4 | 11 | 0.011 |
| 5 | 9 | 0.009 |
| 6 | 9 | 0.009 |
| 7 | 8 | 0.008 |
| 8 | 8 | 0.008 |
| 9 | 7 | 0.007 |
| 10 | 7 | 0.007 |
| 11 | 6 | 0.006 |
| 12 | 6 | 0.006 |
| 13 | 6 | 0.006 |
| 14 | 6 | 0.006 |
| 15 | 6 | 0.006 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 17 | 0.017 |
| rope | 49 | 0.049 |
| scaffold | 30 | 0.030 |
| tangled_rope | 165 | 0.166 |
| piton | 41 | 0.041 |
| snare | 540 | 0.542 |
| naturalized | 2 | 0.002 |
| unknown | 152 | 0.153 |

### Purity Landscape

#### Intrinsic Purity (991/996 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.450 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.494 |

#### Effective Purity (991/996 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.450 |
| Q3 | 0.570 |
| Max | 1.000 |
| Mean | 0.484 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 120 | 109 | 11 |
| Borderline (0.50 - 0.70) | 243 | 218 | 25 |
| Warning (0.30 - 0.50) | 620 | 655 | -35 |
| Degraded (< 0.30) | 8 | 9 | -1 |

**45 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| border_legitimacy__sovereignty_reading | snare | 15 | 1.00 | 0.354 | 15.00 |
| border_normative_status__sovereignty_primary | snare | 14 | 1.00 | 0.354 | 14.00 |
| ai_risk_prioritization__existential_risk_reading | snare | 13 | 1.00 | 0.353 | 13.00 |
| jewish_self_determination__diasporist_reading | snare | 12 | 1.00 | 0.354 | 12.00 |
| border_legitimacy__freedom_of_movement_reading | snare | 11 | 1.00 | 0.460 | 11.00 |
| ai_human_relationship__incarnational_humanism | snare | 10 | 1.00 | 0.354 | 10.00 |
| basic_law_interpretive_authority__popular_constitutionalism_reading | snare | 9 | 1.00 | 0.329 | 9.00 |
| acceptable_risk_energy__option_value_preserving | tangled_rope | 17 | 0.50 | 0.480 | 8.50 |
| border_control_legitimacy__freedom_of_movement_primary | snare | 8 | 1.00 | 0.443 | 8.00 |
| border_normative_status__qualified_sovereignty | snare | 7 | 1.00 | 0.354 | 7.00 |
| us_constitution_1787__positivist_reading | piton | 8 | 0.80 | 0.617 | 6.40 |
| ai_alignment_priority__nearterm_harms_reading | snare | 6 | 1.00 | 0.639 | 6.00 |
| anthropological_record__creationist_reading | snare | 5 | 1.00 | 0.354 | 5.00 |
| combatant_status_definition__functional_protection_reading | piton | 6 | 0.80 | 0.822 | 4.80 |
| ai_governance_legitimacy__technocratic_optimization_reading | tangled_rope | 9 | 0.50 | 0.470 | 4.50 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 4 | 1.00 | 0.429 | 4.00 |
| basic_law_interpretive_boundary__balanced_contestation_reading | tangled_rope | 7 | 0.50 | 0.580 | 3.50 |
| npt_treaty_text__nnws_reading | piton | 4 | 0.80 | 0.604 | 3.20 |
| acceptable_risk_energy__catastrophic_tail_dominant | snare | 3 | 1.00 | 0.354 | 3.00 |
| ai_governance_legitimacy__magisterial_subsidiarity_reading | tangled_rope | 5 | 0.50 | 0.598 | 2.50 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 1829 | 89 | 662 | 0.665 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**Giant component size**: 662 nodes (66.5% of network)

### Giant Component Composition

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 9 | 0.014 |
| rope | 29 | 0.044 |
| scaffold | 21 | 0.032 |
| tangled_rope | 114 | 0.172 |
| piton | 28 | 0.042 |
| snare | 355 | 0.536 |
| naturalized | 1 | 0.002 |
| unknown | 105 | 0.159 |

#### Purity Within Giant Component

- Coverage: intrinsic 661/662 scorable, effective 661/662 scorable
- **Intrinsic**: min=0.271, median=0.450, max=1.000, mean=0.488
- **Effective**: min=0.271, median=0.434, max=1.000, mean=0.475

- **Active contamination sources** (intrinsic purity < 0.50): 430
- **Sound constraints** (effective purity >= 0.70): 63

### Contamination Sources (Super-spreaders in Giant Component)

**36 contamination-capable nodes** in the giant component.

| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |
|------------|------|-----------------|------------|------------|-----------|
| border_legitimacy__sovereignty_reading | snare | 15 | 1.00 | 0.354 | 15.00 |
| border_normative_status__sovereignty_primary | snare | 14 | 1.00 | 0.354 | 14.00 |
| ai_risk_prioritization__existential_risk_reading | snare | 13 | 1.00 | 0.353 | 13.00 |
| jewish_self_determination__diasporist_reading | snare | 12 | 1.00 | 0.354 | 12.00 |
| border_legitimacy__freedom_of_movement_reading | snare | 11 | 1.00 | 0.460 | 11.00 |
| ai_human_relationship__incarnational_humanism | snare | 10 | 1.00 | 0.354 | 10.00 |
| basic_law_interpretive_authority__popular_constitutionalism_reading | snare | 9 | 1.00 | 0.329 | 9.00 |
| acceptable_risk_energy__option_value_preserving | tangled_rope | 17 | 0.50 | 0.480 | 8.50 |
| border_control_legitimacy__freedom_of_movement_primary | snare | 8 | 1.00 | 0.443 | 8.00 |
| border_normative_status__qualified_sovereignty | snare | 7 | 1.00 | 0.354 | 7.00 |
| us_constitution_1787__positivist_reading | piton | 8 | 0.80 | 0.617 | 6.40 |
| ai_alignment_priority__nearterm_harms_reading | snare | 6 | 1.00 | 0.639 | 6.00 |
| anthropological_record__creationist_reading | snare | 5 | 1.00 | 0.354 | 5.00 |
| combatant_status_definition__functional_protection_reading | piton | 6 | 0.80 | 0.822 | 4.80 |
| ai_governance_legitimacy__technocratic_optimization_reading | tangled_rope | 9 | 0.50 | 0.470 | 4.50 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 4 | 1.00 | 0.429 | 4.00 |
| basic_law_interpretive_boundary__balanced_contestation_reading | tangled_rope | 7 | 0.50 | 0.580 | 3.50 |
| npt_treaty_text__nnws_reading | piton | 4 | 0.80 | 0.604 | 3.20 |
| acceptable_risk_energy__catastrophic_tail_dominant | snare | 3 | 1.00 | 0.354 | 3.00 |
| ai_governance_legitimacy__magisterial_subsidiarity_reading | tangled_rope | 5 | 0.50 | 0.598 | 2.50 |

### Multi-hop Contamination Simulation

Simulating contamination propagation beyond the current one-hop model.
Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.

**351 active contamination sources** (type strength >= 0.5, purity < 0.50)

| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |
|--------|------|--------|-------|-------|-------|-------------|
| border_legitimacy__sovereignty_reading | snare | 0.354 | 15 | 24 | 46 | 85 |
| border_normative_status__sovereignty_primary | snare | 0.354 | 14 | 25 | 46 | 85 |
| ai_risk_prioritization__existential_risk_reading | snare | 0.354 | 13 | 32 | 33 | 78 |
| jewish_self_determination__diasporist_reading | snare | 0.354 | 12 | 16 | 15 | 43 |
| hebrew_linguistic_life__liturgical_preservation_reading | snare | 0.354 | 11 | 18 | 12 | 41 |
| ai_human_relationship__incarnational_humanism | snare | 0.354 | 10 | 27 | 53 | 90 |
| basic_law_interpretive_authority__popular_constitutionalism_reading | snare | 0.329 | 9 | 19 | 36 | 64 |
| common_article_3_scope__expansive_human_rights_reading | snare | 0.312 | 8 | 16 | 32 | 56 |
| border_normative_status__qualified_sovereignty | snare | 0.354 | 7 | 18 | 28 | 53 |
| basic_law_interpretive_boundary__judicial_supremacy_reading | snare | 0.354 | 6 | 17 | 30 | 53 |
| anthropological_record__creationist_reading | snare | 0.354 | 5 | 8 | 17 | 30 |
| ai_governance_legitimacy__technocratic_optimization_reading | tangled_rope | 0.486 | 9 | 37 | 48 | 94 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 0.429 | 4 | 23 | 30 | 57 |
| vaccine_mandate_balance__proportionality_reading | tangled_rope | 0.450 | 7 | 18 | 30 | 55 |
| press_reformation_causation__mutual_shaping | piton | 0.434 | 4 | 5 | 3 | 12 |
| acceptable_risk_energy__catastrophic_tail_dominant | snare | 0.354 | 3 | 18 | 31 | 52 |
| ai_human_relationship__instrumental_subsidiarity | tangled_rope | 0.482 | 5 | 22 | 40 | 67 |
| catastrophe_memory_transmission__hybrid_embedded_reading | piton | 0.408 | 3 | 11 | 30 | 44 |
| abrahamic_covenant__isaac_covenant_reading | snare | 0.354 | 2 | 1 | 2 | 5 |
| fair_use_statutory_exception__transformative_right_reading | piton | 0.450 | 2 | 5 | 18 | 25 |
| biblical_authority__sola_scriptura_reading | tangled_rope | 0.488 | 3 | 3 | 2 | 8 |
| ai_dignity_safeguarding__autonomy_rights_reading | tangled_rope | 0.450 | 2 | 1 | 2 | 5 |
| catastrophe_avoidance_retention__simulation_as_proxy_catastrophe | tangled_rope | 0.458 | 1 | 3 | 3 | 7 |

**Total unique nodes reached** within 3 hops of any source: 311 (47.0% of giant component)

### Sound Constraint Exposure to Contamination

**63 sound constraints** (effective purity >= 0.70) in the giant component.

*Showing first 50 of 63 sound constraints.*

| Sound Constraint | Eff Purity | Nearest Source | Distance | Would Cross Threshold? |
|------------------|------------|----------------|----------|----------------------|
| ai_dignity_safeguarding__posthuman_continuity_reading | 0.783 | nearby_source | 1 | ~ |
| ai_governance_legitimacy__market_libertarian_reading | 0.972 | nearby_source | 1 | ~ |
| animal_status__abolitionist_reading | 0.887 | nearby_source | 2 | ~ |
| animal_status__property_reading | 0.988 | nearby_source | 2 | ~ |
| basic_law_interpretive_boundary__parliamentary_sovereignty_reading | 0.968 | nearby_source | 1 | ~ |
| catastrophe_memory_function__survival_competence_reading | 0.915 | nearby_source | 1 | ~ |
| catastrophe_memory_kernel__symbol_continuity_reading | 0.901 | nearby_source | 1 | ~ |
| catastrophe_memory_preservation__mourning_practice_reading | 0.820 | nearby_source | 1 | ~ |
| combatant_status_definition__functional_protection_reading | 0.822 | nearby_source | 1 | ~ |
| constitutional_text__popular_sovereignty_reading | 0.825 | nearby_source | 1 | ~ |
| creed_381_pneumatology__ecumenical_reunion_reading | 0.782 | nearby_source | 1 | ~ |
| derivative_work_statutory_boundary__coordination_reading | 0.754 | nearby_source | 1 | ~ |
| dueling_disappearance_mechanism__contraction_reading | 0.964 | nearby_source | 1 | ~ |
| dueling_disappearance_mechanism__institutional_displacement_reading | 0.960 | nearby_source | 1 | ~ |
| equal_protection_clause__colorblind_reading | 0.992 | nearby_source | 2 | ~ |
| feudal_oath_reciprocity__vassal_coordination_reading | 0.782 | nearby_source | 2 | ~ |
| fisa_702_statutory_text__constitutional_floor_reading | 0.932 | nearby_source | 2 | ~ |
| fisa_702_statutory_text__foreign_target_strict_reading | 0.860 | nearby_source | 2 | ~ |
| hebrew_vitality__liturgical_reading | 0.856 | nearby_source | 1 | ~ |
| honor_satisfaction_substrate__cultural_contraction_reading | 0.984 | nearby_source | 1 | ~ |
| human_transcendence_pathway__jerusalem_reading | 0.764 | nearby_source | 2 | ~ |
| humane_treatment_standard__absolute_prohibition | 0.937 | nearby_source | 1 | ~ |
| ietf_openness_commitment__commons_stewardship_reading | 0.948 | nearby_source | 1 | ~ |
| income_support_commitment__freedom_floor_reading | 0.917 | nearby_source | 1 | ~ |
| income_support_conditionality__freedom_floor_reading | 0.761 | nearby_source | 1 | ~ |
| kodashim_commandment_status__study_as_performance | 1.000 | nearby_source | 1 | ~ |
| kodashim_obligation__study_as_performance | 1.000 | nearby_source | 1 | ~ |
| kodashim_obligation__study_as_preparation | 0.782 | nearby_source | 1 | ~ |
| latin_correctness__continuity_reading | 0.846 | nearby_source | 1 | ~ |
| legal_personhood_boundary__functional_capacity_reading | 0.800 | nearby_source | 1 | ~ |
| lycurgan_laws__sacral_fidelity_reading | 0.976 | nearby_source | 1 | ~ |
| market_as_natural_default__lapsed_alternative_reading | 1.000 | nearby_source | 3 | ~ |
| nafta_jurisdictional_boundary__sovereignty_primacy_reading | 0.935 | nearby_source | 1 | ~ |
| nicene_creed_authority__liturgical_habituation_reading | 0.798 | nearby_source | 1 | ~ |
| nicene_creed_authority__symbolic_confessional_reading | 0.976 | nearby_source | 1 | ~ |
| nuclear_impossibility_kernel__rational_dropout_reading | 1.000 | nearby_source | 1 | ~ |
| nuclear_impossibility_kernel__structural_contraction_reading | 0.988 | nearby_source | 1 | ~ |
| one_country_two_systems_framework__balanced_coexistence_reading | 0.880 | nearby_source | 1 | ~ |
| personhood_boundary__birth_threshold_reading | 0.948 | nearby_source | 1 | ~ |
| preparedness_commitment__competence_reading | 0.782 | nearby_source | 1 | ~ |
| preparedness_persistence__competence_reading | 0.952 | nearby_source | 1 | ~ |
| press_reformation_causality__technological_determinism | 0.960 | nearby_source | 1 | ~ |
| press_reformation_causation__technological_determinism | 0.908 | nearby_source | 1 | ~ |
| quranic_gender_verses__progressive_abrogation | 0.800 | nearby_source | 2 | ~ |
| rfc9293_tcp_specification__strict_invariance_reading | 0.988 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__messianic_suspension_reading | 0.976 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__study_as_exercise_reading | 1.000 | nearby_source | 1 | ~ |
| sacrifice_obligation_kernel__symbolic_archive_reading | 1.000 | nearby_source | 2 | ~ |
| shinbutsu_ontological_commitment__partition_reading | 0.770 | nearby_source | 1 | ~ |
| shinbutsu_ontological_substrate__syncretic_fusion_reading | 0.707 | nearby_source | 1 | ~ |

**Hop distance summary**:
- Within 1 hop of a contamination source: 48/63 sound constraints
- Within 2 hops: 60/63
- Within 3 hops: 63/63

### Contamination Collapse Analysis

At what contamination settings would sound constraints in the giant component collapse into the degraded zone?

Current settings: cap=0.30, attenuation=0.50
Sound constraints in giant component: 63

**Purity coverage**: 661 of 662 giant-component members are banded below; 1 excluded from the bands (no effective purity, non-numeric, or numeric below the 0.0 floor).

Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at 0.50):

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 63 | 136 | 455 | 7 |
| 0.20 | 63 | 136 | 455 | 7 |
| 0.30 | 63 | 136 | 455 | 7 |
| 0.40 | 63 | 136 | 455 | 7 |
| 0.50 | 63 | 136 | 455 | 7 |
| 0.60 | 63 | 136 | 455 | 7 |
| 0.70 | 63 | 136 | 455 | 7 |
| 0.80 | 63 | 136 | 455 | 7 |
| 0.90 | 63 | 136 | 455 | 7 |
| 1.00 | 63 | 136 | 455 | 7 |

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 1829 edges, 89 components, largest = 662 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 17 |
| rope | 790 | 84 | 49 |
| scaffold | 49 | 47 | 30 |
| tangled_rope | 29 | 251 | 165 |
| piton | 42 | 41 | 41 |
| snare | 24 | 507 | 540 |
| naturalized | 60 | 4 | 2 |
| unknown | 2 | 62 | 152 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 24 | 42 | 29 | 49 | 144 |
| Moderate/National | 507 | 41 | 251 | 47 | 846 |
| Analytical/Global (default) | 540 | 41 | 165 | 30 | 776 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 1829, 89, 662, 0.665).
```

---

## Provenance split (OQ-193)

*Pooled topology counts within-kernel reading-plurality (sibling `affects_constraint` edges) as coupling. The stratum strips explicit same-kernel sibling edges (retract-recompute) to expose cross-kernel structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine topology — this is a presentation split only, no engine-behavior change.*

**Sibling edges stripped**: 2004  
**same_kernel_edges_surviving**: 0 (dedup-resurfaced 0, never-stripped 0)  
**Positive control**: ok — raw `affects_constraint` dropped by exactly 2004.

| Stratum | Edges | Components | Giant size | Giant fraction |
|---------|-------|------------|------------|----------------|
| Pooled | 1829 | 89 | 662 | 0.665 |
| Cross-kernel | 806 | 582 | 233 | 0.234 |

---

*End of giant component analysis*
