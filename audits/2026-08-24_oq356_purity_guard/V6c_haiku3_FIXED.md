
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
| Total nodes (constraints) | 993 |
| Connected nodes (degree > 0) | 991 |
| Isolated nodes (degree 0) | 2 |
| Edges | 1697 |
| Graph density | 0.003445 |
| Average degree | 3.42 |
| Connected components | 91 |
| E-R critical edge count (n/2) | 496.5 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 993 |
| Min | 0 |
| Q1 | 2 |
| Median | 3 |
| Q3 | 4 |
| Max | 21 |
| Mean | 3.42 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 2 |
| 1 | 41 |
| 2-3 | 632 |
| 4-6 | 243 |
| 7-10 | 52 |
| 11-20 | 22 |
| 21+ | 1 |

### Connected Components

**91 components** found.

**Largest component**: 629 nodes (63.3% of network)

**Giant component detected.** The largest component contains >50% of all nodes.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 629 | 0.633 |
| 2 | 14 | 0.014 |
| 3 | 12 | 0.012 |
| 4 | 11 | 0.011 |
| 5 | 11 | 0.011 |
| 6 | 10 | 0.010 |
| 7 | 9 | 0.009 |
| 8 | 9 | 0.009 |
| 9 | 8 | 0.008 |
| 10 | 8 | 0.008 |
| 11 | 7 | 0.007 |
| 12 | 7 | 0.007 |
| 13 | 6 | 0.006 |
| 14 | 6 | 0.006 |
| 15 | 6 | 0.006 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 18 | 0.018 |
| rope | 55 | 0.055 |
| scaffold | 29 | 0.029 |
| tangled_rope | 154 | 0.155 |
| piton | 37 | 0.037 |
| snare | 531 | 0.535 |
| naturalized | 1 | 0.001 |
| unknown | 168 | 0.169 |

### Purity Landscape

#### Intrinsic Purity (990/993 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.458 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.495 |

#### Effective Purity (990/993 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.450 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.487 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 116 | 106 | 10 |
| Borderline (0.50 - 0.70) | 247 | 225 | 22 |
| Warning (0.30 - 0.50) | 617 | 649 | -32 |
| Degraded (< 0.30) | 10 | 10 | 0 |

**42 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| technology_legitimacy_kernel__precautionary_reading | snare | 21 | 1.00 | 0.349 | 21.00 |
| ai_alignment_priority__existential_risk_reading | snare | 18 | 1.00 | 0.348 | 18.00 |
| coercion_legitimacy_boundary__proportionality_reading | snare | 15 | 1.00 | 0.326 | 15.00 |
| climate_response_action__degrowth_transformation | snare | 14 | 1.00 | 0.391 | 14.00 |
| border_control_legitimacy__freedom_of_movement_primary | snare | 13 | 1.00 | 0.424 | 13.00 |
| climate_response_obligation__degrowth_reading | snare | 12 | 1.00 | 0.482 | 12.00 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 11 | 1.00 | 0.387 | 11.00 |
| catastrophe_memory_transmission__operational_competence_reading | piton | 13 | 0.80 | 0.687 | 10.40 |
| climate_response_action__mitigation_priority | snare | 10 | 1.00 | 0.327 | 10.00 |
| ai_risk_prioritization__near_term_harms_reading | snare | 9 | 1.00 | 0.312 | 9.00 |
| beta_designation_doctrine__expansive_shield_reading | snare | 8 | 1.00 | 0.451 | 8.00 |
| ai_human_relationship__instrumental_subsidiarity | tangled_rope | 14 | 0.50 | 0.406 | 7.00 |
| ai_risk_prioritization__existential_risk_reading | tangled_rope | 13 | 0.50 | 0.384 | 6.50 |
| udhr_authority__aspirational_sovereignty_reading | piton | 8 | 0.80 | 0.682 | 6.40 |
| acceptable_risk_energy__expected_value_dominant | snare | 6 | 1.00 | 0.312 | 6.00 |
| territorial_legitimacy_dual__two_state_coexistence_reading | piton | 7 | 0.80 | 0.504 | 5.60 |
| digital_money_emergence_boundary__consumer_holdings_reading | tangled_rope | 11 | 0.50 | 0.433 | 5.50 |
| ai_alignment_commitment__integrated_reading | snare | 5 | 1.00 | 0.312 | 5.00 |
| constitutional_interpretive_authority__coordinate_construction_reading | piton | 6 | 0.80 | 0.463 | 4.80 |
| ai_governance_legitimacy__magisterial_subsidiarity_reading | tangled_rope | 9 | 0.50 | 0.481 | 4.50 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 1697 | 91 | 629 | 0.633 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**Giant component size**: 629 nodes (63.3% of network)

### Giant Component Composition

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 12 | 0.019 |
| rope | 28 | 0.045 |
| scaffold | 21 | 0.033 |
| tangled_rope | 103 | 0.164 |
| piton | 23 | 0.037 |
| snare | 331 | 0.526 |
| naturalized | 1 | 0.002 |
| unknown | 110 | 0.175 |

#### Purity Within Giant Component

- Coverage: intrinsic 627/629 scorable, effective 627/629 scorable
- **Intrinsic**: min=0.271, median=0.458, max=1.000, mean=0.492
- **Effective**: min=0.271, median=0.450, max=1.000, mean=0.481

- **Active contamination sources** (intrinsic purity < 0.50): 405
- **Sound constraints** (effective purity >= 0.70): 61

### Contamination Sources (Super-spreaders in Giant Component)

**40 contamination-capable nodes** in the giant component.

| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |
|------------|------|-----------------|------------|------------|-----------|
| technology_legitimacy_kernel__precautionary_reading | snare | 21 | 1.00 | 0.349 | 21.00 |
| ai_alignment_priority__existential_risk_reading | snare | 18 | 1.00 | 0.348 | 18.00 |
| coercion_legitimacy_boundary__proportionality_reading | snare | 15 | 1.00 | 0.326 | 15.00 |
| climate_response_action__degrowth_transformation | snare | 14 | 1.00 | 0.391 | 14.00 |
| border_control_legitimacy__freedom_of_movement_primary | snare | 13 | 1.00 | 0.424 | 13.00 |
| climate_response_obligation__degrowth_reading | snare | 12 | 1.00 | 0.482 | 12.00 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 11 | 1.00 | 0.387 | 11.00 |
| catastrophe_memory_transmission__operational_competence_reading | piton | 13 | 0.80 | 0.687 | 10.40 |
| climate_response_action__mitigation_priority | snare | 10 | 1.00 | 0.327 | 10.00 |
| ai_risk_prioritization__near_term_harms_reading | snare | 9 | 1.00 | 0.312 | 9.00 |
| beta_designation_doctrine__expansive_shield_reading | snare | 8 | 1.00 | 0.451 | 8.00 |
| ai_human_relationship__instrumental_subsidiarity | tangled_rope | 14 | 0.50 | 0.406 | 7.00 |
| ai_risk_prioritization__existential_risk_reading | tangled_rope | 13 | 0.50 | 0.384 | 6.50 |
| udhr_authority__aspirational_sovereignty_reading | piton | 8 | 0.80 | 0.682 | 6.40 |
| acceptable_risk_energy__expected_value_dominant | snare | 6 | 1.00 | 0.312 | 6.00 |
| territorial_legitimacy_dual__two_state_coexistence_reading | piton | 7 | 0.80 | 0.504 | 5.60 |
| digital_money_emergence_boundary__consumer_holdings_reading | tangled_rope | 11 | 0.50 | 0.433 | 5.50 |
| ai_alignment_commitment__integrated_reading | snare | 5 | 1.00 | 0.312 | 5.00 |
| constitutional_interpretive_authority__coordinate_construction_reading | piton | 6 | 0.80 | 0.463 | 4.80 |
| ai_governance_legitimacy__magisterial_subsidiarity_reading | tangled_rope | 9 | 0.50 | 0.481 | 4.50 |

### Multi-hop Contamination Simulation

Simulating contamination propagation beyond the current one-hop model.
Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.

**329 active contamination sources** (type strength >= 0.5, purity < 0.50)

| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |
|--------|------|--------|-------|-------|-------|-------------|
| technology_legitimacy_kernel__precautionary_reading | snare | 0.354 | 21 | 23 | 43 | 87 |
| ai_alignment_priority__existential_risk_reading | snare | 0.354 | 18 | 36 | 33 | 87 |
| coercion_legitimacy_boundary__proportionality_reading | snare | 0.329 | 15 | 17 | 24 | 56 |
| climate_response_action__degrowth_transformation | snare | 0.429 | 14 | 25 | 29 | 68 |
| climate_response_legitimacy__degrowth_transformation | snare | 0.370 | 13 | 22 | 28 | 63 |
| climate_response_obligation__mitigation_priority | snare | 0.354 | 12 | 23 | 28 | 63 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 0.429 | 11 | 26 | 47 | 84 |
| climate_response_action__mitigation_priority | snare | 0.328 | 10 | 23 | 46 | 79 |
| ai_risk_prioritization__near_term_harms_reading | snare | 0.312 | 9 | 37 | 63 | 109 |
| beta_designation_doctrine__expansive_shield_reading | snare | 0.458 | 8 | 16 | 40 | 64 |
| ai_human_relationship__instrumental_subsidiarity | tangled_rope | 0.482 | 14 | 47 | 56 | 117 |
| ai_risk_prioritization__existential_risk_reading | tangled_rope | 0.458 | 13 | 46 | 48 | 107 |
| acceptable_risk_energy__expected_value_dominant | snare | 0.312 | 6 | 15 | 21 | 42 |
| digital_money_emergence_boundary__consumer_holdings_reading | tangled_rope | 0.486 | 11 | 22 | 18 | 51 |
| ai_alignment_commitment__integrated_reading | snare | 0.312 | 5 | 21 | 32 | 58 |
| constitutional_interpretive_authority__coordinate_construction_reading | piton | 0.474 | 6 | 9 | 6 | 21 |
| jewish_sovereignty_palestine__liberal_nationalist_reading | tangled_rope | 0.482 | 9 | 15 | 8 | 32 |
| ai_safety_commitment__dual_priority_reading | snare | 0.388 | 4 | 9 | 21 | 34 |
| competence_occupation__hybrid_occupation | tangled_rope | 0.417 | 7 | 3 | 3 | 13 |
| divine_legitimacy_substrate__folk_syncretistic_reading | piton | 0.408 | 4 | 5 | 6 | 15 |
| ai_alignment_commitment__ethics_justice_reading | snare | 0.354 | 3 | 11 | 50 | 64 |
| basic_law_interpretive_authority__judicial_supremacy_reading | tangled_rope | 0.458 | 5 | 11 | 16 | 32 |
| press_reformation_causation__mutual_shaping | piton | 0.461 | 3 | 4 | 1 | 8 |
| abrahamic_covenant__ishmael_covenant_reading | snare | 0.366 | 2 | 4 | 16 | 22 |
| catastrophe_memory_transmission__hybrid_embedded_reading | piton | 0.408 | 2 | 11 | 22 | 35 |
| acceptable_risk_for_energy__expected_value_dominant | tangled_rope | 0.474 | 3 | 13 | 27 | 43 |
| acceptable_risk_energy__option_value_preserving | tangled_rope | 0.482 | 2 | 4 | 15 | 21 |

**Total unique nodes reached** within 3 hops of any source: 300 (47.7% of giant component)

### Sound Constraint Exposure to Contamination

**61 sound constraints** (effective purity >= 0.70) in the giant component.

*Showing first 50 of 61 sound constraints.*

| Sound Constraint | Eff Purity | Nearest Source | Distance | Would Cross Threshold? |
|------------------|------------|----------------|----------|----------------------|
| ai_dignity_safeguarding__posthuman_continuity_reading | 0.868 | nearby_source | 1 | ~ |
| ai_governance_legitimacy__market_libertarian_reading | 0.818 | nearby_source | 1 | ~ |
| animal_moral_status__property_reading | 0.856 | nearby_source | 1 | ~ |
| animal_status__property_reading | 0.887 | nearby_source | 1 | ~ |
| basic_law_interpretive_boundary__parliamentary_sovereignty_reading | 0.860 | nearby_source | 1 | ~ |
| catastrophe_memory_kernel__symbol_continuity_reading | 0.782 | nearby_source | 1 | ~ |
| catastrophe_memory_preservation__mourning_practice_reading | 0.960 | nearby_source | 2 | ~ |
| combatant_status_definition__functional_protection_reading | 0.868 | nearby_source | 1 | ~ |
| electronic_money_emergence__became_thinkable_reading | 0.746 | nearby_source | 1 | ~ |
| gita_kurukshetra_discourse__gandhian_allegorical_reading | 0.976 | nearby_source | 2 | ~ |
| gita_kurukshetra_discourse__universalist_devotional_reading | 0.753 | nearby_source | 1 | ~ |
| hebrew_linguistic_life__marketplace_pidgin_reading | 0.746 | nearby_source | 1 | ~ |
| hebrew_living_language__literary_revival_reading | 0.868 | nearby_source | 2 | ~ |
| hebrew_living_language__liturgical_continuity_reading | 0.999 | nearby_source | 2 | ~ |
| hebrew_vitality__hybrid_continuity_reading | 0.889 | nearby_source | 1 | ~ |
| hebrew_vitality__liturgical_reading | 0.960 | nearby_source | 1 | ~ |
| human_dignity_ai_governance__pluralist_pragmatic_reading | 0.856 | nearby_source | 2 | ~ |
| ietf_openness_commitment__commons_stewardship_reading | 0.903 | nearby_source | 1 | ~ |
| imposition_mechanism_kernel__endogenous_climb_reading | 0.960 | nearby_source | 1 | ~ |
| income_support_conditionality__freedom_floor_reading | 0.807 | nearby_source | 1 | ~ |
| jati_practice_norm__orthodox_textual_reading | 0.800 | nearby_source | 1 | ~ |
| kodashim_corpus__study_as_exercise | 1.000 | nearby_source | 1 | ~ |
| magna_carta_1215__living_document_reading | 0.794 | nearby_source | 1 | ~ |
| magna_carta_clause_39__feudal_prerogative_reading | 0.744 | nearby_source | 1 | ~ |
| market_as_natural_default__lapsed_alternative_reading | 0.988 | nearby_source | 1 | ~ |
| monetary_anchor_principle__triffin_inevitability_reading | 1.000 | nearby_source | 1 | ~ |
| nafta_jurisdictional_boundary__sovereignty_primacy_reading | 0.782 | nearby_source | 1 | ~ |
| nicene_creed_authority__liturgical_habituation_reading | 0.712 | nearby_source | 1 | ~ |
| nicene_creed_authority__symbolic_confessional_reading | 0.976 | nearby_source | 1 | ~ |
| notability_guidelines__deletionist_reading | 0.984 | nearby_source | 2 | ~ |
| nuclear_impossibility_kernel__structural_contraction_reading | 1.000 | nearby_source | 1 | ~ |
| orthographic_legitimacy_kernel__continuity_reading | 1.000 | nearby_source | 1 | ~ |
| preparedness_commitment__competence_reading | 0.770 | nearby_source | 1 | ~ |
| preparedness_retention__competence_reading | 0.976 | nearby_source | 1 | ~ |
| press_reformation_causality__technological_determinism | 0.972 | nearby_source | 1 | ~ |
| press_reformation_causation__technological_determinism | 0.972 | nearby_source | 1 | ~ |
| qwerty_persistence_mechanism__naturalization_reading | 0.931 | nearby_source | 1 | ~ |
| sacrifice_commandment__archive_maintenance | 0.714 | nearby_source | 2 | ~ |
| sacrifice_commandment__study_as_performance | 1.000 | nearby_source | 2 | ~ |
| sacrifice_obligation_continuity__archival_preservation | 1.000 | nearby_source | 1 | ~ |
| sacrifice_obligation_continuity__study_as_performance | 0.976 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__messianic_suspension_reading | 0.988 | nearby_source | 3 | ~ |
| sacrifice_obligation_kernel__study_as_exercise_reading | 1.000 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__symbolic_archive_reading | 1.000 | nearby_source | 3 | ~ |
| script_as_identity__phonetic_instrumentalism_reading | 0.774 | nearby_source | 1 | ~ |
| shinbutsu_ontological_commitment__partition_reading | 0.853 | nearby_source | 1 | ~ |
| simultaneous_veneration__domain_partition_reading | 0.988 | nearby_source | 2 | ~ |
| software_control_legitimacy__pragmatic_openness_reading | 0.782 | nearby_source | 1 | ~ |
| statutory_debt_ceiling__constitutional_nullity_reading | 1.000 | nearby_source | 1 | ~ |
| statutory_debt_ceiling__coordination_scaffold_reading | 0.976 | nearby_source | 2 | ~ |

**Hop distance summary**:
- Within 1 hop of a contamination source: 46/61 sound constraints
- Within 2 hops: 59/61
- Within 3 hops: 61/61

### Contamination Collapse Analysis

At what contamination settings would sound constraints in the giant component collapse into the degraded zone?

Current settings: cap=0.30, attenuation=0.50
Sound constraints in giant component: 61

**Purity coverage**: 627 of 629 giant-component members have a numeric effective purity; 2 excluded (members with no numeric effective purity).

Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at 0.50):

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 61 | 135 | 425 | 6 |
| 0.20 | 61 | 135 | 425 | 6 |
| 0.30 | 61 | 135 | 425 | 6 |
| 0.40 | 61 | 135 | 425 | 6 |
| 0.50 | 61 | 135 | 425 | 6 |
| 0.60 | 61 | 135 | 425 | 6 |
| 0.70 | 61 | 135 | 425 | 6 |
| 0.80 | 61 | 135 | 425 | 6 |
| 0.90 | 61 | 135 | 425 | 6 |
| 1.00 | 61 | 135 | 425 | 6 |

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 1697 edges, 91 components, largest = 629 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 18 |
| rope | 782 | 90 | 55 |
| scaffold | 54 | 49 | 29 |
| tangled_rope | 33 | 225 | 154 |
| piton | 39 | 37 | 37 |
| snare | 17 | 506 | 531 |
| naturalized | 65 | 5 | 1 |
| unknown | 3 | 81 | 168 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 17 | 39 | 33 | 54 | 143 |
| Moderate/National | 506 | 37 | 225 | 49 | 817 |
| Analytical/Global (default) | 531 | 37 | 154 | 29 | 751 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 1697, 91, 629, 0.633).
```

---

## Provenance split (OQ-193)

*Pooled topology counts within-kernel reading-plurality (sibling `affects_constraint` edges) as coupling. The stratum strips explicit same-kernel sibling edges (retract-recompute) to expose cross-kernel structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine topology — this is a presentation split only, no engine-behavior change.*

**Sibling edges stripped**: 2000  
**same_kernel_edges_surviving**: 0 (dedup-resurfaced 0, never-stripped 0)  
**Positive control**: ok — raw `affects_constraint` dropped by exactly 2000.

| Stratum | Edges | Components | Giant size | Giant fraction |
|---------|-------|------------|------------|----------------|
| Pooled | 1697 | 91 | 629 | 0.633 |
| Cross-kernel | 678 | 607 | 162 | 0.163 |

---

*End of giant component analysis*
