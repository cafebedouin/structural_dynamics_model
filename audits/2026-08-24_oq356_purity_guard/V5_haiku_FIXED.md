
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
| Total nodes (constraints) | 960 |
| Connected nodes (degree > 0) | 955 |
| Isolated nodes (degree 0) | 5 |
| Edges | 1602 |
| Graph density | 0.003480 |
| Average degree | 3.34 |
| Connected components | 95 |
| E-R critical edge count (n/2) | 480.0 |

### Degree Distribution

| Stat | Value |
|------|-------|
| N | 960 |
| Min | 0 |
| Q1 | 2 |
| Median | 2 |
| Q3 | 4 |
| Max | 16 |
| Mean | 3.34 |

#### Degree Histogram

| Degree Range | Count |
|-------------|-------|
| 0 (isolated) | 5 |
| 1 | 57 |
| 2-3 | 621 |
| 4-6 | 188 |
| 7-10 | 65 |
| 11-20 | 24 |
| 21+ | 0 |

### Connected Components

**95 components** found.

**Largest component**: 589 nodes (61.4% of network)

**Giant component detected.** The largest component contains >50% of all nodes.

#### Top Components by Size

| Rank | Size | Fraction |
|------|------|----------|
| 1 | 589 | 0.614 |
| 2 | 35 | 0.036 |
| 3 | 20 | 0.021 |
| 4 | 13 | 0.014 |
| 5 | 12 | 0.013 |
| 6 | 11 | 0.011 |
| 7 | 10 | 0.010 |
| 8 | 9 | 0.009 |
| 9 | 9 | 0.009 |
| 10 | 8 | 0.008 |
| 11 | 6 | 0.006 |
| 12 | 6 | 0.006 |
| 13 | 6 | 0.006 |
| 14 | 5 | 0.005 |
| 15 | 5 | 0.005 |
### Type Distribution

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 15 | 0.016 |
| rope | 46 | 0.048 |
| scaffold | 36 | 0.037 |
| tangled_rope | 139 | 0.145 |
| piton | 36 | 0.037 |
| snare | 525 | 0.547 |
| naturalized | 1 | 0.001 |
| unknown | 162 | 0.169 |

### Purity Landscape

#### Intrinsic Purity (939/960 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.450 |
| Q3 | 0.575 |
| Max | 1.000 |
| Mean | 0.488 |

#### Effective Purity (939/960 constraints with valid scores)

| Stat | Value |
|------|-------|
| Min | 0.271 |
| Q1 | 0.354 |
| Median | 0.442 |
| Q3 | 0.558 |
| Max | 1.000 |
| Mean | 0.480 |

#### Purity Zone Distribution

| Zone | Intrinsic | Effective | Shift |
|------|-----------|-----------|-------|
| Sound (>= 0.70) | 104 | 95 | 9 |
| Borderline (0.50 - 0.70) | 221 | 199 | 22 |
| Warning (0.30 - 0.50) | 604 | 634 | -30 |
| Degraded (< 0.30) | 10 | 11 | -1 |

**36 constraints shifted purity zone** due to network contamination effects.

### Super-spreaders (Highest Contamination Potential)

| Constraint | Type | Degree | Contam Str | Eff Purity | Potential |
|------------|------|--------|------------|------------|-----------|
| climate_response_imperative__degrowth_reading | snare | 14 | 1.00 | 0.354 | 14.00 |
| climate_harm_prevention__degrowth_reading | snare | 13 | 1.00 | 0.354 | 13.00 |
| climate_response_obligation__adaptation_priority | snare | 12 | 1.00 | 0.413 | 12.00 |
| article17_erasure_right__censorship_mechanism_reading | snare | 11 | 1.00 | 0.354 | 11.00 |
| supermajority_threshold__consensus_safeguard_reading | piton | 13 | 0.80 | 0.648 | 10.40 |
| border_control_legitimacy__sovereignty_primary | snare | 10 | 1.00 | 0.349 | 10.00 |
| second_amendment_arms_right__collective_right_reading | piton | 12 | 0.80 | 0.766 | 9.60 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 9 | 1.00 | 0.351 | 9.00 |
| catastrophe_avoidance_retention__catastrophe_as_necessary_selector | snare | 8 | 1.00 | 0.402 | 8.00 |
| ai_human_relationship__instrumental_subsidiarity | snare | 7 | 1.00 | 0.312 | 7.00 |
| constitutional_interpretive_authority__coordinate_construction_reading | piton | 8 | 0.80 | 0.578 | 6.40 |
| border_legitimacy__freedom_of_movement_reading | snare | 6 | 1.00 | 0.489 | 6.00 |
| shinbutsu_coexistence_commitment__domain_partition_reading | piton | 7 | 0.80 | 0.404 | 5.60 |
| acceptable_risk_for_energy__expected_value_dominant | tangled_rope | 11 | 0.50 | 0.387 | 5.50 |
| acceptable_risk_energy__expected_value_dominant | snare | 5 | 1.00 | 0.351 | 5.00 |
| legitimacy_of_practice_standardization__endogenous_displacement_reading | piton | 6 | 0.80 | 0.757 | 4.80 |
| acceptable_risk_for_energy__comparative_risk_dominant | snare | 4 | 1.00 | 0.354 | 4.00 |
| basic_law_interpretive_authority__popular_constitutionalism_reading | tangled_rope | 7 | 0.50 | 0.563 | 3.50 |
| ai_human_relationship__incarnational_humanism | piton | 4 | 0.80 | 0.633 | 3.20 |
| ai_alignment_priority__existential_risk_reading | snare | 3 | 1.00 | 0.575 | 3.00 |


---

## Phase 2: Threshold Sweep (Erdos-Renyi Phase Transition)

**No inferred coupling edges** in the corpus (0 constraints with gradient data).
Threshold sweep is degenerate: all thresholds produce the same edge set (only `explicit` and `shared_agent` edges survive regardless of threshold).

| Threshold | Edges | Components | Largest | Fraction |
|-----------|-------|------------|---------|----------|
| 0.500 (all) | 1602 | 95 | 589 | 0.614 |


---

## Phase 3: Contamination Through the Giant Component

**Threshold**: 0.500 (default)

**Giant component size**: 589 nodes (61.4% of network)

### Giant Component Composition

| Type | Count | Fraction |
|------|-------|----------|
| mountain | 8 | 0.014 |
| rope | 21 | 0.036 |
| scaffold | 23 | 0.039 |
| tangled_rope | 85 | 0.144 |
| piton | 23 | 0.039 |
| snare | 325 | 0.552 |
| naturalized | 1 | 0.002 |
| unknown | 103 | 0.175 |

#### Purity Within Giant Component

- Coverage: intrinsic 579/589 scorable, effective 579/589 scorable
- **Intrinsic**: min=0.271, median=0.450, max=1.000, mean=0.484
- **Effective**: min=0.271, median=0.439, max=1.000, mean=0.472

- **Active contamination sources** (intrinsic purity < 0.50): 380
- **Sound constraints** (effective purity >= 0.70): 51

### Contamination Sources (Super-spreaders in Giant Component)

**35 contamination-capable nodes** in the giant component.

| Constraint | Type | Intra-GC Degree | Contam Str | Eff Purity | Potential |
|------------|------|-----------------|------------|------------|-----------|
| climate_response_imperative__degrowth_reading | snare | 14 | 1.00 | 0.354 | 14.00 |
| climate_harm_prevention__degrowth_reading | snare | 13 | 1.00 | 0.354 | 13.00 |
| climate_response_obligation__adaptation_priority | snare | 12 | 1.00 | 0.413 | 12.00 |
| article17_erasure_right__censorship_mechanism_reading | snare | 11 | 1.00 | 0.354 | 11.00 |
| supermajority_threshold__consensus_safeguard_reading | piton | 13 | 0.80 | 0.648 | 10.40 |
| border_control_legitimacy__sovereignty_primary | snare | 10 | 1.00 | 0.349 | 10.00 |
| second_amendment_arms_right__collective_right_reading | piton | 12 | 0.80 | 0.766 | 9.60 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 9 | 1.00 | 0.351 | 9.00 |
| catastrophe_avoidance_retention__catastrophe_as_necessary_selector | snare | 8 | 1.00 | 0.402 | 8.00 |
| ai_human_relationship__instrumental_subsidiarity | snare | 7 | 1.00 | 0.312 | 7.00 |
| constitutional_interpretive_authority__coordinate_construction_reading | piton | 8 | 0.80 | 0.578 | 6.40 |
| border_legitimacy__freedom_of_movement_reading | snare | 6 | 1.00 | 0.489 | 6.00 |
| shinbutsu_coexistence_commitment__domain_partition_reading | piton | 7 | 0.80 | 0.404 | 5.60 |
| acceptable_risk_for_energy__expected_value_dominant | tangled_rope | 11 | 0.50 | 0.387 | 5.50 |
| acceptable_risk_energy__expected_value_dominant | snare | 5 | 1.00 | 0.351 | 5.00 |
| legitimacy_of_practice_standardization__endogenous_displacement_reading | piton | 6 | 0.80 | 0.757 | 4.80 |
| acceptable_risk_for_energy__comparative_risk_dominant | snare | 4 | 1.00 | 0.354 | 4.00 |
| basic_law_interpretive_authority__popular_constitutionalism_reading | tangled_rope | 7 | 0.50 | 0.563 | 3.50 |
| ai_human_relationship__incarnational_humanism | piton | 4 | 0.80 | 0.633 | 3.20 |
| ai_alignment_priority__existential_risk_reading | snare | 3 | 1.00 | 0.575 | 3.00 |

### Multi-hop Contamination Simulation

Simulating contamination propagation beyond the current one-hop model.
Attenuation: 0.50 per hop. Stop when attenuation * strength < 0.01.

**311 active contamination sources** (type strength >= 0.5, purity < 0.50)

| Source | Type | Purity | 1-hop | 2-hop | 3-hop | Total Reach |
|--------|------|--------|-------|-------|-------|-------------|
| climate_response_imperative__degrowth_reading | snare | 0.354 | 14 | 23 | 30 | 67 |
| climate_harm_prevention__degrowth_reading | snare | 0.354 | 13 | 24 | 30 | 67 |
| climate_response_obligation__degrowth_reading | snare | 0.354 | 12 | 26 | 31 | 69 |
| article17_erasure_right__censorship_mechanism_reading | snare | 0.354 | 11 | 8 | 5 | 24 |
| border_control_legitimacy__sovereignty_primary | snare | 0.354 | 10 | 13 | 14 | 37 |
| acceptable_risk_for_energy__catastrophic_tail_dominant | snare | 0.354 | 9 | 20 | 20 | 49 |
| catastrophe_avoidance_retention__catastrophe_as_necessary_selector | snare | 0.429 | 8 | 19 | 16 | 43 |
| ai_human_relationship__instrumental_subsidiarity | snare | 0.312 | 7 | 16 | 15 | 38 |
| competence_retention_exercise__simulation_as_sufficient | snare | 0.382 | 6 | 7 | 12 | 25 |
| shinbutsu_coexistence_commitment__domain_partition_reading | piton | 0.408 | 7 | 5 | 4 | 16 |
| acceptable_risk_for_energy__expected_value_dominant | tangled_rope | 0.482 | 11 | 24 | 22 | 57 |
| acceptable_risk_energy__expected_value_dominant | snare | 0.354 | 5 | 9 | 23 | 37 |
| acceptable_risk_for_energy__comparative_risk_dominant | snare | 0.354 | 4 | 12 | 28 | 44 |
| employment_boundary__hybrid_security_reading | tangled_rope | 0.498 | 7 | 7 | 7 | 21 |
| ai_risk_governance_priority__near_term_harms_reading | snare | 0.312 | 3 | 15 | 30 | 48 |
| climate_mitigation_legitimacy__renewable_primacy_reading | tangled_rope | 0.482 | 5 | 7 | 26 | 38 |
| article17_erasure_right__privacy_fundamental_reading | piton | 0.449 | 3 | 11 | 7 | 21 |
| acceptable_risk_energy__catastrophic_tail_dominant | snare | 0.354 | 2 | 3 | 9 | 14 |
| catastrophe_memory_preservation__hybrid_atrophy_reading | piton | 0.408 | 2 | 10 | 19 | 31 |
| bretton_woods_treaty_substrate__keynesian_embedded_liberalism | tangled_rope | 0.450 | 3 | 5 | 10 | 18 |
| acceptable_risk_energy__option_value_preserving | tangled_rope | 0.498 | 2 | 3 | 9 | 14 |

**Total unique nodes reached** within 3 hops of any source: 275 (46.7% of giant component)

### Sound Constraint Exposure to Contamination

**51 sound constraints** (effective purity >= 0.70) in the giant component.

*Showing first 50 of 51 sound constraints.*

| Sound Constraint | Eff Purity | Nearest Source | Distance | Would Cross Threshold? |
|------------------|------------|----------------|----------|----------------------|
| ai_governance_legitimacy__market_libertarian_reading | 0.936 | nearby_source | 2 | ~ |
| aneyoshi_land_use_prohibition__behavioral_competence_reading | 0.998 | nearby_source | 3 | ~ |
| aneyoshi_stone_commitment__commemorative_husk_reading | 0.860 | nearby_source | 2 | ~ |
| beta_designation_doctrine__severity_carve_out_reading | 0.770 | nearby_source | 1 | ~ |
| combatant_status_definition__functional_protection_reading | 0.864 | nearby_source | 1 | ~ |
| constitutional_authority_boundary__parliamentary_primacy_reading | 0.767 | nearby_source | 1 | ~ |
| correct_latin__continuity_reading | 0.749 | nearby_source | 1 | ~ |
| derivative_work_statutory_boundary__coordination_reading | 0.746 | nearby_source | 1 | ~ |
| dueling_disappearance_mechanism__institutional_displacement_reading | 0.885 | nearby_source | 1 | ~ |
| hebrew_living_language__literary_revival_reading | 0.972 | nearby_source | 1 | ~ |
| hebrew_living_language__liturgical_continuity_reading | 0.992 | nearby_source | 1 | ~ |
| humane_treatment_standard__absolute_prohibition | 0.892 | nearby_source | 1 | ~ |
| income_support_commitment__freedom_floor_reading | 0.973 | nearby_source | 1 | ~ |
| income_support_conditionality__freedom_floor_reading | 0.978 | nearby_source | 1 | ~ |
| kodashim_commandment_status__study_as_performance | 1.000 | nearby_source | 1 | ~ |
| kodashim_corpus__study_as_exercise | 1.000 | nearby_source | 1 | ~ |
| kodashim_obligation__study_as_performance | 1.000 | nearby_source | 2 | ~ |
| kodashim_obligation__study_as_preparation | 0.806 | nearby_source | 3 | ~ |
| latin_correctness__continuity_reading | 0.974 | nearby_source | 1 | ~ |
| legitimacy_of_practice_standardization__endogenous_displacement_reading | 0.757 | nearby_source | 1 | ~ |
| maat_order_principle__distributed_maintenance_reading | 0.924 | nearby_source | 2 | ~ |
| magna_carta_1215__universal_rights_reading | 0.794 | nearby_source | 2 | ~ |
| magna_carta_clause_39__feudal_prerogative_reading | 0.754 | nearby_source | 2 | ~ |
| marriage_commitment_legitimacy__endogenous_reinterpretation_reading | 0.707 | nearby_source | 1 | ~ |
| nafta_jurisdictional_boundary__embedded_liberalism_reading | 0.857 | nearby_source | 2 | ~ |
| nafta_jurisdictional_boundary__sovereignty_primacy_reading | 0.782 | nearby_source | 2 | ~ |
| notability_guidelines__deletionist_reading | 0.836 | nearby_source | 2 | ~ |
| npt_article_iv_vi_pairing__abolitionist | 0.800 | nearby_source | 1 | ~ |
| nuclear_impossibility_kernel__structural_contraction_reading | 0.769 | nearby_source | 1 | ~ |
| ost_article_ii_non_appropriation__international_regime | 0.781 | nearby_source | 1 | ~ |
| permissive_license_text__commons_coordination_reading | 0.984 | nearby_source | 3 | ~ |
| preparedness_transmission__competence_reading | 0.762 | nearby_source | 1 | ~ |
| sacrifice_commandment__study_as_performance | 1.000 | nearby_source | 1 | ~ |
| sacrifice_obligation_continuity__study_as_performance | 0.952 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__study_as_exercise_reading | 1.000 | nearby_source | 2 | ~ |
| sacrifice_obligation_kernel__symbolic_archive_reading | 1.000 | nearby_source | 2 | ~ |
| second_amendment_arms_right__collective_right_reading | 0.766 | nearby_source | 1 | ~ |
| second_amendment_text__originalist_civic_virtue_reading | 0.770 | nearby_source | 1 | ~ |
| shinbutsu_ontological_commitment__partition_reading | 0.747 | nearby_source | 1 | ~ |
| simultaneous_veneration__domain_partition_reading | 0.992 | nearby_source | 1 | ~ |
| software_control_legitimacy__pragmatic_openness_reading | 0.782 | nearby_source | 1 | ~ |
| statutory_debt_ceiling__constitutional_nullity_reading | 1.000 | nearby_source | 2 | ~ |
| temple_sacrifice_commitment__performance_only | 0.968 | nearby_source | 1 | ~ |
| temple_sacrifice_commitment__study_as_exercise | 1.000 | nearby_source | 1 | ~ |
| temple_sacrifice_obligation__messianic_suspension | 0.896 | nearby_source | 1 | ~ |
| temple_sacrifice_obligation__study_as_occupation | 0.941 | nearby_source | 1 | ~ |
| total_war_possibility_space__space_contraction_reading | 0.984 | nearby_source | 1 | ~ |
| udhr_authority__aspirational_sovereignty_reading | 0.754 | nearby_source | 1 | ~ |
| unconditional_income_support__freedom_floor_reading | 0.806 | nearby_source | 2 | ~ |
| war_winnability_post_1945__deterrence_unthinkable | 0.798 | nearby_source | 1 | ~ |

**Hop distance summary**:
- Within 1 hop of a contamination source: 34/51 sound constraints
- Within 2 hops: 48/51
- Within 3 hops: 51/51

### Contamination Collapse Analysis

At what contamination settings would sound constraints in the giant component collapse into the degraded zone?

Current settings: cap=0.30, attenuation=0.50
Sound constraints in giant component: 51

**Purity coverage**: 579 of 589 giant-component members have a numeric effective purity; 10 excluded (members with no numeric effective purity).

Sweeping contamination_cap from 0.10 to 1.00 (attenuation fixed at 0.50):

| Cap | Sound (>=0.70) | Borderline | Warning | Degraded (<0.30) |
|-----|--------|------------|---------|---------|
| 0.10 | 51 | 124 | 394 | 10 |
| 0.20 | 51 | 124 | 394 | 10 |
| 0.30 | 51 | 124 | 394 | 10 |
| 0.40 | 51 | 124 | 394 | 10 |
| 0.50 | 51 | 124 | 394 | 10 |
| 0.60 | 51 | 124 | 394 | 10 |
| 0.70 | 51 | 124 | 394 | 10 |
| 0.80 | 51 | 124 | 394 | 10 |
| 0.90 | 51 | 124 | 394 | 10 |
| 1.00 | 51 | 124 | 394 | 10 |

---

## Phase 4: Context Comparison

The edge set is context-independent (edges come from `affects_constraint`, `infer_structural_coupling`, and `shared_agent_link` — none of which depend on observer context). What changes across contexts is the **type classification** and hence the **contamination dynamics**.

**Fixed topology**: 1602 edges, 95 components, largest = 589 nodes (threshold = 0.500)

### Type Distribution by Context

| Type | Institutional/Local | Moderate/National | Analytical/Global (default) |
|------|------|------|------|
| mountain | 0 | 0 | 15 |
| rope | 722 | 81 | 46 |
| scaffold | 68 | 60 | 36 |
| tangled_rope | 40 | 218 | 139 |
| piton | 39 | 36 | 36 |
| snare | 24 | 481 | 525 |
| naturalized | 60 | 6 | 1 |
| unknown | 7 | 78 | 162 |

### Contamination Source Comparison

Number of constraints that are active contamination sources (type strength > 0, acts as contamination emitter) by context:

| Context | Snare | Piton | Tangled Rope | Scaffold | Total Sources |
|---------|-------|-------|-------------|----------|---------------|
| Institutional/Local | 24 | 39 | 40 | 68 | 171 |
| Moderate/National | 481 | 36 | 218 | 60 | 795 |
| Analytical/Global (default) | 525 | 36 | 139 | 36 | 736 |

### Key Finding

Since edges are context-independent, the network topology (connected components, component sizes, degree distribution) is identical across all contexts. What changes is WHICH nodes are contamination sources. A constraint classified as a snare from one context (high contamination strength = 1.0) may be classified as a rope from another (low strength = 0.1). This means the effective contamination pressure varies by context even though the network structure does not.

---

## Embedded Prolog Facts

```prolog
%% Sweep results: gc_sweep_result(Threshold, NEdges, NComponents, LargestSize, LargestFraction)
gc_sweep_result(0.500, 1602, 95, 589, 0.614).
```

---

## Provenance split (OQ-193)

*Pooled topology counts within-kernel reading-plurality (sibling `affects_constraint` edges) as coupling. The stratum strips explicit same-kernel sibling edges (retract-recompute) to expose cross-kernel structure. Operator ruling (c), 2026-07-02: siblings STAY in the engine topology — this is a presentation split only, no engine-behavior change.*

**Sibling edges stripped**: 1818  
**same_kernel_edges_surviving**: 0 (dedup-resurfaced 0, never-stripped 0)  
**Positive control**: ok — raw `affects_constraint` dropped by exactly 1818.

| Stratum | Edges | Components | Giant size | Giant fraction |
|---------|-------|------------|------------|----------------|
| Pooled | 1602 | 95 | 589 | 0.614 |
| Cross-kernel | 659 | 586 | 102 | 0.106 |

---

*End of giant component analysis*
