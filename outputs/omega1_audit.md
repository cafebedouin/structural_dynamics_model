
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

============================================================
  FNL SHADOW MODE DIAGNOSTIC TRACE
============================================================

[STEP 1] Loading fnl_shadow_probe through pipeline...

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fnl_shadow_probe.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fnl_shadow_probe...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.5 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.5 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.5 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.5 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.5 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.5 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: fnl_shadow_probe...

>>> INITIATING DR-AUDIT SUITE: fnl_shadow_probe

[REPAIR] Auditing vectors for: fnl_shadow_probe...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fnl_shadow_probe (0-10)
Checking Interval: fnl_shadow_probe (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: fnl_shadow_probe

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] fnl_shadow_probe from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] fnl_shadow_probe from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX OK] fnl_shadow_probe from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: fnl_shadow_probe ===

[CONSTRAINT INVENTORY]
  - fnl_shadow_probe: hybrid_extraction (Intensity: 0.30)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 2 | Watch: 0

  fnl_shadow_probe:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.25,0.3)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4525,decline_signals,[coupling_above_threshold(1.0),excess_above_floor(0.15)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fnl_shadow_probe -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fnl_shadow_probe (ε=0.30):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.30 × 1.36 × 0.80 = 0.326
    moderate@national: d=0.700 f(d)=1.11 χ = 0.30 × 1.11 × 1.00 = 0.332
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.30 × -0.04 × 1.00 = -0.013
    analytical@global: d=0.720 f(d)=1.14 χ = 0.30 × 1.14 × 1.20 = 0.411

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fnl_shadow_probe ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.453 (contaminated)


--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: fnl_shadow_probe
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: tangled_rope (Matches Claim)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: fnl_shadow_probe]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  fnl_shadow_probe: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for fnl_shadow_probe: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([rope,tangled_rope]),excess_above_floor(0.15),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_apparatus
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.50

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: fnl_shadow_probe
    - Individual (Powerless): tangled_rope [d=0.900 f(d)=1.36 χ=0.33]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.34 (moderate)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: fnl_shadow_probe]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_perspectival_fnl_shadow_probe (conceptual)
     Question: Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...
     Source: gap(general_type_mismatch,tangled_rope,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_perspectival_fnl_shadow_probe (conceptual)
      Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_perspectival_fnl_shadow_probe] CONCEPTUAL CLARIFICATION
  │  Constraint: fnl_shadow_probe
  │  Gap: Constraint fnl_shadow_probe appears as tangled_rope to individuals but rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive fnl_shadow_probe
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

====================================================

============================================================
  DIAGNOSTIC RESULTS
============================================================

[STEP 2] Metric-based classification (pre-override):
  metric_based_type_indexed = tangled_rope

[STEP 3] Structural signature:
  constraint_signature = false_ci_rope

[STEP 4] Boltzmann compliance:
  boltzmann_compliant = non_compliant(1.0,0.3)

[STEP 5] Cross-index coupling:
  cross_index_coupling = 1.0

[STEP 6] Final classification (post-override via dr_type/3):
  dr_type = tangled_rope

============================================================
  OVERRIDE ANALYSIS
============================================================

  MetricType  = tangled_rope
  Signature   = false_ci_rope
  FinalType   = tangled_rope

  >>> NO OVERRIDE DETECTED <<<
  MetricType == FinalType. Either the override did not
  fire, or both paths agree on the same type.

============================================================
  OVERRIDE RULE STATUS
============================================================

  Rule NL  (natural_law -> mountain):      ACTIVE (maps to mountain)
  Rule FNL (false_natural_law -> tangled):  ACTIVE (maps to tangled_rope)
  Rule CIR (coupling_invariant -> rope):    ACTIVE (maps to rope)
  Rule FCR (false_ci_rope -> tangled):      ACTIVE (maps to tangled_rope)

============================================================
  END OF DIAGNOSTIC
============================================================

# Omega1 Audit: The 39 Unknown Constraints

*Investigates why 39 constraints classify as `unknown` at the*
*analytical/global context, diagnoses their blocking gates,*
*and recommends classification fixes.*

---

## Summary

- **Total corpus**: 469 constraints
- **Unknowns at analytical/global**: 39 (8.3%)
- **Unknowns at institutional/local**: 0
- **Unknowns at moderate/national**: 23
- **Moderate unknowns are a subset of analytical unknowns**: yes
- **Self-consistency check**: PASS (all 39 unknowns confirmed via classify_from_metrics/6)

## Metric Distribution of the Unknowns

| Metric | Min | Q1 | Median | Q3 | Max | Mean |
|--------|-----|----|---------|----|-----|------|
| BaseEps | 0.300 | 0.420 | 0.700 | 0.750 | 0.950 | 0.619 |
| Chi (analytical/global) | 0.411 | 0.575 | 0.959 | 1.027 | 1.301 | 0.848 |
| Suppression | 0.000 | 0.300 | 0.400 | 0.500 | 0.800 | 0.391 |
| Theater | 0.000 | 0.100 | 0.120 | 0.200 | 0.900 | 0.240 |

## Gap Region Distribution

| Region | Description | Count |
|--------|-------------|-------|
| region_a_chi_gap | Chi in (0.35, 0.40): between rope ceiling and tangled_rope floor | 0 |
| region_b_low_epsilon | Chi >= 0.40, BaseEps < 0.30: tangled_rope epsilon floor violation | 0 |
| region_c_low_suppression | Chi >= 0.40, Supp < 0.40: tangled_rope suppression floor violation | 17 |
| region_d_missing_flags | Chi >= 0.40, metrics met, missing structural flags | 6 |
| region_e_near_snare | Chi >= 0.66, BaseEps < 0.46: near-snare with low epsilon | 0 |
| region_f_other | Other / uncategorized | 16 |

### Region Membership

**region_c_low_suppression** (Chi >= 0.40, Supp < 0.40: tangled_rope suppression floor violation): 17

- `france_2027_presidential_election` (eps=0.480, chi=0.658, supp=0.380)
- `glp1_payload_efficiency_pivot` (eps=0.350, chi=0.479, supp=0.200)
- `gradient_descent_optimization` (eps=0.300, chi=0.411, supp=0.200)
- `hoa_covenants` (eps=0.550, chi=0.753, supp=0.300)
- `maha_recovery_2026` (eps=0.420, chi=0.575, supp=0.350)
- `pareto_principle` (eps=0.400, chi=0.548, supp=0.300)
- `paris_municipal_reform_2026` (eps=0.470, chi=0.644, supp=0.350)
- `platonic_coparenting_decoupling` (eps=0.480, chi=0.658, supp=0.350)
- `quantum_decryption_risk_2026` (eps=0.800, chi=1.096, supp=0.300)
- `recipe_scaling_ai` (eps=0.490, chi=0.671, supp=0.250)
- `regulatory_capture` (eps=0.800, chi=1.096, supp=0.200)
- `repair_probe_incomplete` (eps=0.550, chi=0.759, supp=0.000)
- `rotation_seven_black_soil` (eps=0.950, chi=1.301, supp=0.100)
- `shitty_feedback_handling` (eps=0.400, chi=0.548, supp=0.300)
- `skills_based_hiring` (eps=0.300, chi=0.411, supp=0.200)
- `trillion_bond_rush_2026` (eps=0.520, chi=0.712, supp=0.100)
- `unclos_2026` (eps=0.300, chi=0.411, supp=0.200)

**region_d_missing_flags** (Chi >= 0.40, metrics met, missing structural flags): 6

- `gamblers_ruin_stochastic_extinction` (eps=0.900, chi=1.233, supp=0.500)
- `gs1_standardized_identification` (eps=0.400, chi=0.552, supp=0.800)
- `hawthorne_effect` (eps=0.400, chi=0.548, supp=0.500)
- `hegemonic_entropy_2026` (eps=0.720, chi=0.986, supp=0.550)
- `qwerty_vs_dvorak` (eps=0.400, chi=0.548, supp=0.700)
- `russells_paradox_self_reference` (eps=0.700, chi=0.959, supp=0.400)

**region_f_other** (Other / uncategorized): 16

- `grete_samsa_transition` (eps=0.750, chi=1.027, supp=0.500)
- `hydra_game` (eps=0.700, chi=0.959, supp=0.500)
- `institutional_trust_decay` (eps=0.680, chi=0.932, supp=0.550)
- `insult_wisdom_training` (eps=0.750, chi=1.027, supp=0.500)
- `moltbook_agent_theater` (eps=0.700, chi=0.959, supp=0.450)
- `negative_emissions_arbitrage` (eps=0.820, chi=1.123, supp=0.550)
- `net_zero_stabilization` (eps=0.850, chi=1.164, supp=0.450)
- `neural_interoperability` (eps=0.850, chi=1.164, supp=0.500)
- `neurodiversity_spectrum` (eps=0.720, chi=0.986, supp=0.550)
- `royal_navy_middle_east_withdrawal` (eps=0.800, chi=1.096, supp=0.500)
- `rule_update_failure` (eps=0.720, chi=0.986, supp=0.580)
- `smartphone_ubiquity` (eps=0.750, chi=1.027, supp=0.500)
- `teaching_horses_to_sing` (eps=0.750, chi=1.027, supp=0.400)
- `temporal_scale_arbitrage` (eps=0.760, chi=1.041, supp=0.400)
- `tragedy_of_the_commons` (eps=0.700, chi=0.959, supp=0.400)
- `transformer_self_attention` (eps=0.750, chi=1.027, supp=0.400)

## Blocking Gate Frequency

How many of the unknowns fail each specific gate condition:

| Gate | Count | Description |
|------|-------|-------------|
| not_mountain_supp | 38 | Supp > 0.05 |
| not_mountain_eps | 39 | BaseEps > 0.25 |
| snare_chi_below_066 | 13 | Chi < 0.66 |
| snare_eps_below_046 | 10 | BaseEps < 0.46 |
| snare_supp_below_060 | 37 | Supp < 0.60 |
| scaffold_chi_above_030 | 39 | Chi > 0.30 |
| scaffold_no_coordination | 2 | No has_coordination_function |
| scaffold_no_temporality | 26 | No sunset clause and requires enforcement |
| scaffold_theater_above_070 | 6 | Theater > 0.70 |
| rope_chi_above_035 | 39 | Chi > 0.35 |
| rope_eps_above_045 | 29 | BaseEps > 0.45 (when Chi > 0) |
| tangled_chi_above_090 | 22 | Chi > 0.90 |
| tangled_supp_below_040 | 17 | Supp < 0.40 |
| tangled_no_enforcement | 10 | No requires_active_enforcement |
| tangled_no_coordination | 2 | No has_coordination_function |
| tangled_no_asymmetry | 2 | No has_asymmetric_extraction |
| piton_chi_above_025 | 39 | Chi > 0.25 |
| piton_theater_below_070 | 33 | Theater < 0.70 |
| opaque_eps_at_or_below_045 | 10 | BaseEps <= 0.45 |
| opaque_chi_at_or_above_040 | 39 | Chi >= 0.40 |

## Primary Blocking Gate (What Each Unknown "Wants to Be")

The primary gate is the first type in the classifier chain where the constraint
almost qualifies but fails one condition.

| Primary Gate | Count | Interpretation |
|--------------|-------|----------------|
| near_tangled_no_enforcement | 2 | Would be tangled_rope but no requires_active_enforcement |
| near_tangled_no_coordination | 1 | Would be tangled_rope but no has_coordination_function |
| near_tangled_supp_too_low | 10 | Would be tangled_rope but Supp < 0.40 |
| near_snare_supp_too_low | 26 | Would be snare but Supp < 0.60 |

### Detailed Breakdown

**near_tangled_no_enforcement** (Would be tangled_rope but no requires_active_enforcement): 2 constraints
- `hawthorne_effect` (eps=0.400, chi=0.548, supp=0.500, theater=0.190)
- `qwerty_vs_dvorak` (eps=0.400, chi=0.548, supp=0.700, theater=0.120)

**near_tangled_no_coordination** (Would be tangled_rope but no has_coordination_function): 1 constraints
- `gs1_standardized_identification` (eps=0.400, chi=0.552, supp=0.800, theater=0.110)

**near_tangled_supp_too_low** (Would be tangled_rope but Supp < 0.40): 10 constraints
- `france_2027_presidential_election` (eps=0.480, chi=0.658, supp=0.380, theater=0.400)
- `glp1_payload_efficiency_pivot` (eps=0.350, chi=0.479, supp=0.200, theater=0.120)
- `gradient_descent_optimization` (eps=0.300, chi=0.411, supp=0.200, theater=0.100)
- `maha_recovery_2026` (eps=0.420, chi=0.575, supp=0.350, theater=0.650)
- `pareto_principle` (eps=0.400, chi=0.548, supp=0.300, theater=0.060)
- `paris_municipal_reform_2026` (eps=0.470, chi=0.644, supp=0.350, theater=0.250)
- `platonic_coparenting_decoupling` (eps=0.480, chi=0.658, supp=0.350, theater=0.150)
- `shitty_feedback_handling` (eps=0.400, chi=0.548, supp=0.300, theater=0.150)
- `skills_based_hiring` (eps=0.300, chi=0.411, supp=0.200, theater=0.110)
- `unclos_2026` (eps=0.300, chi=0.411, supp=0.200, theater=0.140)

**near_snare_supp_too_low** (Would be snare but Supp < 0.60): 26 constraints
- `gamblers_ruin_stochastic_extinction` (eps=0.900, chi=1.233, supp=0.500, theater=0.050)
- `grete_samsa_transition` (eps=0.750, chi=1.027, supp=0.500, theater=0.100)
- `hegemonic_entropy_2026` (eps=0.720, chi=0.986, supp=0.550, theater=0.780)
- `hoa_covenants` (eps=0.550, chi=0.753, supp=0.300, theater=0.750)
- `hydra_game` (eps=0.700, chi=0.959, supp=0.500, theater=0.000)
- `institutional_trust_decay` (eps=0.680, chi=0.932, supp=0.550, theater=0.880)
- `insult_wisdom_training` (eps=0.750, chi=1.027, supp=0.500, theater=0.050)
- `moltbook_agent_theater` (eps=0.700, chi=0.959, supp=0.450, theater=0.900)
- `negative_emissions_arbitrage` (eps=0.820, chi=1.123, supp=0.550, theater=0.150)
- `net_zero_stabilization` (eps=0.850, chi=1.164, supp=0.450, theater=0.100)
- `neural_interoperability` (eps=0.850, chi=1.164, supp=0.500, theater=0.150)
- `neurodiversity_spectrum` (eps=0.720, chi=0.986, supp=0.550, theater=0.200)
- `quantum_decryption_risk_2026` (eps=0.800, chi=1.096, supp=0.300, theater=0.100)
- `recipe_scaling_ai` (eps=0.490, chi=0.671, supp=0.250, theater=0.100)
- `regulatory_capture` (eps=0.800, chi=1.096, supp=0.200, theater=0.750)
- `repair_probe_incomplete` (eps=0.550, chi=0.759, supp=0.000, theater=0.150)
- `rotation_seven_black_soil` (eps=0.950, chi=1.301, supp=0.100, theater=0.100)
- `royal_navy_middle_east_withdrawal` (eps=0.800, chi=1.096, supp=0.500, theater=0.200)
- `rule_update_failure` (eps=0.720, chi=0.986, supp=0.580, theater=0.750)
- `russells_paradox_self_reference` (eps=0.700, chi=0.959, supp=0.400, theater=0.000)
- `smartphone_ubiquity` (eps=0.750, chi=1.027, supp=0.500, theater=0.100)
- `teaching_horses_to_sing` (eps=0.750, chi=1.027, supp=0.400, theater=0.100)
- `temporal_scale_arbitrage` (eps=0.760, chi=1.041, supp=0.400, theater=0.100)
- `tragedy_of_the_commons` (eps=0.700, chi=0.959, supp=0.400, theater=0.100)
- `transformer_self_attention` (eps=0.750, chi=1.027, supp=0.400, theater=0.050)
- `trillion_bond_rush_2026` (eps=0.520, chi=0.712, supp=0.100, theater=0.100)

## Cross-Context Transition Map

How each unknown is classified from different observer perspectives:

| Constraint | Institutional/Local | Moderate/National | Analytical/Global |
|------------|---------------------|-------------------|-------------------|
| france_2027_presidential_election | rope | unknown | unknown |
| gamblers_ruin_stochastic_extinction | scaffold | unknown | unknown |
| glp1_payload_efficiency_pivot | scaffold | unknown | unknown |
| gradient_descent_optimization | scaffold | rope | unknown |
| grete_samsa_transition | rope | tangled_rope | unknown |
| gs1_standardized_identification | rope | unknown | unknown |
| hawthorne_effect | scaffold | unknown | unknown |
| hegemonic_entropy_2026 | rope | unknown | unknown |
| hoa_covenants | rope | unknown | unknown |
| hydra_game | rope | tangled_rope | unknown |
| institutional_trust_decay | rope | tangled_rope | unknown |
| insult_wisdom_training | rope | tangled_rope | unknown |
| maha_recovery_2026 | scaffold | unknown | unknown |
| moltbook_agent_theater | rope | tangled_rope | unknown |
| negative_emissions_arbitrage | rope | unknown | unknown |
| net_zero_stabilization | rope | unknown | unknown |
| neural_interoperability | rope | unknown | unknown |
| neurodiversity_spectrum | rope | tangled_rope | unknown |
| pareto_principle | rope | unknown | unknown |
| paris_municipal_reform_2026 | rope | unknown | unknown |
| platonic_coparenting_decoupling | scaffold | unknown | unknown |
| quantum_decryption_risk_2026 | rope | unknown | unknown |
| qwerty_vs_dvorak | scaffold | unknown | unknown |
| recipe_scaling_ai | scaffold | unknown | unknown |
| regulatory_capture | rope | unknown | unknown |
| repair_probe_incomplete | rope | unknown | unknown |
| rotation_seven_black_soil | rope | unknown | unknown |
| royal_navy_middle_east_withdrawal | rope | tangled_rope | unknown |
| rule_update_failure | rope | tangled_rope | unknown |
| russells_paradox_self_reference | scaffold | unknown | unknown |
| shitty_feedback_handling | rope | unknown | unknown |
| skills_based_hiring | scaffold | rope | unknown |
| smartphone_ubiquity | rope | tangled_rope | unknown |
| teaching_horses_to_sing | rope | tangled_rope | unknown |
| temporal_scale_arbitrage | rope | tangled_rope | unknown |
| tragedy_of_the_commons | rope | tangled_rope | unknown |
| transformer_self_attention | rope | tangled_rope | unknown |
| trillion_bond_rush_2026 | scaffold | unknown | unknown |
| unclos_2026 | rope | rope | unknown |

### Transition Pattern Summary

| Institutional -> Moderate -> Analytical | Count |
|----------------------------------------|-------|
| rope -> rope -> unknown | 1 |
| rope -> tangled_rope -> unknown | 13 |
| rope -> unknown -> unknown | 14 |
| scaffold -> rope -> unknown | 2 |
| scaffold -> unknown -> unknown | 9 |

## Structural Flags Summary

| Flag | Present | Absent | Present % |
|------|---------|--------|-----------|
| has_coordination_function | 37 | 2 | 94.9% |
| has_asymmetric_extraction | 37 | 2 | 94.9% |
| requires_active_enforcement | 29 | 10 | 74.4% |
| emerges_naturally | 5 | 34 | 12.8% |
| natural_law_without_beneficiary | 0 | 39 | 0.0% |

## Indexically Opaque Impossibility at Analytical Context

**Claim**: The `indexically_opaque` clause (drl_core.pl:314-318) can NEVER fire at the analytical/global context.

**Proof**:
- The clause requires: `BaseEps > 0.45 AND Chi < 0.40`
- At analytical/global: `Chi = BaseEps * f(0.725) * 1.20 = BaseEps * 1.380`
- For `Chi < 0.40`: `BaseEps < 0.40 / 1.380 = 0.290`
- But `BaseEps > 0.45` is simultaneously required
- `0.290 < 0.45` -- **contradiction**

**Empirical verification**:
- Constraints satisfying BOTH BaseEps > 0.45 AND Chi < 0.40: **0** (expected: 0)
- **Confirmed**: The opaque clause is structurally impossible at analytical context.

## Structural Signature Analysis

The `dr_type/3` pipeline passes metric-based type through `integrate_signature_with_modal/3`.
When the metric layer returns `unknown`, certain signatures can rescue the classification
(e.g., `coordination_scaffold` -> `rope`, `constructed_high_extraction` -> `snare`).
The 39 constraints that remain `unknown` after this stage have signatures that do NOT rescue.

| Signature | Count |
|-----------|-------|
| false_ci_rope | 39 |

For `unknown` metric type, the rescue rules are:
- `coordination_scaffold` -> rope
- `constructed_low_extraction` -> rope
- `constructed_high_extraction` -> snare
- `constructed_constraint` -> tangled_rope
- `piton_signature` -> piton
- `ambiguous` -> unknown (no rescue)
- Any other signature with no `unknown` rescue rule -> keeps unknown

## Finding Classification

### Distribution

- Region A (chi gap): 0 (0.0%)
- Region B (low epsilon): 0 (0.0%)
- Region C (low suppression): 17 (43.6%)
- Region D (missing flags): 6 (15.4%)
- Region E (near-snare): 0 (0.0%)
- Region F (other): 16 (41.0%)

### Verdict

**Category C -- Classifier Edge Cases**: The unknowns are scattered across boundary conditions with no clear pattern. Consider whether expanding existing type boundaries or adding new types is warranted.

### Recommendations

2. **Missing flags (Region D)**: Review these testsets and add missing structural declarations (has_coordination_function, requires_active_enforcement, has_asymmetric_extraction) where warranted by the domain.

3. **Low suppression (Region C)**: These constraints have tangled_rope chi and epsilon but suppression < 0.40. Consider whether they represent a genuine category (extraction without enforcement) or data errors.

## Embedded Prolog Facts

```prolog
%% omega1_unknown(Constraint, AnalyticalContext, PrimaryBlockingGate).
omega1_unknown(france_2027_presidential_election, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(gamblers_ruin_stochastic_extinction, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(glp1_payload_efficiency_pivot, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(gradient_descent_optimization, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(grete_samsa_transition, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(gs1_standardized_identification, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_no_coordination).
omega1_unknown(hawthorne_effect, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_no_enforcement).
omega1_unknown(hegemonic_entropy_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(hoa_covenants, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(hydra_game, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(institutional_trust_decay, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(insult_wisdom_training, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(maha_recovery_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(moltbook_agent_theater, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(negative_emissions_arbitrage, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(net_zero_stabilization, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(neural_interoperability, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(neurodiversity_spectrum, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(pareto_principle, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(paris_municipal_reform_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(platonic_coparenting_decoupling, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(quantum_decryption_risk_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(qwerty_vs_dvorak, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_no_enforcement).
omega1_unknown(recipe_scaling_ai, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(regulatory_capture, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(repair_probe_incomplete, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(rotation_seven_black_soil, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(royal_navy_middle_east_withdrawal, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(rule_update_failure, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(russells_paradox_self_reference, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(shitty_feedback_handling, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(skills_based_hiring, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).
omega1_unknown(smartphone_ubiquity, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(teaching_horses_to_sing, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(temporal_scale_arbitrage, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(tragedy_of_the_commons, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(transformer_self_attention, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(trillion_bond_rush_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_snare_supp_too_low).
omega1_unknown(unclos_2026, context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)), near_tangled_supp_too_low).

%% omega1_transition(Constraint, InstitutionalType, ModerateType, AnalyticalType).
omega1_transition(france_2027_presidential_election, rope, unknown, unknown).
omega1_transition(gamblers_ruin_stochastic_extinction, scaffold, unknown, unknown).
omega1_transition(glp1_payload_efficiency_pivot, scaffold, unknown, unknown).
omega1_transition(gradient_descent_optimization, scaffold, rope, unknown).
omega1_transition(grete_samsa_transition, rope, tangled_rope, unknown).
omega1_transition(gs1_standardized_identification, rope, unknown, unknown).
omega1_transition(hawthorne_effect, scaffold, unknown, unknown).
omega1_transition(hegemonic_entropy_2026, rope, unknown, unknown).
omega1_transition(hoa_covenants, rope, unknown, unknown).
omega1_transition(hydra_game, rope, tangled_rope, unknown).
omega1_transition(institutional_trust_decay, rope, tangled_rope, unknown).
omega1_transition(insult_wisdom_training, rope, tangled_rope, unknown).
omega1_transition(maha_recovery_2026, scaffold, unknown, unknown).
omega1_transition(moltbook_agent_theater, rope, tangled_rope, unknown).
omega1_transition(negative_emissions_arbitrage, rope, unknown, unknown).
omega1_transition(net_zero_stabilization, rope, unknown, unknown).
omega1_transition(neural_interoperability, rope, unknown, unknown).
omega1_transition(neurodiversity_spectrum, rope, tangled_rope, unknown).
omega1_transition(pareto_principle, rope, unknown, unknown).
omega1_transition(paris_municipal_reform_2026, rope, unknown, unknown).
omega1_transition(platonic_coparenting_decoupling, scaffold, unknown, unknown).
omega1_transition(quantum_decryption_risk_2026, rope, unknown, unknown).
omega1_transition(qwerty_vs_dvorak, scaffold, unknown, unknown).
omega1_transition(recipe_scaling_ai, scaffold, unknown, unknown).
omega1_transition(regulatory_capture, rope, unknown, unknown).
omega1_transition(repair_probe_incomplete, rope, unknown, unknown).
omega1_transition(rotation_seven_black_soil, rope, unknown, unknown).
omega1_transition(royal_navy_middle_east_withdrawal, rope, tangled_rope, unknown).
omega1_transition(rule_update_failure, rope, tangled_rope, unknown).
omega1_transition(russells_paradox_self_reference, scaffold, unknown, unknown).
omega1_transition(shitty_feedback_handling, rope, unknown, unknown).
omega1_transition(skills_based_hiring, scaffold, rope, unknown).
omega1_transition(smartphone_ubiquity, rope, tangled_rope, unknown).
omega1_transition(teaching_horses_to_sing, rope, tangled_rope, unknown).
omega1_transition(temporal_scale_arbitrage, rope, tangled_rope, unknown).
omega1_transition(tragedy_of_the_commons, rope, tangled_rope, unknown).
omega1_transition(transformer_self_attention, rope, tangled_rope, unknown).
omega1_transition(trillion_bond_rush_2026, scaffold, unknown, unknown).
omega1_transition(unclos_2026, rope, rope, unknown).
```

---

*End of Omega1 audit*
