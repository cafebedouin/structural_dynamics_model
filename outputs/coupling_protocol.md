
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

# Inferred Coupling Activation Protocol

*Tests whether the dormant `infer_structural_coupling/3` mechanism*
*can be activated by providing `measurement/5` ground facts, and*
*measures the resulting network impact.*

---

## Protocol Documentation

### measurement/5 Signature

```prolog
measurement(?Source, ?Constraint, extractiveness, ?Time, ?Value)
```

- **Source**: Atom identifying the measurement source
- **Constraint**: The constraint being measured
- **Metric**: Must be the atom `extractiveness` (hardcoded in `dr_gradient_at/3`)
- **Time**: Numeric timepoint
- **Value**: Float in [0, 1]

### Algorithm

1. `dr_gradient_at(C, T, Grad)` extracts gradient = X(T2) - X(T) for consecutive timepoints
2. `infer_structural_coupling(C1, C2, Strength)` computes sign-agreement ratio
3. If Strength >= `network_coupling_threshold` (0.50), an edge is created
4. `constraint_neighbors/3` includes inferred edges in neighbor discovery

### Requirements

- 3+ timepoints per constraint (produces 2+ gradients, satisfying L > 1)
- Both constraints in a pair must have the same number of gradients
- Sign-agreement: both positive, both negative, or both zero

## Phase 2: Gradient Verification

Verifying that `dr_gradient_at/3` produces gradients from the `measurement/5` facts:

| Constraint | Gradients | Count | Status |
|------------|-----------|-------|--------|
| quantum_decryption_risk_2026 | T0:0.100, T5:0.100 | 2 | PASS |
| smartphone_ubiquity | T0:0.100, T5:0.100 | 2 | PASS |
| regulatory_capture | T0:0.100, T5:0.070 | 2 | PASS |
| institutional_trust_decay | T0:0.120, T5:0.080 | 2 | PASS |
| tragedy_of_the_commons | T0:0.100, T5:0.070 | 2 | PASS |
| pareto_principle | T0:0.070, T5:0.050 | 2 | PASS |
| hawthorne_effect | T0:0.080, T5:0.040 | 2 | PASS |
| rotation_seven_black_soil | T0:-0.070, T5:-0.080 | 2 | PASS |

**Result**: 8/8 constraints produce 2+ gradients.

## Phase 3: Edge Creation Verification

Testing each designed constraint pair:

| Pair | C1 | C2 | Expected | Actual | Edge? | Verdict |
|------|----|----|----------|--------|-------|---------|
| pair_1_tech_ecosystem | quantum_decryption_risk_2026 | smartphone_ubiquity | 1.00 | 1.00 | yes | PASS |
| pair_2_institutional_erosion | regulatory_capture | institutional_trust_decay | 1.00 | 1.00 | yes | PASS |
| pair_3_commons_degradation | tragedy_of_the_commons | pareto_principle | 1.00 | 1.00 | yes | PASS |
| pair_4_negative_control | hawthorne_effect | rotation_seven_black_soil | 0.00 | 0.00 | no | PASS |

**Result**: 4/4 pairs behave as expected.

## All Inferred Coupling Edges

Total inferred edges above threshold: **55**

| C1 | C2 | Strength |
|----|----|-----------| 
| hawthorne_effect | institutional_trust_decay | 1.000 |
| hawthorne_effect | iran_war_room_2026 | 1.000 |
| hawthorne_effect | pareto_principle | 1.000 |
| hawthorne_effect | quantum_decryption_risk_2026 | 1.000 |
| hawthorne_effect | regulatory_capture | 1.000 |
| hawthorne_effect | silklink_2026 | 1.000 |
| hawthorne_effect | smartphone_ubiquity | 1.000 |
| hawthorne_effect | tragedy_of_the_commons | 1.000 |
| hawthorne_effect | trillion_bond_rush_2026 | 1.000 |
| hawthorne_effect | world_factbook_sunset_2026 | 1.000 |
| institutional_trust_decay | iran_war_room_2026 | 1.000 |
| institutional_trust_decay | pareto_principle | 1.000 |
| institutional_trust_decay | quantum_decryption_risk_2026 | 1.000 |
| institutional_trust_decay | regulatory_capture | 1.000 |
| institutional_trust_decay | silklink_2026 | 1.000 |
| institutional_trust_decay | smartphone_ubiquity | 1.000 |
| institutional_trust_decay | tragedy_of_the_commons | 1.000 |
| institutional_trust_decay | trillion_bond_rush_2026 | 1.000 |
| institutional_trust_decay | world_factbook_sunset_2026 | 1.000 |
| iran_war_room_2026 | pareto_principle | 1.000 |
| iran_war_room_2026 | quantum_decryption_risk_2026 | 1.000 |
| iran_war_room_2026 | regulatory_capture | 1.000 |
| iran_war_room_2026 | silklink_2026 | 1.000 |
| iran_war_room_2026 | smartphone_ubiquity | 1.000 |
| iran_war_room_2026 | tragedy_of_the_commons | 1.000 |
| iran_war_room_2026 | trillion_bond_rush_2026 | 1.000 |
| iran_war_room_2026 | world_factbook_sunset_2026 | 1.000 |
| pareto_principle | quantum_decryption_risk_2026 | 1.000 |
| pareto_principle | regulatory_capture | 1.000 |
| pareto_principle | silklink_2026 | 1.000 |
| pareto_principle | smartphone_ubiquity | 1.000 |
| pareto_principle | tragedy_of_the_commons | 1.000 |
| pareto_principle | trillion_bond_rush_2026 | 1.000 |
| pareto_principle | world_factbook_sunset_2026 | 1.000 |
| quantum_decryption_risk_2026 | regulatory_capture | 1.000 |
| quantum_decryption_risk_2026 | silklink_2026 | 1.000 |
| quantum_decryption_risk_2026 | smartphone_ubiquity | 1.000 |
| quantum_decryption_risk_2026 | tragedy_of_the_commons | 1.000 |
| quantum_decryption_risk_2026 | trillion_bond_rush_2026 | 1.000 |
| quantum_decryption_risk_2026 | world_factbook_sunset_2026 | 1.000 |
| regulatory_capture | silklink_2026 | 1.000 |
| regulatory_capture | smartphone_ubiquity | 1.000 |
| regulatory_capture | tragedy_of_the_commons | 1.000 |
| regulatory_capture | trillion_bond_rush_2026 | 1.000 |
| regulatory_capture | world_factbook_sunset_2026 | 1.000 |
| silklink_2026 | smartphone_ubiquity | 1.000 |
| silklink_2026 | tragedy_of_the_commons | 1.000 |
| silklink_2026 | trillion_bond_rush_2026 | 1.000 |
| silklink_2026 | world_factbook_sunset_2026 | 1.000 |
| smartphone_ubiquity | tragedy_of_the_commons | 1.000 |
| smartphone_ubiquity | trillion_bond_rush_2026 | 1.000 |
| smartphone_ubiquity | world_factbook_sunset_2026 | 1.000 |
| tragedy_of_the_commons | trillion_bond_rush_2026 | 1.000 |
| tragedy_of_the_commons | world_factbook_sunset_2026 | 1.000 |
| trillion_bond_rush_2026 | world_factbook_sunset_2026 | 1.000 |

## Phase 4: Network Impact

| Metric | Baseline | With Inferred | Delta |
|--------|----------|---------------|-------|
| Total edges | 303 | 358 | +55 |
| Connected components | 398 | 388 | -10 |
| Largest component | 14 | 26 | +26-14 |
| Largest as % of corpus | 3.0% | 5.5% | +2.6% |

**No giant component**: Largest component is 26 nodes (5.5% of corpus). Network remains fragmented even with inferred coupling.

## Phase 5: Cross-Domain Bridge Analysis

### Bridge Edges

Inferred edges that connect previously separate components:

| C1 | Type1 | C2 | Type2 | Strength | Bridge? |
|----|-------|----|-------|----------|---------|
| hawthorne_effect | unknown | institutional_trust_decay | unknown | 1.000 | yes |
| hawthorne_effect | unknown | iran_war_room_2026 | snare | 1.000 | yes |
| hawthorne_effect | unknown | pareto_principle | unknown | 1.000 | yes |
| hawthorne_effect | unknown | quantum_decryption_risk_2026 | unknown | 1.000 | yes |
| hawthorne_effect | unknown | regulatory_capture | unknown | 1.000 | yes |
| hawthorne_effect | unknown | silklink_2026 | rope | 1.000 | yes |
| hawthorne_effect | unknown | smartphone_ubiquity | unknown | 1.000 | yes |
| hawthorne_effect | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| hawthorne_effect | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| hawthorne_effect | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| institutional_trust_decay | unknown | iran_war_room_2026 | snare | 1.000 | yes |
| institutional_trust_decay | unknown | pareto_principle | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | quantum_decryption_risk_2026 | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | regulatory_capture | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | silklink_2026 | rope | 1.000 | yes |
| institutional_trust_decay | unknown | smartphone_ubiquity | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| institutional_trust_decay | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| iran_war_room_2026 | snare | pareto_principle | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | quantum_decryption_risk_2026 | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | regulatory_capture | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | silklink_2026 | rope | 1.000 | yes |
| iran_war_room_2026 | snare | smartphone_ubiquity | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | tragedy_of_the_commons | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| iran_war_room_2026 | snare | world_factbook_sunset_2026 | snare | 1.000 | yes |
| pareto_principle | unknown | quantum_decryption_risk_2026 | unknown | 1.000 | yes |
| pareto_principle | unknown | regulatory_capture | unknown | 1.000 | yes |
| pareto_principle | unknown | silklink_2026 | rope | 1.000 | yes |
| pareto_principle | unknown | smartphone_ubiquity | unknown | 1.000 | yes |
| pareto_principle | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| pareto_principle | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| pareto_principle | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | regulatory_capture | unknown | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | silklink_2026 | rope | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | smartphone_ubiquity | unknown | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| quantum_decryption_risk_2026 | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| regulatory_capture | unknown | silklink_2026 | rope | 1.000 | yes |
| regulatory_capture | unknown | smartphone_ubiquity | unknown | 1.000 | yes |
| regulatory_capture | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| regulatory_capture | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| regulatory_capture | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| silklink_2026 | rope | smartphone_ubiquity | unknown | 1.000 | yes |
| silklink_2026 | rope | tragedy_of_the_commons | unknown | 1.000 | yes |
| silklink_2026 | rope | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| silklink_2026 | rope | world_factbook_sunset_2026 | snare | 1.000 | yes |
| smartphone_ubiquity | unknown | tragedy_of_the_commons | unknown | 1.000 | yes |
| smartphone_ubiquity | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| smartphone_ubiquity | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| tragedy_of_the_commons | unknown | trillion_bond_rush_2026 | unknown | 1.000 | yes |
| tragedy_of_the_commons | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |
| trillion_bond_rush_2026 | unknown | world_factbook_sunset_2026 | snare | 1.000 | yes |

**55/55 inferred edges are bridges** (connecting previously separate components).

### Component Merges

- Baseline components: 398
- After inferred coupling: 388
- Components merged: 10

### Merged Component Members

Bridge `hawthorne_effect` -- `institutional_trust_decay` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `iran_war_room_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `pareto_principle` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `quantum_decryption_risk_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `regulatory_capture` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `hawthorne_effect` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `iran_war_room_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `pareto_principle` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `quantum_decryption_risk_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `regulatory_capture` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `institutional_trust_decay` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `pareto_principle` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `quantum_decryption_risk_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `regulatory_capture` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `iran_war_room_2026` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `quantum_decryption_risk_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `regulatory_capture` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `pareto_principle` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `regulatory_capture` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `quantum_decryption_risk_2026` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `regulatory_capture` -- `silklink_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `regulatory_capture` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `regulatory_capture` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `regulatory_capture` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `regulatory_capture` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `smartphone_ubiquity` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `silklink_2026` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `smartphone_ubiquity` -- `tragedy_of_the_commons` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `smartphone_ubiquity` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `smartphone_ubiquity` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `tragedy_of_the_commons` -- `trillion_bond_rush_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `tragedy_of_the_commons` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

Bridge `trillion_bond_rush_2026` -- `world_factbook_sunset_2026` is now in a component of size 26:
  - bip_narrative_illusion
  - cbdc_implementation
  - civilizational_maintenance_debt
  - deferred_risk_realization
  - fossil_fuel_dependency
  - hawthorne_effect
  - institutional_inertia
  - institutional_trust_decay
  - interface_contract_breakdown
  - iran_war_room_2026
  - knowledge_action_gap
  - media_ownership_rules
  - network_effects
  - pareto_principle
  - pharmaceutical_patents
  - platform_lock_in
  - policy_lag_catastrophe
  - post_quantum_cryptography_adoption
  - quantum_decryption_risk_2026
  - regulatory_capture
  - silklink_2026
  - slow_crisis_invisibility
  - smartphone_ubiquity
  - tragedy_of_the_commons
  - trillion_bond_rush_2026
  - world_factbook_sunset_2026

### Type Diversity in Bridged Components

If inferred coupling edges carry contamination, what types are now reachable from each bridged constraint?

Component containing `hawthorne_effect` and `institutional_trust_decay`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `iran_war_room_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `pareto_principle`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `quantum_decryption_risk_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `regulatory_capture`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `hawthorne_effect` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `iran_war_room_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `pareto_principle`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `quantum_decryption_risk_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `regulatory_capture`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `institutional_trust_decay` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `pareto_principle`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `quantum_decryption_risk_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `regulatory_capture`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `iran_war_room_2026` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `quantum_decryption_risk_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `regulatory_capture`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `pareto_principle` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `regulatory_capture`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `quantum_decryption_risk_2026` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `regulatory_capture` and `silklink_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `regulatory_capture` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `regulatory_capture` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `regulatory_capture` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `regulatory_capture` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `silklink_2026` and `smartphone_ubiquity`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `silklink_2026` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `silklink_2026` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `silklink_2026` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `smartphone_ubiquity` and `tragedy_of_the_commons`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `smartphone_ubiquity` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `smartphone_ubiquity` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `tragedy_of_the_commons` and `trillion_bond_rush_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `tragedy_of_the_commons` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

Component containing `trillion_bond_rush_2026` and `world_factbook_sunset_2026`:
  - rope: 1
  - snare: 5
  - tangled_rope: 2
  - unknown: 18

---

*End of Inferred Coupling Activation Protocol*
