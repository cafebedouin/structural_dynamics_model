
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/china_africa_zero_tariff_2026.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: china_africa_zero_tariff_2026...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.45 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.45 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.45 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.45 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.45 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.45 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: china_africa_zero_tariff_2026...

>>> INITIATING DR-AUDIT SUITE: china_africa_zero_tariff_2026

[REPAIR] Auditing vectors for: china_africa_zero_tariff_2026...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: china_africa_zero_tariff_2026 (0-10)
Checking Interval: china_africa_zero_tariff_2026 (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: china_africa_zero_tariff_2026

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] china_africa_zero_tariff_2026 from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(continental)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] china_africa_zero_tariff_2026 from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=rope
  [INDEX OK] china_africa_zero_tariff_2026 from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: china_africa_zero_tariff_2026 ===

[CONSTRAINT INVENTORY]
  - china_africa_zero_tariff_2026: hybrid_extraction (Intensity: 0.32)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 0 | Warning: 3 | Watch: 0

  china_africa_zero_tariff_2026:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.25,0.32)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3403333333333333,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.17)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  china_africa_zero_tariff_2026 -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  china_africa_zero_tariff_2026 (ε=0.32):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.32 × 1.36 × 0.80 = 0.348
    moderate@national: d=0.700 f(d)=1.11 χ = 0.32 × 1.11 × 1.00 = 0.354
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.32 × -0.04 × 1.00 = -0.014
    analytical@global: d=0.720 f(d)=1.14 χ = 0.32 × 1.14 × 1.20 = 0.438

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: china_africa_zero_tariff_2026 ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=unknown  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.340 (contaminated)



--- ORBIT CONTEXT ---

  Not found in orbit analysis — constraint may not have been
  included in batch run or may lack sufficient index configurations.

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       4
  Types Produced:      2

  Structural Twin Group:
    Signature:   (0.3, 0.5, False, True)
    Group Size:  16
    Types:       tangled_rope, scaffold

  Covering Analysis: No missed transitions on expanded grid

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: china_africa_zero_tariff_2026
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(continental))]: tangled_rope (Matches Claim)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: china_africa_zero_tariff_2026]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for china_africa_zero_tariff_2026

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  china_africa_zero_tariff_2026: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for china_africa_zero_tariff_2026: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([rope,tangled_rope,unknown]),excess_above_floor(0.17),nonsensical_coupling(0.6666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: china_state_actors
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.49

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: china_africa_zero_tariff_2026
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.35]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: china_africa_zero_tariff_2026]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [moderate] 1 omega(s):
    - omega_china_africa_2026 (empirical)
      Non-tariff barrier impact of green channel standards

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_china_africa_2026] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Non-tariff barrier impact of green channel standards
  │
  │  RESOLUTION STRATEGY:
  │  1. Design measurement protocol for unknown
  │  2. Collect data from N=30+ real-world instances
  │  3. Calculate empirical metrics:
  │     - suppression_requirement (enforcement needed)
  │     - resistance_to_change (pushback level)
  │     - base_extractiveness (asymmetric benefit flow)
  │  4. Update constraint_metric/3 declarations with data
  │  5. Re-run classification to resolve perspectival gap
  └─

====================================================
