
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/boom_bust_path_dependency.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: boom_bust_path_dependency...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.08 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.08 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.08 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.08 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.08 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.08 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: boom_bust_path_dependency...

>>> INITIATING DR-AUDIT SUITE: boom_bust_path_dependency

[REPAIR] Auditing vectors for: boom_bust_path_dependency...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: boom_bust_path_dependency (0-10)
Checking Interval: boom_bust_path_dependency (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: boom_bust_path_dependency

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): piton

From ANALYTICAL perspective: piton

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] boom_bust_path_dependency from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=rope
  [INDEX MISMATCH] boom_bust_path_dependency from context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=piton, computed=rope
  [INDEX MISMATCH] boom_bust_path_dependency from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=piton, computed=rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: boom_bust_path_dependency ===

[CONSTRAINT INVENTORY]
  - boom_bust_path_dependency: inertial_piton (Intensity: 0.78)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  5
  Critical: 1 | Warning: 2 | Watch: 2

  boom_bust_path_dependency:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.4,0.78)
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.15,0.22)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,0.625,threshold,0.25,extraction_trend,increasing)
    [watch] boltzmann_floor_drift
        Evidence: evidence(current_eps,0.22,floor,0.15,excess,0.07,trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.7428333333333333,decline_signals,[extraction_rising,coupling_above_threshold(0.625),theater_rising,excess_above_floor(0.07)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  boom_bust_path_dependency -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  boom_bust_path_dependency (ε=0.22):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.22 × 1.36 × 0.80 = 0.239
    moderate@national: d=0.700 f(d)=1.11 χ = 0.22 × 1.11 × 1.00 = 0.243
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.22 × -0.04 × 1.00 = -0.009
    analytical@global: d=0.720 f(d)=1.14 χ = 0.22 × 1.14 × 1.20 = 0.301

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: boom_bust_path_dependency ===
  Shift (computed via dr_type/3):
    powerless=piton  moderate=rope  institutional=rope  analytical=rope
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=negligible  suppression=low
  Coupling:   strongly_coupled (score=0.625, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0)], boltzmann=non_compliant(0.625,0.3))
  Purity:     0.743 (sound)



--- ORBIT CONTEXT ---

  Orbit Signature:    [piton, rope]
  Orbit Span:         2
  Number of Contexts: 4
  Gauge Status:       Gauge-Variant

  Per-context types:
    powerless       -> piton
    moderate        -> rope
    institutional   -> rope
    analytical      -> rope

  Orbit Family ID:    No family assignment (no omega generated)

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       4
  Types Produced:      2

  Structural Twins: Not in any structural twin group

  Covering Analysis:
    Missed Transitions: 2
    Unique Type Shifts:  piton -> unknown

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: boom_bust_path_dependency
  Claimed Type: piton
  Perspectives:
    - [context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional))]: rope (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(national))]: piton (Matches Claim)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: piton (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: boom_bust_path_dependency]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for boom_bust_path_dependency

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  boom_bust_path_dependency: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for boom_bust_path_dependency: Appears to be rope (indexed_rope_classification) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(0.625,0.3),excess_above_floor(0.07),nonsensical_coupling(0.16666666666666666)]. Coupling score=0.625. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: low_tax_constituencies
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.43

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: boom_bust_path_dependency
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.24]
    - Institutional (Manager): piton [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: boom_bust_path_dependency]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_royalty_restitution (conceptual)
      Reversibility of atrophied fiscal structures

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_royalty_restitution] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Reversibility of atrophied fiscal structures
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive unknown
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
