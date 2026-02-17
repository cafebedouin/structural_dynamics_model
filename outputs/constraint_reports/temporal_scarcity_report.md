
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/temporal_scarcity.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: temporal_scarcity...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.35 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.35 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.35 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.35 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.35 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.35 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: temporal_scarcity...

>>> INITIATING DR-AUDIT SUITE: temporal_scarcity

[REPAIR] Auditing vectors for: temporal_scarcity...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: temporal_scarcity (0-10)
Checking Interval: temporal_scarcity (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: temporal_scarcity

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): piton

From ANALYTICAL perspective: piton

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] temporal_scarcity from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=piton, computed=piton
  [INDEX MISMATCH] temporal_scarcity from context(agent_power(institutional),time_horizon(immediate),exit_options(constrained),spatial_scope(national)): declared=rope, computed=piton
  [INDEX MISMATCH] temporal_scarcity from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=piton, computed=rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: temporal_scarcity ===

[CONSTRAINT INVENTORY]
  - temporal_scarcity: inertial_piton (Intensity: 0.72)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 1 | Watch: 0

  temporal_scarcity:
    [warning] purity_drift
        Evidence: evidence(current_purity,0.8475,decline_signals,[coupling_above_threshold(0.375),excess_above_floor(0.09999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  temporal_scarcity (ε=0.15):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.15 × 1.36 × 0.80 = 0.163
    moderate@national: d=0.700 f(d)=1.11 χ = 0.15 × 1.11 × 1.00 = 0.166
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.15 × -0.04 × 1.00 = -0.006
    analytical@global: d=0.720 f(d)=1.14 χ = 0.15 × 1.14 × 1.20 = 0.205

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: temporal_scarcity ===
  Shift (computed via dr_type/3):
    powerless=piton  moderate=rope  institutional=rope  analytical=rope
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=moderate
  Coupling:   weakly_coupled (score=0.375, pairs=[], boltzmann=non_compliant(0.375,0.25))
  Purity:     0.848 (sound)



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

  Orbit Family ID:    F018

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  Omega Name:          omega_perspectival_temporal_scarcity
  Severity Score:      0.18
  Severity Category:   low
  Gap Class:           powerless_blind
  Gap Pattern:         general_type_mismatch
  Epsilon:             0.15
  Suppression:         0.35
  Family ID:           F018
  Resolution Strategy: Affected population cannot classify. Test whether opacity serves extraction (deliberate complexity) or coordination (legitimate technical barrier). Measure information asymmetry.

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


Constraint: temporal_scarcity
  Claimed Type: piton
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: piton (Matches Claim)
    - [context(agent_power(institutional),time_horizon(immediate),exit_options(constrained),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: piton (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: temporal_scarcity]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for temporal_scarcity

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  temporal_scarcity: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for temporal_scarcity: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.375,0.25),excess_above_floor(0.09999999999999999)]. Coupling score=0.375. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: digital_platforms
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.47

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: temporal_scarcity
    - Individual (Powerless): piton [d=0.900 f(d)=1.36 χ=0.16]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.17 (moderate)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: temporal_scarcity]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_perspectival_temporal_scarcity (conceptual)
     Question: Constraint temporal_scarcity appears as piton to individuals but rope to institutions...
     Source: gap(general_type_mismatch,piton,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_perspectival_temporal_scarcity (conceptual)
      Constraint temporal_scarcity appears as piton to individuals but rope to institutions...

  [moderate] 1 omega(s):
    - omega_temporal_scarcity (empirical)
      Uncertainty about the historical function (Scaffold vs. Piton) of rewarding publication speed.

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

  ┌─ [omega_temporal_scarcity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Uncertainty about the historical function (Scaffold vs. Piton) of rewarding publication speed.
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

  ┌─ [omega_perspectival_temporal_scarcity] CONCEPTUAL CLARIFICATION
  │  Constraint: temporal_scarcity
  │  Gap: Constraint temporal_scarcity appears as piton to individuals but rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive temporal_scarcity
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
