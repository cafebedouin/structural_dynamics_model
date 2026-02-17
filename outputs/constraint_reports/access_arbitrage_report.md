
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/access_arbitrage.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: access_arbitrage...
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.5 for suppression(organizational) at T=0
  [FIXED] Imputed 0.6 for resistance(organizational) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.5 for suppression(organizational) at T=10
  [FIXED] Imputed 0.6 for resistance(organizational) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.5 for suppression(class) at T=0
  [FIXED] Imputed 0.6 for resistance(class) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.5 for suppression(class) at T=10
  [FIXED] Imputed 0.6 for resistance(class) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.5 for suppression(individual) at T=0
  [FIXED] Imputed 0.6 for resistance(individual) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.5 for suppression(individual) at T=10
  [FIXED] Imputed 0.6 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: access_arbitrage...

>>> INITIATING DR-AUDIT SUITE: access_arbitrage

[REPAIR] Auditing vectors for: access_arbitrage...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: access_arbitrage (0-10)
Checking Interval: access_arbitrage (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: access_arbitrage

No classification from your perspective

No analytical classification


--- PER-INDEX VALIDATION ---
  [INDEX OK] access_arbitrage from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX OK] access_arbitrage from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: access_arbitrage ===

[CONSTRAINT INVENTORY]
  - access_arbitrage: hybrid_extraction (Intensity: 0.45)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  0
  Critical: 0 | Warning: 0 | Watch: 0

  No drift events detected.

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  access_arbitrage (ε=0.45):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.45 × 1.36 × 0.80 = 0.489
    moderate@national: d=0.700 f(d)=1.11 χ = 0.45 × 1.11 × 1.00 = 0.498
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.45 × -0.04 × 1.00 = -0.019
    analytical@global: d=0.720 f(d)=1.14 χ = 0.45 × 1.14 × 1.20 = 0.616

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: access_arbitrage ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=low  suppression=high
  Coupling:   inconclusive (insufficient data)



--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Number of Contexts: 4
  Gauge Status:       Gauge-Variant

  Per-context types:
    powerless       -> tangled_rope
    moderate        -> tangled_rope
    institutional   -> rope
    analytical      -> tangled_rope

  Orbit Family ID:    F074

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  Omega Name:          omega_perspectival_access_arbitrage
  Severity Score:      0.495
  Severity Category:   high
  Gap Class:           coordination_washing
  Gap Pattern:         general_type_mismatch
  Epsilon:             0.45
  Suppression:         0.5
  Family ID:           F074
  Resolution Strategy: Map beneficiary flows. Interview affected populations to verify coordination claim. Test: who controls change mechanisms? If control is asymmetric, extraction is likely.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       3
  Types Produced:      2

  Structural Twin Group:
    Signature:   (0.5, 0.5, False, True)
    Group Size:  13
    Types:       tangled_rope, rope

  Covering Analysis:
    Missed Transitions: 8
    Unique Type Shifts:  rope -> tangled_rope, rope -> unknown, unknown -> tangled_rope

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: access_arbitrage
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: tangled_rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: access_arbitrage]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for access_arbitrage

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  access_arbitrage: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for access_arbitrage: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [excess_above_floor(0.4),nonsensical_coupling(0.16666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: government_institutions
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.58

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: access_arbitrage
    - Individual (Powerless): tangled_rope [d=0.900 f(d)=1.36 χ=0.49]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.02 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.51 (high)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: access_arbitrage]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_perspectival_access_arbitrage (conceptual)
     Question: Constraint access_arbitrage appears as tangled_rope to individuals but rope to institutions...
     Source: gap(general_type_mismatch,tangled_rope,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_perspectival_access_arbitrage (conceptual)
      Constraint access_arbitrage appears as tangled_rope to individuals but rope to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_perspectival_access_arbitrage] CONCEPTUAL CLARIFICATION
  │  Constraint: access_arbitrage
  │  Gap: Constraint access_arbitrage appears as tangled_rope to individuals but rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive access_arbitrage
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
