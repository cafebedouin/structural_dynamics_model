
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/sovereignty_as_arbitrage.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: sovereignty_as_arbitrage...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.42 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.42 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.42 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.42 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.42 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.42 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: sovereignty_as_arbitrage...

>>> INITIATING DR-AUDIT SUITE: sovereignty_as_arbitrage

[REPAIR] Auditing vectors for: sovereignty_as_arbitrage...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: sovereignty_as_arbitrage (0-10)
Checking Interval: sovereignty_as_arbitrage (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: sovereignty_as_arbitrage

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] sovereignty_as_arbitrage from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=indexically_opaque
  [INDEX MISMATCH] sovereignty_as_arbitrage from context(agent_power(institutional),time_horizon(generational),exit_options(trapped),spatial_scope(national)): declared=snare, computed=indexically_opaque
  [INDEX OK] sovereignty_as_arbitrage from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: sovereignty_as_arbitrage ===

[CONSTRAINT INVENTORY]
  - sovereignty_as_arbitrage: hybrid_extraction (Intensity: 0.48)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  sovereignty_as_arbitrage:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.4,0.65)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.35,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.623,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.38)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  sovereignty_as_arbitrage -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  sovereignty_as_arbitrage (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: sovereignty_as_arbitrage ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=high
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.33))
  Purity:     0.623 (borderline)



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

  Orbit Family ID:    No family assignment (no omega generated)

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.75 (high variance)
  Index Configs:       4
  Types Produced:      3

  Structural Twins: Not in any structural twin group

  Covering Analysis:
    Missed Transitions: 10
    Unique Type Shifts:  indexically_opaque -> tangled_rope, rope -> indexically_opaque

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: sovereignty_as_arbitrage
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional))]: rope (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(trapped),spatial_scope(national))]: snare (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: sovereignty_as_arbitrage]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for sovereignty_as_arbitrage

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  sovereignty_as_arbitrage: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for sovereignty_as_arbitrage: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.33),excess_above_floor(0.38)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: separatist_leadership
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.49

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: sovereignty_as_arbitrage
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.52]
    - Institutional (Manager): snare [d=0.120 f(d)=-0.04 χ=-0.02 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: sovereignty_as_arbitrage]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [moderate] 1 omega(s):
    - omega_u_s_recognition (empirical)
      Geopolitical recognition of separatist arbitrage

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_u_s_recognition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Geopolitical recognition of separatist arbitrage
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
