
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/columbia_2026_elections.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: colombia_2026_presidential_election...
  [BRIDGE] Imported omega omega_colombian_polarization from module colombia_2026_presidential_election
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

[REPAIR] Auditing vectors for: columbia_2026_elections...
  [BRIDGE] Derived metric extractiveness = 0.1 for columbia_2026_elections from config default
  [BRIDGE] Derived metric suppression_requirement = 0.1 for columbia_2026_elections from config default
  [BRIDGE] Derived metric theater_ratio = 0.0 for columbia_2026_elections from config default
  [BRIDGE] Derived constraint_claim(columbia_2026_elections, tangled_rope) from computed analytical classification

>>> INITIATING DR-AUDIT SUITE: columbia_2026_elections

[REPAIR] Auditing vectors for: columbia_2026_elections...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: colombia_2026_presidential_election (0-10)
Checking Interval: columbia_2026_elections (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: colombia_2026_presidential_election

No classification from your perspective

No analytical classification


=== PERSPECTIVE COMPARISON ===
Constraint: columbia_2026_elections

No classification from your perspective

No analytical classification


--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] columbia_2026_elections from agent_power(analytical): declared=mountain, computed=unknown
  [INDEX MISMATCH] columbia_2026_elections from agent_power(institutional): declared=rope, computed=unknown
  [INDEX MISMATCH] columbia_2026_elections from agent_power(powerless): declared=snare, computed=unknown
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: columbia_2026_elections ===

[CONSTRAINT INVENTORY]
  - colombia_2026_presidential_election: coordination_rope (Intensity: 0.80)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 2
  Total drift events:  1
  Critical: 0 | Warning: 1 | Watch: 0

  columbia_2026_elections:
    [warning] purity_drift
        Evidence: evidence(current_purity,0.8675,decline_signals,[coupling_above_threshold(0.375),excess_above_floor(0.05)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  colombia_2026_presidential_election (ε=0.45):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.45 × 1.36 × 0.80 = 0.489
    moderate@national: d=0.700 f(d)=1.11 χ = 0.45 × 1.11 × 1.00 = 0.498
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.45 × -0.04 × 1.00 = -0.019
    analytical@global: d=0.720 f(d)=1.14 χ = 0.45 × 1.14 × 1.20 = 0.616
  columbia_2026_elections (ε=0.10):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.10 × 1.42 × 0.80 = 0.114
    moderate@national: d=0.646 f(d)=1.00 χ = 0.10 × 1.00 × 1.00 = 0.100
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.10 × -0.12 × 1.00 = -0.012
    analytical@global: d=0.725 f(d)=1.15 χ = 0.10 × 1.15 × 1.20 = 0.138

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: colombia_2026_presidential_election ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=unknown  institutional=rope  analytical=unknown
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=low  suppression=moderate
  Coupling:   inconclusive (insufficient data)


=== Logical Fingerprint: columbia_2026_elections ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [has_temporal_data]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=low
  Coupling:   weakly_coupled (score=0.375, pairs=[], boltzmann=non_compliant(0.375,0.25))
  Purity:     0.868 (sound)



--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, unknown]
  Orbit Span:         2
  Number of Contexts: 4
  Gauge Status:       Gauge-Variant

  Per-context types:
    powerless       -> unknown
    moderate        -> unknown
    institutional   -> rope
    analytical      -> unknown

  Orbit Family ID:    No family assignment (no omega generated)

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      1.5 (high variance)
  Index Configs:       2
  Types Produced:      3

  Structural Twins: Not in any structural twin group

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


Constraint: colombia_2026_presidential_election
  Claimed Type: rope
  Perspectives:
    - [context(agent_power(powerful),time_horizon(biographical),exit_options(mobile),spatial_scope(national))]: rope (Matches Claim)


Constraint: columbia_2026_elections
  Claimed Type: tangled_rope
  Perspectives:
    - [agent_power(analytical)]: mountain (Mismatch)
    - [agent_power(institutional)]: rope (Mismatch)
    - [agent_power(powerless)]: snare (Mismatch)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: columbia_2026_elections]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for colombia_2026_presidential_election

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  colombia_2026_presidential_election: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for colombia_2026_presidential_election: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.4)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: political_elites
    → Institutional d=0.120
  columbia_2026_elections: false_natural_law (confidence: medium)
    → FALSE NATURAL LAW signature for columbia_2026_elections: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=0.375 with 0 coupled dimension pairs. Excess extraction=0.05. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.47

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: colombia_2026_presidential_election
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.49]
    - Institutional (Manager): none [d=0.120 f(d)=-0.04 χ=-0.02 → net benefit]

  Analysis for Constraint: columbia_2026_elections
    - Individual (Powerless): none [d=1.000 f(d)=1.42 χ=0.11]
    - Institutional (Manager): none [d=0.000 f(d)=-0.12 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: columbia_2026_elections]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [moderate] 1 omega(s):
    - omega_colombian_polarization (empirical)
      Will the two-round system intensify polarization to the point where centrist alternatives are structurally eliminated?

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_colombian_polarization] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Will the two-round system intensify polarization to the point where centrist alternatives are structurally eliminated?
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
