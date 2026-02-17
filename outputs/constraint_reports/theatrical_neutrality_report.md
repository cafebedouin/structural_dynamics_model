
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/theatrical_neutrality.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: theatrical_neutrality...
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.72 for suppression(organizational) at T=0
  [FIXED] Imputed 0.6 for resistance(organizational) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.72 for suppression(organizational) at T=10
  [FIXED] Imputed 0.6 for resistance(organizational) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.72 for suppression(class) at T=0
  [FIXED] Imputed 0.6 for resistance(class) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.72 for suppression(class) at T=10
  [FIXED] Imputed 0.6 for resistance(class) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.72 for suppression(individual) at T=0
  [FIXED] Imputed 0.6 for resistance(individual) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.72 for suppression(individual) at T=10
  [FIXED] Imputed 0.6 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: theatrical_neutrality...

>>> INITIATING DR-AUDIT SUITE: theatrical_neutrality

[REPAIR] Auditing vectors for: theatrical_neutrality...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: theatrical_neutrality (0-10)
Checking Interval: theatrical_neutrality (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: theatrical_neutrality

No classification from your perspective

No analytical classification


--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] theatrical_neutrality from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=piton, computed=rope
  [INDEX OK] theatrical_neutrality from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=snare
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: theatrical_neutrality ===

[CONSTRAINT INVENTORY]
  - theatrical_neutrality: extractive_snare (Intensity: 0.68)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 3 | Warning: 0 | Watch: 0

  theatrical_neutrality:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.3,0.85)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.4,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  theatrical_neutrality -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  theatrical_neutrality (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: theatrical_neutrality ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,self_sustaining_extraction,unaccountable_extraction,unenforced_suppression]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   inconclusive (insufficient data)



--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Number of Contexts: 4
  Gauge Status:       Gauge-Variant

  Per-context types:
    powerless       -> snare
    moderate        -> snare
    institutional   -> rope
    analytical      -> snare

  Orbit Family ID:    F052

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  Omega Name:          omega_perspectival_theatrical_neutrality
  Severity Score:      0.716
  Severity Category:   critical
  Gap Class:           coordination_washing
  Gap Pattern:         general_type_mismatch
  Epsilon:             0.68
  Suppression:         0.72
  Family ID:           F052
  Resolution Strategy: Map beneficiary flows. Interview affected populations to verify coordination claim. Test: who controls change mechanisms? If control is asymmetric, extraction is likely.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       3
  Types Produced:      2

  Structural Twins: Not in any structural twin group

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  piton -> snare, piton -> unknown, rope -> piton, unknown -> snare

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: theatrical_neutrality
  Claimed Type: snare
  Perspectives:
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: piton (Mismatch)
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: snare (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: theatrical_neutrality]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for theatrical_neutrality

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  theatrical_neutrality: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for theatrical_neutrality: Enforcement present (suppression=0.72, resistance=0.50) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.62

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: theatrical_neutrality
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.74]
    - Institutional (Manager): piton [d=0.120 f(d)=-0.04 χ=-0.03 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.77 (high)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: theatrical_neutrality]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_perspectival_theatrical_neutrality (conceptual)
     Question: Constraint theatrical_neutrality appears as snare to individuals but piton to institutions...
     Source: gap(general_type_mismatch,snare,piton)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 1 omega(s):
    - omega_perspectival_theatrical_neutrality (conceptual)
      Constraint theatrical_neutrality appears as snare to individuals but piton to institutions...

  [high] 1 omega(s):
    - omega_perspectival_theatrical_neutrality (conceptual)
      Constraint theatrical_neutrality appears as snare to individuals but piton to institutions...

  [low] 1 omega(s):
    - omega_epistemic_exit (preference)
      Does the existence of "Theatrical Neutrality" create a structural demand for alternative, high-bias media as a compensatory mechanism?

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

  ┌─ [omega_epistemic_exit] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Does the existence of "Theatrical Neutrality" create a structural demand for alternative, high-bias media as a compensatory mechanism?
  │
  │  NOTE: Not resolvable via data or logic alone
  │
  │  RESOLUTION STRATEGY:
  │  1. Document competing value frameworks:
  │     - What values support current unknown?
  │     - What values oppose it?
  │     - Are these incommensurable?
  │  2. Propose scaffolded solution:
  │     - Design mechanism respecting both value sets
  │     - Create exit options for dissenters
  │     - Allow preference-based sorting
  │  3. Accept unresolvability if necessary:
  │     - Some omegas represent genuine value pluralism
  │     - Solution: coexistence, not consensus
  └─

  ┌─ [omega_perspectival_theatrical_neutrality] CONCEPTUAL CLARIFICATION
  │  Constraint: theatrical_neutrality
  │  Gap: Constraint theatrical_neutrality appears as snare to individuals but piton to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive theatrical_neutrality
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
