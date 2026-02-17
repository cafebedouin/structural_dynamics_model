
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fiscal_equalization_friction.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fiscal_equalization_friction...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.15 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.15 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.15 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.15 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.15 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.15 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: fiscal_equalization_friction...

>>> INITIATING DR-AUDIT SUITE: fiscal_equalization_friction

[REPAIR] Auditing vectors for: fiscal_equalization_friction...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fiscal_equalization_friction (0-10)
Checking Interval: fiscal_equalization_friction (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: fiscal_equalization_friction

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): rope

From ANALYTICAL perspective: rope

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] fiscal_equalization_friction from context(agent_power(moderate),time_horizon(generational),exit_options(trapped),spatial_scope(national)): declared=snare, computed=unknown
  [INDEX MISMATCH] fiscal_equalization_friction from context(agent_power(institutional),time_horizon(historical),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] fiscal_equalization_friction from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=unknown
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: fiscal_equalization_friction ===

[CONSTRAINT INVENTORY]
  - fiscal_equalization_friction: coordination_rope (Intensity: 0.55)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 2 | Watch: 0

  fiscal_equalization_friction:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.28,0.32)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.48616666666666664,decline_signals,[coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.17)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fiscal_equalization_friction -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fiscal_equalization_friction (ε=0.32):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.32 × 1.36 × 0.80 = 0.348
    moderate@national: d=0.700 f(d)=1.11 χ = 0.32 × 1.11 × 1.00 = 0.354
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.32 × -0.04 × 1.00 = -0.014
    analytical@global: d=0.720 f(d)=1.14 χ = 0.32 × 1.14 × 1.20 = 0.438

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fiscal_equalization_friction ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=unknown  institutional=scaffold  analytical=unknown
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      [no_exit_for_victims,self_sustaining_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=stable  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=low
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.486 (contaminated)



--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold, unknown]
  Orbit Span:         2
  Number of Contexts: 4
  Gauge Status:       Gauge-Variant

  Per-context types:
    powerless       -> unknown
    moderate        -> unknown
    institutional   -> scaffold
    analytical      -> unknown

  Orbit Family ID:    No family assignment (no omega generated)

--- MAXENT SHADOW CLASSIFICATION ---

  High Uncertainty (but types agree)
  H_norm:        0.4341
  Confidence:    0.5659
  Top P:         0.4939
  Det Type:      unknown
  Shadow Top:    rope

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       4
  Types Produced:      2

  Structural Twins: Not in any structural twin group

  Covering Analysis:
    Missed Transitions: 8
    Unique Type Shifts:  rope -> unknown, scaffold -> rope, scaffold -> unknown

--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: fiscal_equalization_friction
  Claimed Type: rope
  Perspectives:
    - [context(agent_power(moderate),time_horizon(generational),exit_options(trapped),spatial_scope(national))]: snare (Mismatch)
    - [context(agent_power(institutional),time_horizon(historical),exit_options(arbitrage),spatial_scope(national))]: rope (Matches Claim)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: rope (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: fiscal_equalization_friction]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for fiscal_equalization_friction

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  fiscal_equalization_friction: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for fiscal_equalization_friction: Appears to be rope (explicit_rope_claim) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([scaffold,unknown]),excess_above_floor(0.17),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: federal_standards
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.44

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: fiscal_equalization_friction
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.35]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: fiscal_equalization_friction]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_fair_deal_metric (conceptual)
      Definitional threshold of fiscal fairness

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_fair_deal_metric] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Definitional threshold of fiscal fairness
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
