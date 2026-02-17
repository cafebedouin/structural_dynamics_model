
====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fossil_fuel_lock_in.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fossil_fuel_lock_in...
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.02 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.02 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.02 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.02 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.02 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.02 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: fossil_fuel_lock_in...

>>> INITIATING DR-AUDIT SUITE: fossil_fuel_lock_in

[REPAIR] Auditing vectors for: fossil_fuel_lock_in...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fossil_fuel_lock_in (0-10)
Checking Interval: fossil_fuel_lock_in (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: fossil_fuel_lock_in

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): mountain

From ANALYTICAL perspective: mountain

→ Perspectives AGREE

--- PER-INDEX VALIDATION ---
  [INDEX OK] fossil_fuel_lock_in from context(agent_power(moderate),time_horizon(biographical),exit_options(trapped),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] fossil_fuel_lock_in from context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] fossil_fuel_lock_in from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: fossil_fuel_lock_in ===

[CONSTRAINT INVENTORY]
  No active constraints found.

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  fossil_fuel_lock_in:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.12,0.15)

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fossil_fuel_lock_in -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fossil_fuel_lock_in (ε=0.15):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.15 × 1.36 × 0.80 = 0.163
    moderate@national: d=0.700 f(d)=1.11 χ = 0.15 × 1.11 × 1.00 = 0.166
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.15 × -0.04 × 1.00 = -0.006
    analytical@global: d=0.720 f(d)=1.14 χ = 0.15 × 1.14 × 1.20 = 0.205

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fossil_fuel_lock_in ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [asymmetric,coordination,has_beneficiaries,natural]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     1.000 (pristine)



--- ORBIT CONTEXT ---

  Orbit Signature:    [mountain]
  Orbit Span:         1
  Number of Contexts: 4
  Gauge Status:       Gauge-Invariant

  Per-context types:
    powerless       -> mountain
    moderate        -> mountain
    institutional   -> mountain
    analytical      -> mountain

  Orbit Family ID:    No family assignment (no omega generated)

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  No omega violations detected — perspectives agree on
  classification for this constraint.

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.25 (low variance)
  Index Configs:       4
  Types Produced:      1

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


Constraint: fossil_fuel_lock_in
  Claimed Type: mountain
  Perspectives:
    - [context(agent_power(moderate),time_horizon(biographical),exit_options(trapped),spatial_scope(regional))]: mountain (Matches Claim)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(national))]: mountain (Matches Claim)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: mountain (Matches Claim)

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: fossil_fuel_lock_in]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for fossil_fuel_lock_in

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  fossil_fuel_lock_in: natural_law (confidence: low)
    → NATURAL LAW signature for fossil_fuel_lock_in: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.02, resistance=0.05). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.42

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: fossil_fuel_lock_in
    - Individual (Powerless): none [d=0.900 f(d)=1.36 χ=0.16]
    - Institutional (Manager): mountain [d=0.120 f(d)=-0.04 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: fossil_fuel_lock_in]
  No perspectival gaps detected requiring Ω tracking.

[OMEGA TRIAGE & PRIORITIZATION]

  [high] 1 omega(s):
    - omega_lock_in_permanence (conceptual)
      Classification of resource dependency as natural law vs. policy choice

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_lock_in_permanence] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Classification of resource dependency as natural law vs. policy choice
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
