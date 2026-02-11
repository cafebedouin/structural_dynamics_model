?- run_scenario('./testsets/labor_union_dues_structure.pl', labor_union_dues_structure).

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: ./testsets/labor_union_dues_structure.pl...
Warning: /home/scott/bin/structural_dynamics_model/prolog/testsets/labor_union_dues_structure.pl:16:
Warning:    Local definition of domain_priors:base_extractiveness/2 overrides weak import from drl_core
Warning: /home/scott/bin/structural_dynamics_model/prolog/testsets/labor_union_dues_structure.pl:16:
Warning:    Local definition of domain_priors:suppression_score/2 overrides weak import from drl_core
Warning: /home/scott/bin/structural_dynamics_model/prolog/testsets/labor_union_dues_structure.pl:16:
Warning:    Local definition of domain_priors:requires_active_enforcement/1 overrides weak import from drl_core
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: labor_union_dues...
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.65 for suppression(organizational) at T=0
  [FIXED] Imputed 0.6 for resistance(organizational) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.65 for suppression(organizational) at T=10
  [FIXED] Imputed 0.6 for resistance(organizational) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.65 for suppression(class) at T=0
  [FIXED] Imputed 0.6 for resistance(class) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.65 for suppression(class) at T=10
  [FIXED] Imputed 0.6 for resistance(class) at T=10
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.65 for suppression(individual) at T=0
  [FIXED] Imputed 0.6 for resistance(individual) at T=0
  [FIXED] Imputed 0.55 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.73 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.65 for suppression(individual) at T=10
  [FIXED] Imputed 0.6 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: labor_union_dues_structure...
  [BRIDGE] Derived metric extractiveness = 0.1 for labor_union_dues_structure from config default
  [BRIDGE] Derived metric suppression_requirement = 0.1 for labor_union_dues_structure from config default
  [BRIDGE] Derived metric theater_ratio = 0.0 for labor_union_dues_structure from config default
  [BRIDGE] Derived constraint_claim(labor_union_dues_structure, rope) from computed analytical classification

>>> INITIATING DR-AUDIT SUITE: labor_union_dues_structure

[REPAIR] Auditing vectors for: labor_union_dues_structure...

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: labor_union_dues (0-10)
Checking Interval: labor_union_dues_structure (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

=== PERSPECTIVE COMPARISON ===
Constraint: labor_union_dues

From YOUR perspective (context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))): tangled_rope

From ANALYTICAL perspective: tangled_rope

→ Perspectives AGREE

=== PERSPECTIVE COMPARISON ===
Constraint: labor_union_dues_structure

No classification from your perspective

No analytical classification


--- PER-INDEX VALIDATION ---
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: labor_union_dues_structure ===

[CONSTRAINT INVENTORY]
  - labor_union_dues_structure: coordination_rope (Intensity: 1.00)

[FEASIBILITY BRIDGE]
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 2
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  labor_union_dues:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.35,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.5263333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.32999999999999996)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  labor_union_dues -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  labor_union_dues (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658
  labor_union_dues_structure (ε=0.10):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.10 × 1.42 × 0.80 = 0.114
    moderate@national: d=0.646 f(d)=1.00 χ = 0.10 × 1.00 × 1.00 = 0.100
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.10 × -0.12 × 1.00 = -0.012
    analytical@global: d=0.725 f(d)=1.15 χ = 0.10 × 1.15 × 1.20 = 0.138

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: labor_union_dues ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.526 (borderline)


=== Logical Fingerprint: labor_union_dues_structure ===
  Shift (computed via dr_type/3):
    powerless=rope  moderate=rope  institutional=rope  analytical=rope
  Properties: [has_temporal_data]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=low
  Coupling:   inconclusive (insufficient data)


--- SYSTEM INSIGHTS ---
  Omegas Identified: 0


====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: labor_union_dues
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: snare (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)


Constraint: labor_union_dues_structure
  Claimed Type: rope
  Perspectives:

[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: labor_union_dues_structure]
  No high-risk isomorphisms detected for current constraints.

[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]
  No cross-domain isomorphisms detected.

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  labor_union_dues: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for labor_union_dues: Appears to be rope (indexed_rope_classification) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),excess_above_floor(0.32999999999999996),nonsensical_coupling(0.16666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: union_administrative_body
    → Institutional d=0.120
  labor_union_dues_structure: constructed_low_extraction (confidence: low)
    → CONSTRUCTED LOW-EXTRACTION signature for labor_union_dues_structure: Enforcement present (suppression=0.10, resistance=0.50) but extraction is low (0.10). This is a rule-based coordination structure, not an extraction mechanism.

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.60

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: labor_union_dues
    ! ALERT: Extractive "Snare" is masked as functional "Rope".
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.52]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.02 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

  Analysis for Constraint: labor_union_dues_structure
    - Individual (Powerless): none [d=1.000 f(d)=1.42 χ=0.11]
    - Institutional (Manager): none [d=0.000 f(d)=-0.12 χ=-0.01 → net benefit]

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: labor_union_dues_structure]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_extraction_blindness_labor_union_dues (conceptual)
     Question: Constraint labor_union_dues appears extractive (Snare) to individuals but functional (Rope) to institutions...
     Source: gap(snare_masked_as_rope,snare,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 1 omega(s):
    - omega_extraction_blindness_labor_union_dues (conceptual)
      Constraint labor_union_dues appears extractive (Snare) to individuals but functional (Rope) to institutions...

  [high] 1 omega(s):
    - omega_extraction_blindness_labor_union_dues (conceptual)
      Constraint labor_union_dues appears extractive (Snare) to individuals but functional (Rope) to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_extraction_blindness_labor_union_dues] CONCEPTUAL CLARIFICATION
  │  Constraint: labor_union_dues
  │  Gap: Constraint labor_union_dues appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from labor_union_dues?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does labor_union_dues serve?
  │     - Who would object to removing it?
  │     - What alternatives exist?
  │  3. Document benefit flows:
  │     - Track who gains vs. who loses from status quo
  │     - Measure asymmetric benefit distribution
  │  4. Decision tree:
  │     IF extraction confirmed → Reclassify as SNARE
  │     IF functional & fair → Reclassify as ROPE
  │     IF context-dependent → Add indexical resolution
  └─

====================================================
true .

?-