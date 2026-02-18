CORPUS CONTEXT: 1034 constraints
  Types: 128 mountain, 60 rope, 663 tangled_rope, 68 snare, 92 piton, 21 scaffold, 1 [social_governance]
  Network stability: cascading | 887 omegas (702 critical)
  Confidence: 403 deep (39%) | 162 moderate (16%) | 467 borderline (45%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/abstraction_boundary_overrun.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: abstraction_boundary_overrun...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: abstraction_boundary_overrun

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: abstraction_boundary_overrun (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] abstraction_boundary_overrun from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=snare
  [INDEX OK] abstraction_boundary_overrun from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=rope, computed=rope
  [INDEX MISMATCH] abstraction_boundary_overrun from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 3 | Warning: 1 | Watch: 0

  abstraction_boundary_overrun:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.7,0.88)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.75,0.81)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.79)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  abstraction_boundary_overrun -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  abstraction_boundary_overrun (ε=0.81):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.81 × 1.36 × 0.80 = 0.880
    moderate@national: d=0.700 f(d)=1.11 χ = 0.81 × 1.11 × 1.00 = 0.896
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.81 × -0.04 × 1.00 = -0.034
    analytical@global: d=0.720 f(d)=1.14 χ = 0.81 × 1.14 × 1.20 = 1.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: abstraction_boundary_overrun ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.25))
  Purity:     0.575 (borderline)




--- CORPUS POSITIONING ---

  This Constraint:
    Claimed Type:     tangled_rope
    Live Type:        snare (powerless), rope (institutional), tangled_rope (analytical)
    Batch Type:       snare (powerless), rope (institutional)
    Signature:        false_ci_rope
    Purity:           0.575 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        unknown
    Confidence:       0.0238 (borderline)
    Rival Type:       snare (P=0.9649)
    Margin:           -0.9411
    Boundary:         tangled_rope->snare
    Tangled psi:      0.9990 (snare_leaning)
    Coalition:        institutional_dissent

  Corpus Distribution:
    Type:      128 mountain | 60 rope | 663 tangled_rope | 68 snare | 92 piton | 21 scaffold | 1 [social_governance]
    Purity:    143 pristine | 57 sound | 362 borderline | 434 contaminated | 12 degraded
    Coupling:  809 strongly | 52 weakly | 146 independent | 26 inconclusive
    Signature: 789 false_ci_rope | 114 natural_law | 91 false_natural_law | 14 constructed_low_extraction | 13 constructed_high_extraction | ...
    Confidence: 403 deep | 162 moderate | 467 borderline (mean: 0.530)

  Positioning:
    This constraint is a false_ci_rope (76.3% of corpus shares this signature)
    Purity band: borderline (35.0% of corpus in this band)
    Confidence band: borderline (45.3% of corpus in this band)
    Boundary zone: tangled_rope->snare (553 constraints share this boundary)

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant
  Orbit Family ID:    F132

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says tangled_rope, MaxEnt says snare
  Confidence:    0.0238 (borderline)
  Rival Type:    snare (P=0.9649)
  Margin:        -0.9411
  Entropy:       0.0970
  Distribution:  snare: 0.965, tangled_rope: 0.024, piton: 0.011

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_abstraction_boundary_overrun
    Severity Score:    0.772
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F132

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      1.0 (stable)
  Index Configs:       3
  Types Produced:      3

  Structural Twin Group:
    Signature:   (0.8, 0.7, False, True)
    Group Size:  68
    Types:       tangled_rope, snare, piton

  Covering Analysis:
    Missed Transitions: 11
    Unique Type Shifts:  piton -> snare, piton -> tangled_rope, rope -> piton, tangled_rope -> snare

====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: abstraction_boundary_overrun
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national))]: snare (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(global))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for abstraction_boundary_overrun

[STRUCTURAL SIGNATURE ANALYSIS]
  abstraction_boundary_overrun: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for abstraction_boundary_overrun: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.25),excess_above_floor(0.79)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: abstraction_architects
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.53

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: abstraction_boundary_overrun
    ! ALERT: Extractive "Snare" is masked as functional "Rope".
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.88]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.03 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.91 (high)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: abstraction_boundary_overrun]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_extraction_blindness_abstraction_boundary_overrun (conceptual)
     Question: Constraint abstraction_boundary_overrun appears extractive (Snare) to individuals but functional (Rope) to institutions...
     Source: gap(snare_masked_as_rope,snare,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 1 omega(s):
    - omega_extraction_blindness_abstraction_boundary_overrun (conceptual)
      Constraint abstraction_boundary_overrun appears extractive (Snare) to individuals but functional (Rope) to institutions...

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_extraction_blindness_abstraction_boundary_overrun] CONCEPTUAL CLARIFICATION
  │  Constraint: abstraction_boundary_overrun
  │  Gap: Constraint abstraction_boundary_overrun appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from abstraction_boundary_overrun?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does abstraction_boundary_overrun serve?
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
