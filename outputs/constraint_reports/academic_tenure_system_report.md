CORPUS CONTEXT: 1034 constraints
  Types: 128 mountain, 60 rope, 663 tangled_rope, 68 snare, 92 piton, 21 scaffold, 1 [social_governance]
  Network stability: cascading | 887 omegas (702 critical)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/academic_tenure_system.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: academic_tenure_system...
  [BRIDGE] Imported omega omega_tenure_extraction_intent from module academic_tenure_system
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=0
  [FIXED] Imputed 0.6 for suppression(organizational) at T=0
  [FIXED] Imputed 0.5 for resistance(organizational) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(organizational) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(organizational) at T=10
  [FIXED] Imputed 0.6 for suppression(organizational) at T=10
  [FIXED] Imputed 0.5 for resistance(organizational) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=0
  [FIXED] Imputed 0.6 for suppression(class) at T=0
  [FIXED] Imputed 0.5 for resistance(class) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(class) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(class) at T=10
  [FIXED] Imputed 0.6 for suppression(class) at T=10
  [FIXED] Imputed 0.5 for resistance(class) at T=10
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=0
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=0
  [FIXED] Imputed 0.6 for suppression(individual) at T=0
  [FIXED] Imputed 0.5 for resistance(individual) at T=0
  [FIXED] Imputed 0.5 for accessibility_collapse(individual) at T=10
  [FIXED] Imputed 0.5 for stakes_inflation(individual) at T=10
  [FIXED] Imputed 0.6 for suppression(individual) at T=10
  [FIXED] Imputed 0.5 for resistance(individual) at T=10

[REPAIR] Auditing vectors for: academic_tenure_system...

>>> INITIATING DR-AUDIT SUITE: academic_tenure_system

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: academic_tenure_system (0-10)
Checking Interval: academic_tenure_system (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] academic_tenure_system from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX OK] academic_tenure_system from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] academic_tenure_system from context(agent_power(analytical),time_horizon(historical),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: academic_tenure_system ===
====================================================

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  academic_tenure_system:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.15,0.52)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.4,0.75)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.7)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  academic_tenure_system -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  academic_tenure_system (ε=0.75):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.75 × 1.36 × 0.80 = 0.815
    moderate@national: d=0.700 f(d)=1.11 χ = 0.75 × 1.11 × 1.00 = 0.830
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.75 × -0.04 × 1.00 = -0.032
    analytical@global: d=0.720 f(d)=1.14 χ = 0.75 × 1.14 × 1.20 = 1.027

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: academic_tenure_system ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
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

  Corpus Distribution:
    Type:      128 mountain | 60 rope | 663 tangled_rope | 68 snare | 92 piton | 21 scaffold | 1 [social_governance]
    Purity:    143 pristine | 57 sound | 362 borderline | 434 contaminated | 12 degraded
    Coupling:  809 strongly | 52 weakly | 146 independent | 26 inconclusive
    Signature: 789 false_ci_rope | 114 natural_law | 91 false_natural_law | 14 constructed_low_extraction | 13 constructed_high_extraction | ...

  Positioning:
    This constraint is a false_ci_rope (76.3% of corpus shares this signature)
    Purity band: borderline (35.0% of corpus in this band)

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant
  Orbit Family ID:    F003

--- MAXENT SHADOW CLASSIFICATION ---

  No MaxEnt flags — classification is stable (low entropy, types agree)

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_academic_tenure_system
    Severity Score:    0.715
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F003

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       6
  Types Produced:      3

  Structural Twin Group:
    Signature:   (0.8, 0.6, False, True)
    Group Size:  24
    Types:       piton, snare, rope, tangled_rope

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  indexically_opaque -> snare, indexically_opaque -> tangled_rope, rope -> indexically_opaque, tangled_rope -> snare

====================================================
   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[CONSTRAINT INVENTORY: INDEXICAL AUDIT]


Constraint: academic_tenure_system
  Claimed Type: tangled_rope
  Perspectives:
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))]: snare (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(analytical),time_horizon(historical),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for academic_tenure_system

[STRUCTURAL SIGNATURE ANALYSIS]
  academic_tenure_system: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for academic_tenure_system: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.25),excess_above_floor(0.7)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: research_universities
    → Institutional d=0.120

[UKE_DR FEASIBILITY BRIDGE]
  Recommendation | UKE Status
  ----------------------------------------------------------------------

Aggregate Magnitude (Kappa) at Tn: 0.52

[PERSPECTIVAL GAP ANALYSIS]

  Analysis for Constraint: academic_tenure_system
    ! ALERT: Extractive "Snare" is masked as functional "Rope".
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.82]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.03 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.85 (high)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: academic_tenure_system]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_extraction_blindness_academic_tenure_system (conceptual)
     Question: Constraint academic_tenure_system appears extractive (Snare) to individuals but functional (Rope) to institutions...
     Source: gap(snare_masked_as_rope,snare,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 2 omega(s):
    - omega_tenure_extraction_intent (empirical)
      Is the 0.75 extraction a functional necessity for intellectual rigor or a predatory bypass for senior labor?
    - omega_extraction_blindness_academic_tenure_system (conceptual)
      Constraint academic_tenure_system appears extractive (Snare) to individuals but functional (Rope) to institutions...

  [high] 1 omega(s):
    - omega_extraction_blindness_academic_tenure_system (conceptual)
      Constraint academic_tenure_system appears extractive (Snare) to individuals but functional (Rope) to institutions...

  [moderate] 1 omega(s):
    - omega_tenure_extraction_intent (empirical)
      Is the 0.75 extraction a functional necessity for intellectual rigor or a predatory bypass for senior labor?

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

  ┌─ [omega_tenure_extraction_intent] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Is the 0.75 extraction a functional necessity for intellectual rigor or a predatory bypass for senior labor?
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

  ┌─ [omega_extraction_blindness_academic_tenure_system] CONCEPTUAL CLARIFICATION
  │  Constraint: academic_tenure_system
  │  Gap: Constraint academic_tenure_system appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from academic_tenure_system?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does academic_tenure_system serve?
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
