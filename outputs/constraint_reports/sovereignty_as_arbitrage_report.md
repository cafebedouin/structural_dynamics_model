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
[SCENARIO MANAGER] Loading: testsets/sovereignty_as_arbitrage.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: sovereignty_as_arbitrage...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: sovereignty_as_arbitrage

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: sovereignty_as_arbitrage (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] sovereignty_as_arbitrage from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] sovereignty_as_arbitrage from context(agent_power(institutional),time_horizon(generational),exit_options(trapped),spatial_scope(national)): declared=snare, computed=indexically_opaque
  [INDEX MISMATCH] sovereignty_as_arbitrage from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(regional)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] sovereignty_as_arbitrage from context(agent_power(analytical),time_horizon(civil_izational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=unknown
  [INTENT] Result: stable (Confidence: high)

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
  Actors:     beneficiaries=concentrated  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=high
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.33))
  Purity:     0.623 (borderline)




--- CORPUS POSITIONING ---

  This Constraint:
    Claimed Type:     tangled_rope
    Live Type:        snare (powerless), tangled_rope (analytical)
    Batch Type:       tangled_rope (powerless), rope (institutional)
    Signature:        false_ci_rope
    Purity:           0.623 (borderline)
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

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant
  Orbit Family ID:    F018

--- MAXENT SHADOW CLASSIFICATION ---

  Not yet in MaxEnt batch — run full pipeline to include.
  (MaxEnt validates classification stability across the full corpus.)

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_sovereignty_as_arbitrage
    Severity Score:    0.486
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F018

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.42857142857142855 (low variance)
  Index Configs:       7
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 10
    Unique Type Shifts:  indexically_opaque -> tangled_rope, rope -> indexically_opaque

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
    - [context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))]: rope (Mismatch)
    - [context(agent_power(institutional),time_horizon(generational),exit_options(trapped),spatial_scope(national))]: snare (Mismatch)
    - [context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(regional))]: snare (Mismatch)
    - [context(agent_power(analytical),time_horizon(civil_izational),exit_options(analytical),spatial_scope(global))]: tangled_rope (Matches Claim)

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for sovereignty_as_arbitrage

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
    ! ALERT: Extractive "Snare" is masked as functional "Rope".
    - Individual (Powerless): snare [d=0.900 f(d)=1.36 χ=0.52]
    - Institutional (Manager): rope [d=0.120 f(d)=-0.04 χ=-0.02 → net benefit]
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

[OMEGA GENERATION FROM PERSPECTIVAL GAPS: sovereignty_as_arbitrage]
  Generated 1 Omega variables from perspectival gaps:

  Ω: omega_extraction_blindness_sovereignty_as_arbitrage (conceptual)
     Question: Constraint sovereignty_as_arbitrage appears extractive (Snare) to individuals but functional (Rope) to institutions...
     Source: gap(snare_masked_as_rope,snare,rope)


[OMEGA TRIAGE & PRIORITIZATION]

  [critical] 1 omega(s):
    - omega_extraction_blindness_sovereignty_as_arbitrage (conceptual)
      Constraint sovereignty_as_arbitrage appears extractive (Snare) to individuals but functional (Rope) to institutions...

  [moderate] 1 omega(s):
    - omega_u_s_recognition (empirical)
      Geopolitical recognition of separatist arbitrage

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

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

  ┌─ [omega_extraction_blindness_sovereignty_as_arbitrage] CONCEPTUAL CLARIFICATION
  │  Constraint: sovereignty_as_arbitrage
  │  Gap: Constraint sovereignty_as_arbitrage appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from sovereignty_as_arbitrage?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does sovereignty_as_arbitrage serve?
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
