CORPUS CONTEXT: 979 constraints
  Types: 146 mountain, 25 rope, 598 tangled_rope, 190 snare, 9 piton, 10 scaffold
  Network stability: cascading | 811 omegas (749 critical)
  Confidence: 728 deep (74%) | 54 moderate (6%) | 196 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/visibility_as_incitement.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: visibility_as_incitement...
  [BRIDGE] Derived has_sunset_clause(visibility_as_incitement) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: visibility_as_incitement

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: visibility_as_incitement (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(regional)): declared=snare, computed=tangled_rope
  [INDEX OK] visibility_as_incitement from context(agent_power(moderate),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INDEX MISMATCH] visibility_as_incitement from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=scaffold, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  visibility_as_incitement:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,6,0.45,0.68)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.62,0.78)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.625,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.5708333333333333,decline_signals,[extraction_rising,coupling_above_threshold(0.625),theater_rising,excess_above_floor(0.68)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  visibility_as_incitement -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  visibility_as_incitement (ε=0.78):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.78 × 0.65 × 0.80 = 0.406
    moderate@national: d=0.700 f(d)=1.11 χ = 0.78 × 1.11 × 1.00 = 0.863
    institutional@national: d=0.850 f(d)=1.31 χ = 0.78 × 1.31 × 1.00 = 1.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.78 × 1.14 × 1.20 = 1.069

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: visibility_as_incitement ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.625, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0)], boltzmann=non_compliant(0.625,0.33))
  Purity:     0.571 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_natural_law
    Purity:           0.6875 (borderline)
    Coupling:         weakly_coupled (score: 0.375)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6875
    Effective purity:   0.6875
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_visibility_as_incitement
    Severity Score:    0.645
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F069

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.0100 (borderline)
    Rival Type:       tangled_rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         snare->tangled_rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says snare, MaxEnt says tangled_rope
  Confidence:    0.0100 (borderline)
  Rival Type:    tangled_rope (P=0.9500)
  Margin:        -0.9400
  Entropy:       0.1557
  Distribution:  tangled_rope: 0.950, mountain: 0.010, rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9500)
  Entropy:       0.1557
  Distribution:  tangled_rope: 0.950, mountain: 0.010, rope: 0.010

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  No abductive triggers fired. All diagnostic paths agree.

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (9 subsystems):
    maxent, cohomology, abductive, signature, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions: none

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      146 mountain | 25 rope | 598 tangled_rope | 190 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 101 borderline | 655 contaminated | 19 degraded
    Coupling:  746 strongly | 22 weakly | 183 independent | 1 inconclusive
    Signature: 560 false_natural_law | 267 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 728 deep | 54 moderate | 196 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (57.2% of corpus shares this signature)
    Purity band: borderline (10.3% of corpus in this band)
    Confidence band: borderline (20.0% of corpus in this band)
    Boundary zone: snare->tangled_rope (187 constraints share this boundary)
    Orbit Family ID:  F069

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 11
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  visibility_as_incitement: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for visibility_as_incitement: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=0.625 with 2 coupled dimension pairs. Excess extraction=0.68. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.58

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  visibility_as_incitement (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.62 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [legibility_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Repetition threshold for visibility to catalyze resistance
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

  ┌─ [suppression_visibility_tradeoff] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether suppression requires public visibility
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

  ┌─ [witness_identity_lock_mechanism] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Mechanism of identity fusion in forced witnesses
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

  ┌─ [incitement_coordination_boundary] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold for visibility to enable collective coordination
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

  ┌─ [omega_extraction_blindness_visibility_as_incitement] CONCEPTUAL CLARIFICATION
  │  Constraint: visibility_as_incitement
  │  Gap: Constraint visibility_as_incitement appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from visibility_as_incitement?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does visibility_as_incitement serve?
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
