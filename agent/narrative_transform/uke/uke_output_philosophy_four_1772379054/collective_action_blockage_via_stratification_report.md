CORPUS CONTEXT: 917 constraints
  Types: 132 mountain, 18 rope, 572 tangled_rope, 176 snare, 9 piton, 9 scaffold
  Network stability: cascading | 768 omegas (713 critical)
  Confidence: 690 deep (75%) | 45 moderate (5%) | 181 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/collective_action_blockage_via_stratification.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: collective_action_blockage_via_stratification...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: collective_action_blockage_via_stratification

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: collective_action_blockage_via_stratification (0-100)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] collective_action_blockage_via_stratification from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] collective_action_blockage_via_stratification from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] collective_action_blockage_via_stratification from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX OK] collective_action_blockage_via_stratification from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] collective_action_blockage_via_stratification from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=tangled_rope
  [INDEX OK] collective_action_blockage_via_stratification from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  collective_action_blockage_via_stratification:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,100,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,100,0.52,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  collective_action_blockage_via_stratification -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  collective_action_blockage_via_stratification (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: collective_action_blockage_via_stratification ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_natural_law
    Purity:           0.3125 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3125
    Effective purity:   0.2977
    Propagation delta:  -0.0149

    Network neighbors (4):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | cross_class_coalition_formation | unknown | explicit | 1.00 | N/A |
    | epistemic_commons_fragmentation | unknown | explicit | 1.00 | N/A |
    | labor_organizing_barriers | unknown | explicit | 1.00 | N/A |
    | meritocratic_ideology_as_error_propagation | tangled_rope | shared_victim | 0.30 | 0.0473 |

  Purity degraded from 0.3125 to 0.2977 by contamination from 4 neighbor(s), primarily meritocratic_ideology_as_error_propagation (shared_victim, purity 0.0473).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_collective_action_blockage_via_stratification
    Severity Score:    0.565
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F168

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

    Type:      132 mountain | 18 rope | 572 tangled_rope | 176 snare | 9 piton | 9 scaffold
    Purity:    141 pristine | 40 sound | 88 borderline | 629 contaminated | 18 degraded
    Coupling:  709 strongly | 20 weakly | 161 independent | 1 inconclusive
    Signature: 549 false_natural_law | 231 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 45 moderate | 181 borderline (mean: 0.755)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (59.9% of corpus shares this signature)
    Purity band: contaminated (68.6% of corpus in this band)
    Confidence band: borderline (19.8% of corpus in this band)
    Boundary zone: snare->tangled_rope (173 constraints share this boundary)
    Orbit Family ID:  F168

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 100
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  collective_action_blockage_via_stratification: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for collective_action_blockage_via_stratification: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=1.000 with 6 coupled dimension pairs. Excess extraction=0.6000000000000001. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.56

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  collective_action_blockage_via_stratification (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.38 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [recognition_threshold_ambiguity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Recognition threshold for coalition formation
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

  ┌─ [transformation_rule_counterfactual] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Sufficiency of recognition for power transformation
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

  ┌─ [stratification_maintenance_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Structural vs internalized stratification maintenance
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

  ┌─ [elite_defection_dynamics] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Elite defection frequency and triggers
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

  ┌─ [omega_extraction_blindness_collective_action_blockage_via_stratification] CONCEPTUAL CLARIFICATION
  │  Constraint: collective_action_blockage_via_stratification
  │  Gap: Constraint collective_action_blockage_via_stratification appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from collective_action_blockage_via_stratification?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does collective_action_blockage_via_stratification serve?
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
