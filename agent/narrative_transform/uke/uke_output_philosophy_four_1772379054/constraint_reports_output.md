
============================================================
CONSTRAINT REPORT: structural_position_constraint_divergence
============================================================
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
[SCENARIO MANAGER] Loading: testsets/structural_position_constraint_divergence.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: structural_position_constraint_divergence...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: structural_position_constraint_divergence

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: structural_position_constraint_divergence (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] structural_position_constraint_divergence from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] structural_position_constraint_divergence from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] structural_position_constraint_divergence from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] structural_position_constraint_divergence from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] structural_position_constraint_divergence from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  structural_position_constraint_divergence:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  structural_position_constraint_divergence (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: structural_position_constraint_divergence ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.976 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.976 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9760
    Effective purity:   0.9760
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | meritocratic_ideology_as_error_propagation | tangled_rope | explicit | 1.00 | 0.0473 |

  No significant contamination — purity unchanged across 1 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [mountain]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       mountain (powerless)
    Confidence:       0.9500 (deep)
    Rival Type:       rope (P=0.0100)
    Margin:           +0.9400
    Boundary:         mountain->rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9500 (deep)
  Rival Type:    rope (P=0.0100)
  Margin:        +0.9400
  Entropy:       0.1557
  Distribution:  mountain: 0.950, rope: 0.010, tangled_rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      mountain (P=0.9500)
  Entropy:       0.1557
  Distribution:  mountain: 0.950, rope: 0.010, tangled_rope: 0.010

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (10 subsystems):
    maxent, cohomology, signature, boltzmann, purity, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts (1):
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      132 mountain | 18 rope | 572 tangled_rope | 176 snare | 9 piton | 9 scaffold
    Purity:    141 pristine | 40 sound | 88 borderline | 629 contaminated | 18 degraded
    Coupling:  709 strongly | 20 weakly | 161 independent | 1 inconclusive
    Signature: 549 false_natural_law | 231 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 45 moderate | 181 borderline (mean: 0.755)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (13.8% of corpus shares this signature)
    Purity band: pristine (15.4% of corpus in this band)
    Confidence band: deep (75.3% of corpus in this band)
    Boundary zone: mountain->rope (132 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.2 (low variance)
  Index Configs:       5
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 0
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for structural_position_constraint_divergence

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  structural_position_constraint_divergence: natural_law (confidence: low)
    → NATURAL LAW signature for structural_position_constraint_divergence: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.03, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: collective_action_blockage_via_stratification
============================================================
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


============================================================
CONSTRAINT REPORT: meritocratic_ideology_as_error_propagation
============================================================
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
[SCENARIO MANAGER] Loading: testsets/meritocratic_ideology_as_error_propagation.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: meritocratic_ideology_as_error_propagation...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: meritocratic_ideology_as_error_propagation

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: meritocratic_ideology_as_error_propagation (0-45)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] meritocratic_ideology_as_error_propagation from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(national)): declared=snare, computed=naturalized
  [INDEX OK] meritocratic_ideology_as_error_propagation from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] meritocratic_ideology_as_error_propagation from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] meritocratic_ideology_as_error_propagation from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] meritocratic_ideology_as_error_propagation from context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(continental)): declared=piton, computed=rope
  [INDEX OK] meritocratic_ideology_as_error_propagation from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  meritocratic_ideology_as_error_propagation:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,45,0.42,0.68)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,45,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.615,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  meritocratic_ideology_as_error_propagation -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  meritocratic_ideology_as_error_propagation (ε=0.48):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.48 × 0.65 × 0.80 = 0.250
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: meritocratic_ideology_as_error_propagation ===
  Shift (computed via dr_type/3):
    powerless=naturalized  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.615 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.615 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9703 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6150
    Effective purity:   0.0473
    Propagation delta:  -0.5677

    Network neighbors (19):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | adversarial_truth_decay | tangled_rope | shared_victim | 0.30 | 0.0880 |
    | attention_as_bottleneck_resource | tangled_rope | shared_victim | 0.30 | 0.0871 |
    | awareness_without_leverage | tangled_rope | shared_victim | 0.30 | 0.0880 |
    | belief_argument_conclusion | tangled_rope | shared_victim | 0.30 | 0.0874 |
    | cognitive_induction_gap | tangled_rope | shared_victim | 0.30 | 0.0774 |
    | collective_action_blockage_via_stratification | tangled_rope | shared_victim | 0.30 | 0.2977 |
    | confirmation_bias | tangled_rope | shared_victim | 0.30 | 0.0852 |
    | consensus_without_truth | snare | shared_victim | 0.30 | 0.1740 |
    | data_laundering_pipeline | tangled_rope | shared_victim | 0.30 | 0.0509 |
    | epistemic_free_rider_problem | tangled_rope | shared_victim | 0.30 | 0.0745 |
    | epistemic_overload_collapse | tangled_rope | shared_victim | 0.30 | 0.0880 |
    | information_foraging_theory | tangled_rope | shared_victim | 0.30 | 0.0683 |
    | memetic_fitness_vs_truth | tangled_rope | shared_victim | 0.30 | 0.0871 |
    | model_collapse_feedback_loop | tangled_rope | shared_victim | 0.30 | 0.0787 |
    | narrative_overfitting | tangled_rope | shared_victim | 0.30 | 0.0512 |
    | reputational_cascade_failure | tangled_rope | shared_victim | 0.30 | 0.0845 |
    | structural_position_constraint_divergence | mountain | explicit | 1.00 | 0.9760 |
    | theatrical_neutrality | tangled_rope | shared_victim | 0.30 | 0.0880 |
    | utopia_apocalypse_fragility | tangled_rope | shared_victim | 0.30 | 0.0849 |

  Purity degraded from 0.6150 to 0.0473 by contamination from 19 neighbor(s), primarily data_laundering_pipeline (shared_victim, purity 0.0509).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_meritocratic_ideology_as_error_propagation
    Severity Score:    0.546
    Gap Class:         powerless_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F353

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate), rope (institutional)
    Confidence:       0.9672 (deep)
    Rival Type:       snare (P=0.0328)
    Margin:           +0.9344
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → tangled_rope; powerless → naturalized; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9672 (deep)
  Rival Type:    snare (P=0.0328)
  Margin:        +0.9344
  Entropy:       0.0807
  Distribution:  tangled_rope: 0.967, snare: 0.033, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9665)
  Entropy:       0.0821
  Distribution:  tangled_rope: 0.967, snare: 0.033, piton: 0.000

  Classical/Indexed TV Distance: 0.0007 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    maxent, signature, purity, fingerprint_voids, context_gap, fcr_gate

  Expected Conflicts (4):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,45,0.42,0.68),warning),drift(extraction_accumulation,evidence(extraction_delta,0,45,0.32,0.48),critical),drift(coupling_drift,evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.615,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.39999999999999997)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(adversarial_truth_decay,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.66)]),contagion(attention_as_bottleneck_resource,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(awareness_without_leverage,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(belief_argument_conclusion,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(cognitive_induction_gap,0.019875,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5)]),contagion(collective_action_blockage_via_stratification,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)]),contagion(confirmation_bias,0.019875,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(consensus_without_truth,0.045375,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(data_laundering_pipeline,0.017775,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.42999999999999994)]),contagion(epistemic_free_rider_problem,0.019875,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(epistemic_overload_collapse,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(information_foraging_theory,0.015362499999999998,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.36)]),contagion(memetic_fitness_vs_truth,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(model_collapse_feedback_loop,0.0187875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.37)]),contagion(reputational_cascade_failure,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(theatrical_neutrality,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(utopia_apocalypse_fragility,0.0220875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.30844000000000005,intrinsic_purity,0.615),critical)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      132 mountain | 18 rope | 572 tangled_rope | 176 snare | 9 piton | 9 scaffold
    Purity:    141 pristine | 40 sound | 88 borderline | 629 contaminated | 18 degraded
    Coupling:  709 strongly | 20 weakly | 161 independent | 1 inconclusive
    Signature: 549 false_natural_law | 231 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 45 moderate | 181 borderline (mean: 0.755)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.2% of corpus shares this signature)
    Purity band: borderline (9.6% of corpus in this band)
    Confidence band: deep (75.3% of corpus in this band)
    Boundary zone: tangled_rope->snare (146 constraints share this boundary)
    Orbit Family ID:  F353

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 14
    Unique Type Shifts:  naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 45
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for meritocratic_ideology_as_error_propagation

[STRUCTURAL SIGNATURE ANALYSIS]
  meritocratic_ideology_as_error_propagation: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for meritocratic_ideology_as_error_propagation: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.29),excess_above_floor(0.39999999999999997)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_evaluators
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  meritocratic_ideology_as_error_propagation (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.27 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [error_type_measurement_basis] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Ground truth basis for competence evaluation error rates
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

  ┌─ [coordination_floor_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Minimum extractiveness floor for evaluation systems
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

  ┌─ [identity_lock_vs_structural_trap] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Cognitive vs material binding mechanism for marginalized actors
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

  ┌─ [cassandra_analytical_capacity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether marginalized position correlates with analytical capacity
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

  ┌─ [omega_extraction_blindness_meritocratic_ideology_as_error_propagation] CONCEPTUAL CLARIFICATION
  │  Constraint: meritocratic_ideology_as_error_propagation
  │  Gap: Constraint meritocratic_ideology_as_error_propagation appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from meritocratic_ideology_as_error_propagation?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does meritocratic_ideology_as_error_propagation serve?
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

