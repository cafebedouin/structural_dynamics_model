CORPUS CONTEXT: 926 constraints
  Types: 133 mountain, 18 rope, 577 tangled_rope, 178 snare, 9 piton, 10 scaffold
  Network stability: cascading | 776 omegas (720 critical)
  Confidence: 690 deep (75%) | 51 moderate (6%) | 184 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/coordination_extraction_invisibility.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: coordination_extraction_invisibility...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: coordination_extraction_invisibility

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: coordination_extraction_invisibility (0-12)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] coordination_extraction_invisibility from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] coordination_extraction_invisibility from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] coordination_extraction_invisibility from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX OK] coordination_extraction_invisibility from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] coordination_extraction_invisibility from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] coordination_extraction_invisibility from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  coordination_extraction_invisibility:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,12,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,12,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  coordination_extraction_invisibility -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  coordination_extraction_invisibility (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: coordination_extraction_invisibility ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_natural_law
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.0000
    Propagation delta:  -0.3542

    Network neighbors (19):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | adversarial_truth_decay | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | attention_as_bottleneck_resource | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | awareness_without_leverage | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | belief_argument_conclusion | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | cognitive_induction_gap | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | confirmation_bias | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | consensus_without_truth | snare | shared_victim | 0.30 | 0.1016 |
    | data_laundering_pipeline | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | epistemic_free_rider_problem | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | epistemic_overload_collapse | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | information_foraging_theory | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | memetic_fitness_vs_truth | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | meritocratic_ideology_as_error_propagation | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | model_collapse_feedback_loop | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | narrative_overfitting | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | power_indexed_classification_variance | mountain | explicit | 1.00 | 0.9760 |
    | reputational_cascade_failure | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | theatrical_neutrality | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | utopia_apocalypse_fragility | tangled_rope | shared_victim | 0.30 | 0.0000 |

  Purity degraded from 0.3542 to 0.0000 by contamination from 19 neighbor(s), primarily adversarial_truth_decay (shared_victim, purity 0.0000).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_coordination_extraction_invisibility
    Severity Score:    0.556
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F181

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

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, cohomology, signature, purity, fingerprint_voids, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,12,0.35,0.58),warning),drift(extraction_accumulation,evidence(extraction_delta,0,12,0.48,0.68),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(adversarial_truth_decay,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.66)]),contagion(attention_as_bottleneck_resource,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(awareness_without_leverage,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(belief_argument_conclusion,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(cognitive_induction_gap,0.00031250000000000304,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5)]),contagion(confirmation_bias,0.00031250000000000304,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(consensus_without_truth,0.006250000000000003,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(epistemic_free_rider_problem,0.00031250000000000304,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(epistemic_overload_collapse,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(memetic_fitness_vs_truth,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(reputational_cascade_failure,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(theatrical_neutrality,0.0031250000000000015,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(utopia_apocalypse_fragility,0.0025250000000000008,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.32639666666666667,intrinsic_purity,0.3541666666666667),critical)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      133 mountain | 18 rope | 577 tangled_rope | 178 snare | 9 piton | 10 scaffold
    Purity:    142 pristine | 41 sound | 89 borderline | 635 contaminated | 18 degraded
    Coupling:  715 strongly | 21 weakly | 163 independent | 1 inconclusive
    Signature: 552 false_natural_law | 236 false_ci_rope | 128 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 51 moderate | 184 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (59.6% of corpus shares this signature)
    Purity band: contaminated (68.6% of corpus in this band)
    Confidence band: borderline (19.9% of corpus in this band)
    Boundary zone: snare->tangled_rope (175 constraints share this boundary)
    Orbit Family ID:  F181

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twin Group:
    Signature:   (0.7, 0.7, False, True)
    Group Size:  24
    Types:       tangled_rope, snare

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 12
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  coordination_extraction_invisibility: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for coordination_extraction_invisibility: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=1.000 with 4 coupled dimension pairs. Excess extraction=0.6000000000000001. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.55

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  coordination_extraction_invisibility (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.77 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [visibility_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Power threshold for extraction visibility in hybrid constraints
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

  ┌─ [exit_option_causality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Causal direction between exit options and coordination experience
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

  ┌─ [extraction_concentration_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Mechanism of extraction concentration on powerless agents
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

  ┌─ [coordination_function_necessity] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Structural necessity of extraction for coordination function
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

  ┌─ [omega_extraction_blindness_coordination_extraction_invisibility] CONCEPTUAL CLARIFICATION
  │  Constraint: coordination_extraction_invisibility
  │  Gap: Constraint coordination_extraction_invisibility appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from coordination_extraction_invisibility?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does coordination_extraction_invisibility serve?
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
