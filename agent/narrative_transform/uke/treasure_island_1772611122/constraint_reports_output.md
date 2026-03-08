
============================================================
CONSTRAINT REPORT: unilateral_condition_control
============================================================
CORPUS CONTEXT: 951 constraints
  Types: 141 mountain, 18 rope, 587 tangled_rope, 185 snare, 9 piton, 10 scaffold
  Network stability: cascading | 792 omegas (734 critical)
  Confidence: 703 deep (74%) | 57 moderate (6%) | 190 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/unilateral_condition_control.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: unilateral_condition_control...
  [BRIDGE] Derived has_sunset_clause(unilateral_condition_control) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: unilateral_condition_control

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: unilateral_condition_control (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] unilateral_condition_control from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] unilateral_condition_control from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] unilateral_condition_control from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] unilateral_condition_control from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] unilateral_condition_control from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  unilateral_condition_control:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.55)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.42,0.58)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3405,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.42999999999999994)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  unilateral_condition_control -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  unilateral_condition_control (ε=0.58):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.58 × 1.36 × 0.80 = 0.630
    moderate@national: d=0.700 f(d)=1.11 χ = 0.58 × 1.11 × 1.00 = 0.642
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.58 × -0.04 × 1.00 = -0.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: unilateral_condition_control ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.341 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.3405 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9966 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3405
    Effective purity:   0.3405
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_unilateral_condition_control
    Severity Score:    0.654
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F535

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional), snare (analytical)
    Confidence:       0.7101 (moderate)
    Rival Type:       snare (P=0.2899)
    Margin:           +0.4201
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: powerless, moderate → tangled_rope; institutional → rope; analytical → snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7101 (moderate)
  Rival Type:    snare (P=0.2899)
  Margin:        +0.4201
  Entropy:       0.3360
  Distribution:  tangled_rope: 0.710, snare: 0.290

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.7047)
  Entropy:       0.3387
  Distribution:  tangled_rope: 0.705, snare: 0.295

  Classical/Indexed TV Distance: 0.0053 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (5):
    maxent: cohomological_fracture_divergence
      H1 > 0 confirms perspectival fracture; MaxEnt ambiguity is structural
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    signature: fcr_deferred_signature_mismatch
      FCR override target mismatch; gate deferred due to perspectival variance
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **6 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      141 mountain | 18 rope | 587 tangled_rope | 185 snare | 9 piton | 10 scaffold
    Purity:    150 pristine | 41 sound | 94 borderline | 647 contaminated | 18 degraded
    Coupling:  731 strongly | 21 weakly | 171 independent | 1 inconclusive
    Signature: 556 false_natural_law | 248 false_ci_rope | 136 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 703 deep | 57 moderate | 190 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.1% of corpus shares this signature)
    Purity band: contaminated (68.0% of corpus in this band)
    Confidence band: moderate (6.0% of corpus in this band)
    Boundary zone: tangled_rope->snare (159 constraints share this boundary)
    Orbit Family ID:  F535

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.8 (high variance)
  Index Configs:       5
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 15
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for unilateral_condition_control

[STRUCTURAL SIGNATURE ANALYSIS]
  unilateral_condition_control: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for unilateral_condition_control: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([snare,tangled_rope]),excess_above_floor(0.42999999999999994),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: metric_setting_authority
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  unilateral_condition_control (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.65 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [condition_manipulation_intentionality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether condition deterioration is manipulated or structural
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

  ┌─ [performance_metric_validity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether performance metrics measure real outcomes or proxy goals
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

  ┌─ [exit_option_asymmetry] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Distribution of genuine exit capacity among metric subjects
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

  ┌─ [collective_action_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Critical mass threshold for collective action among subjects
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

  ┌─ [omega_extraction_blindness_unilateral_condition_control] CONCEPTUAL CLARIFICATION
  │  Constraint: unilateral_condition_control
  │  Gap: Constraint unilateral_condition_control appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from unilateral_condition_control?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does unilateral_condition_control serve?
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
CONSTRAINT REPORT: legitimation_through_objectivity
============================================================
CORPUS CONTEXT: 951 constraints
  Types: 141 mountain, 18 rope, 587 tangled_rope, 185 snare, 9 piton, 10 scaffold
  Network stability: cascading | 792 omegas (734 critical)
  Confidence: 703 deep (74%) | 57 moderate (6%) | 190 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/legitimation_through_objectivity.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: legitimation_through_objectivity...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: legitimation_through_objectivity

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: legitimation_through_objectivity (0-30)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] legitimation_through_objectivity from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] legitimation_through_objectivity from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] legitimation_through_objectivity from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legitimation_through_objectivity from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX OK] legitimation_through_objectivity from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] legitimation_through_objectivity from context(agent_power(moderate),time_horizon(generational),exit_options(constrained),spatial_scope(continental)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] legitimation_through_objectivity from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 3 | Warning: 1 | Watch: 0

  legitimation_through_objectivity:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,30,0.55,0.78)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,30,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.53)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  legitimation_through_objectivity -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  legitimation_through_objectivity (ε=0.68):
    powerless→organized@local: d=0.650 f(d)=1.01 χ = 0.68 × 1.01 × 0.80 = 0.549
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: legitimation_through_objectivity ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (drift)          ║
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
    Effective purity:   0.3125
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | compounding_obligation_trap | snare | shared_victim | 0.30 | 0.3526 |

  No significant contamination — purity unchanged across 1 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_legitimation_through_objectivity
    Severity Score:    0.556
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F352

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

  Agreements (8 subsystems):
    maxent, cohomology, abductive, signature, purity, fingerprint_voids, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,30,0.55,0.78),critical),drift(extraction_accumulation,evidence(extraction_delta,0,30,0.48,0.68),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.53)]),warning)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      141 mountain | 18 rope | 587 tangled_rope | 185 snare | 9 piton | 10 scaffold
    Purity:    150 pristine | 41 sound | 94 borderline | 647 contaminated | 18 degraded
    Coupling:  731 strongly | 21 weakly | 171 independent | 1 inconclusive
    Signature: 556 false_natural_law | 248 false_ci_rope | 136 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 703 deep | 57 moderate | 190 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (58.5% of corpus shares this signature)
    Purity band: contaminated (68.0% of corpus in this band)
    Confidence band: borderline (20.0% of corpus in this band)
    Boundary zone: snare->tangled_rope (182 constraints share this boundary)
    Orbit Family ID:  F352

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5714285714285714 (high variance)
  Index Configs:       7
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  piton -> snare, piton -> tangled_rope, rope -> piton, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 30
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  legitimation_through_objectivity: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for legitimation_through_objectivity: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=1.000 with 6 coupled dimension pairs. Excess extraction=0.53. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.55

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  legitimation_through_objectivity (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.58 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [measurement_precision_necessity] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether numerical precision is inherent to coordination or serves specific interests
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

  ┌─ [parameter_setting_visibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether transparency in parameter-setting enables political contestation
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

  ┌─ [technocratic_capture_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether democratic institutions can reclaim measurement discretion from technocrats
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

  ┌─ [creditor_measurement_asymmetry] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Why measurement precision is asymmetric between creditor and debtor claims
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

  ┌─ [omega_extraction_blindness_legitimation_through_objectivity] CONCEPTUAL CLARIFICATION
  │  Constraint: legitimation_through_objectivity
  │  Gap: Constraint legitimation_through_objectivity appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from legitimation_through_objectivity?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does legitimation_through_objectivity serve?
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
CONSTRAINT REPORT: compounding_obligation_trap
============================================================
CORPUS CONTEXT: 951 constraints
  Types: 141 mountain, 18 rope, 587 tangled_rope, 185 snare, 9 piton, 10 scaffold
  Network stability: cascading | 792 omegas (734 critical)
  Confidence: 703 deep (74%) | 57 moderate (6%) | 190 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/compounding_obligation_trap.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: compounding_obligation_trap...
  [BRIDGE] Derived has_sunset_clause(compounding_obligation_trap) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: compounding_obligation_trap

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: compounding_obligation_trap (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] compounding_obligation_trap from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX OK] compounding_obligation_trap from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] compounding_obligation_trap from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=snare
  [INDEX MISMATCH] compounding_obligation_trap from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] compounding_obligation_trap from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=scaffold
  [INDEX OK] compounding_obligation_trap from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] compounding_obligation_trap from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] compounding_obligation_trap from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  compounding_obligation_trap:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.42,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.53)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  compounding_obligation_trap -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  compounding_obligation_trap (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: compounding_obligation_trap ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9954 (snare_leaning)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3526
    Propagation delta:  -0.0016

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | legitimation_through_objectivity | tangled_rope | shared_victim | 0.30 | 0.3125 |

  Purity degraded from 0.3542 to 0.3526 by contamination from 1 neighbor(s), primarily legitimation_through_objectivity (shared_victim, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_compounding_obligation_trap
    Severity Score:    0.734
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F181

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    Confidence:       0.7814 (moderate)
    Rival Type:       snare (P=0.2186)
    Margin:           +0.5628
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7814 (moderate)
  Rival Type:    snare (P=0.2186)
  Margin:        +0.5628
  Entropy:       0.2931
  Distribution:  tangled_rope: 0.781, snare: 0.219, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.7832)
  Entropy:       0.2918
  Distribution:  tangled_rope: 0.783, snare: 0.217, piton: 0.000

  Classical/Indexed TV Distance: 0.0018 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (5):
    maxent: cohomological_fracture_divergence
      H1 > 0 confirms perspectival fracture; MaxEnt ambiguity is structural
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    signature: fcr_deferred_signature_mismatch
      FCR override target mismatch; gate deferred due to perspectival variance
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **6 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      141 mountain | 18 rope | 587 tangled_rope | 185 snare | 9 piton | 10 scaffold
    Purity:    150 pristine | 41 sound | 94 borderline | 647 contaminated | 18 degraded
    Coupling:  731 strongly | 21 weakly | 171 independent | 1 inconclusive
    Signature: 556 false_natural_law | 248 false_ci_rope | 136 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 703 deep | 57 moderate | 190 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.1% of corpus shares this signature)
    Purity band: contaminated (68.0% of corpus in this band)
    Confidence band: moderate (6.0% of corpus in this band)
    Boundary zone: tangled_rope->snare (159 constraints share this boundary)
    Orbit Family ID:  F181

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       8
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for compounding_obligation_trap

[STRUCTURAL SIGNATURE ANALYSIS]
  compounding_obligation_trap: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for compounding_obligation_trap: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([snare,tangled_rope]),excess_above_floor(0.53),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: obligation_administrator
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.55

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  compounding_obligation_trap (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.77 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [payment_capacity_measurement] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Definition of maximum sustainable payment capacity
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

  ┌─ [origination_intent_vs_outcome] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether trap results from predatory design or exogenous shocks
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

  ┌─ [alternative_suppression] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether alternative credit structures are structurally suppressed or legally suppressed
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

  ┌─ [bankruptcy_exit_effectiveness] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether bankruptcy provides genuine exit or continued extraction
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

  ┌─ [omega_extraction_blindness_compounding_obligation_trap] CONCEPTUAL CLARIFICATION
  │  Constraint: compounding_obligation_trap
  │  Gap: Constraint compounding_obligation_trap appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from compounding_obligation_trap?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does compounding_obligation_trap serve?
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

