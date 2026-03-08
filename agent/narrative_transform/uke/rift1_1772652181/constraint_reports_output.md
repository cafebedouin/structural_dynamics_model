
============================================================
CONSTRAINT REPORT: transformation_as_threshold_marker
============================================================
CORPUS CONTEXT: 956 constraints
  Types: 142 mountain, 19 rope, 588 tangled_rope, 187 snare, 9 piton, 10 scaffold
  Network stability: cascading | 795 omegas (737 critical)
  Confidence: 710 deep (74%) | 53 moderate (6%) | 192 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/transformation_as_threshold_marker.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: transformation_as_threshold_marker...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: transformation_as_threshold_marker

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: transformation_as_threshold_marker (0-15)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] transformation_as_threshold_marker from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=snare
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=naturalized
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(powerful),time_horizon(immediate),exit_options(mobile),spatial_scope(local)): declared=rope, computed=naturalized
  [INDEX OK] transformation_as_threshold_marker from context(agent_power(powerless),time_horizon(generational),exit_options(trapped),spatial_scope(regional)): declared=snare, computed=snare
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(institutional),time_horizon(biographical),exit_options(identity_locked),spatial_scope(national)): declared=snare, computed=naturalized
  [INDEX MISMATCH] transformation_as_threshold_marker from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  transformation_as_threshold_marker:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,15,0.52,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.625,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4666666666666667,decline_signals,[extraction_rising,coupling_above_threshold(0.625),theater_rising,excess_above_floor(0.53)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  transformation_as_threshold_marker -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  transformation_as_threshold_marker (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.400 f(d)=0.40 χ = 0.68 × 0.40 × 1.00 = 0.274
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: transformation_as_threshold_marker ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=naturalized  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.625, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(0.625,0.3))
  Purity:     0.467 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_ci_rope
    Purity:           0.466667 (contaminated)
    Coupling:         strongly_coupled (score: 0.625)
    Boltzmann:        non_compliant
    Drift events:     3 — extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4667
    Effective purity:   0.4667
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_transformation_as_threshold_marker
    Severity Score:    0.586
    Gap Class:         analytical_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F510

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), naturalized (institutional)
    Confidence:       0.1181 (borderline)
    Rival Type:       tangled_rope (P=0.8819)
    Margin:           -0.7637
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees naturalized while powerless, moderate, analytical see snare.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says snare, MaxEnt says tangled_rope
  Confidence:    0.1181 (borderline)
  Rival Type:    tangled_rope (P=0.8819)
  Margin:        -0.7637
  Entropy:       0.2027
  Distribution:  tangled_rope: 0.882, snare: 0.118

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.8829)
  Entropy:       0.2015
  Distribution:  tangled_rope: 0.883, snare: 0.117

  Classical/Indexed TV Distance: 0.0010 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.84 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
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
    abductive: abductive_tension([trigger(convergent_structural_stress,0.84,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      142 mountain | 19 rope | 588 tangled_rope | 187 snare | 9 piton | 10 scaffold
    Purity:    152 pristine | 41 sound | 96 borderline | 648 contaminated | 18 degraded
    Coupling:  734 strongly | 21 weakly | 173 independent | 1 inconclusive
    Signature: 556 false_natural_law | 252 false_ci_rope | 137 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 710 deep | 53 moderate | 192 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.4% of corpus shares this signature)
    Purity band: contaminated (67.8% of corpus in this band)
    Confidence band: borderline (20.1% of corpus in this band)
    Boundary zone: snare->tangled_rope (184 constraints share this boundary)
    Orbit Family ID:  F510

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.375 (low variance)
  Index Configs:       8
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 15
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for transformation_as_threshold_marker

[STRUCTURAL SIGNATURE ANALYSIS]
  transformation_as_threshold_marker: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for transformation_as_threshold_marker: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(0.625,0.3),scope_variant([snare,tangled_rope]),excess_above_floor(0.53),nonsensical_coupling(0.3333333333333333)]. Coupling score=0.625. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: quota_system_administrators
    → Institutional d=0.400

Aggregate Magnitude (Kappa) at Tn: 0.57

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  transformation_as_threshold_marker (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.47 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 6 resolution scenario(s):

  ┌─ [threshold_optimality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether disposal threshold optimizes safety or throughput
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

  ┌─ [transformation_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether transformation is physically irreversible or epistemically suppressed
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

  ┌─ [productivity_necessity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether productivity gain is real or measurement artifact
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

  ┌─ [alternative_tracking_mechanisms] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether non-bodily productivity tracking is feasible
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

  ┌─ [disposal_compensation_adequacy] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether disposal compensation is actuarially adequate
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

  ┌─ [omega_extraction_blindness_transformation_as_threshold_marker] CONCEPTUAL CLARIFICATION
  │  Constraint: transformation_as_threshold_marker
  │  Gap: Constraint transformation_as_threshold_marker appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from transformation_as_threshold_marker?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does transformation_as_threshold_marker serve?
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
CONSTRAINT REPORT: exclusion_as_preparation
============================================================
CORPUS CONTEXT: 956 constraints
  Types: 142 mountain, 19 rope, 588 tangled_rope, 187 snare, 9 piton, 10 scaffold
  Network stability: cascading | 795 omegas (737 critical)
  Confidence: 710 deep (74%) | 53 moderate (6%) | 192 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/exclusion_as_preparation.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: exclusion_as_preparation...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: exclusion_as_preparation

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: exclusion_as_preparation (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] exclusion_as_preparation from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] exclusion_as_preparation from context(agent_power(organized),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] exclusion_as_preparation from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] exclusion_as_preparation from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] exclusion_as_preparation from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  exclusion_as_preparation:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.15,0.18)
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  exclusion_as_preparation -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  exclusion_as_preparation (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: exclusion_as_preparation ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.936 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.936 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — extraction_accumulation, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9360
    Effective purity:   0.9360
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.7326 (moderate)
    Rival Type:       scaffold (P=0.2674)
    Margin:           +0.4651
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7326 (moderate)
  Rival Type:    scaffold (P=0.2674)
  Margin:        +0.4651
  Entropy:       0.3241
  Distribution:  rope: 0.733, scaffold: 0.267

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.8621)
  Entropy:       0.2238
  Distribution:  rope: 0.862, scaffold: 0.138

  Classical/Indexed TV Distance: 0.1296 (large)
  Significant divergence — observer-dependence changes the probabilistic landscape. Classical Oracle Gap (Theorem 4): single-position analysis misses this structure.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    cohomology, signature, boltzmann, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      142 mountain | 19 rope | 588 tangled_rope | 187 snare | 9 piton | 10 scaffold
    Purity:    152 pristine | 41 sound | 96 borderline | 648 contaminated | 18 degraded
    Coupling:  734 strongly | 21 weakly | 173 independent | 1 inconclusive
    Signature: 556 false_natural_law | 252 false_ci_rope | 137 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 710 deep | 53 moderate | 192 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.4% of corpus shares this signature)
    Purity band: pristine (15.9% of corpus in this band)
    Confidence band: moderate (5.5% of corpus in this band)
    Boundary zone: rope->scaffold (8 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.2 (low variance)
  Index Configs:       5
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  exclusion_as_preparation: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for exclusion_as_preparation: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.16)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: colony_collective
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.44

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [signal_causality_direction] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Causal direction between social exclusion and physiological transformation
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

  ┌─ [alternative_signal_sufficiency] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether alternative signals could replace the exclusion ritual
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

  ┌─ [disposal_timeline_variance] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Variance in exclusion-to-disposal interval as coordination reliability measure
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

====================================================

