
============================================================
CONSTRAINT REPORT: measurement_apparatus_bidirectionality
============================================================
CORPUS CONTEXT: 970 constraints
  Types: 145 mountain, 24 rope, 593 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 803 omegas (741 critical)
  Confidence: 723 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/measurement_apparatus_bidirectionality.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: measurement_apparatus_bidirectionality...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: measurement_apparatus_bidirectionality

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: measurement_apparatus_bidirectionality (0-100)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(powerful),time_horizon(immediate),exit_options(arbitrage),spatial_scope(continental)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  measurement_apparatus_bidirectionality:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  measurement_apparatus_bidirectionality (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: measurement_apparatus_bidirectionality ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
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
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9760
    Effective purity:   0.9760
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

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

    Type:      145 mountain | 24 rope | 593 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    160 pristine | 42 sound | 99 borderline | 650 contaminated | 18 degraded
    Coupling:  739 strongly | 21 weakly | 182 independent | 1 inconclusive
    Signature: 557 false_natural_law | 262 false_ci_rope | 140 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 723 deep | 53 moderate | 193 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.4% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: mountain->rope (145 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.16666666666666666 (low variance)
  Index Configs:       6
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 100
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for measurement_apparatus_bidirectionality

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  measurement_apparatus_bidirectionality: natural_law (confidence: low)
    → NATURAL LAW signature for measurement_apparatus_bidirectionality: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.02, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.39

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [contact_duration_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Duration threshold for structurally significant bidirectionality
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

  ┌─ [substrate_coupling_mechanism] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether bidirectionality is universal or technology-contingent
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

  ┌─ [observer_awareness_necessity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether observer awareness is necessary for bidirectionality
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


============================================================
CONSTRAINT REPORT: categorical_instrument_blindness
============================================================
CORPUS CONTEXT: 970 constraints
  Types: 145 mountain, 24 rope, 593 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 803 omegas (741 critical)
  Confidence: 723 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/categorical_instrument_blindness.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: categorical_instrument_blindness...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: categorical_instrument_blindness

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: categorical_instrument_blindness (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(institutional),time_horizon(biographical),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(powerful),time_horizon(biographical),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(analytical),time_horizon(generational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  categorical_instrument_blindness:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.15,0.18)
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  categorical_instrument_blindness -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  categorical_instrument_blindness (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: categorical_instrument_blindness ===
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
    Drift events:     3 — extraction_accumulation, purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9360
    Effective purity:   0.8556
    Propagation delta:  -0.0804

    Network neighbors (2):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | documentation_fidelity_collapse | snare | shared_beneficiary | 0.30 | 0.3125 |
    | protocol_rigidity_under_unclassified_variance | tangled_rope | shared_beneficiary | 0.30 | 0.8432 |

  Purity degraded from 0.9360 to 0.8556 by contamination from 2 neighbor(s), primarily documentation_fidelity_collapse (shared_beneficiary, purity 0.3125).

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
    Confidence:       0.9609 (deep)
    Rival Type:       scaffold (P=0.0391)
    Margin:           +0.9218
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9609 (deep)
  Rival Type:    scaffold (P=0.0391)
  Margin:        +0.9218
  Entropy:       0.0921
  Distribution:  rope: 0.961, scaffold: 0.039, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9825)
  Entropy:       0.0492
  Distribution:  rope: 0.982, scaffold: 0.018, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0216 (moderate)
  Moderate divergence — observer-dependence shifts probabilistic weights without changing the top classification.

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

    Type:      145 mountain | 24 rope | 593 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    160 pristine | 42 sound | 99 borderline | 650 contaminated | 18 degraded
    Coupling:  739 strongly | 21 weakly | 182 independent | 1 inconclusive
    Signature: 557 false_natural_law | 262 false_ci_rope | 140 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 723 deep | 53 moderate | 193 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.0% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: rope->scaffold (13 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.3333333333333333 (low variance)
  Index Configs:       6
  Types Produced:      2

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  categorical_instrument_blindness: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for categorical_instrument_blindness: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.16)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: administrative_systems
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.45

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [substrate_recovery_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where substrate information loss becomes epistemic damage
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

  ┌─ [schema_lock_in_dynamics] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether categorical schemas create path-dependent lock-in
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

  ┌─ [biometric_accumulation_externality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether substrate blindness creates uncompensated health externalities
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


============================================================
CONSTRAINT REPORT: witness_obligation_without_recipient
============================================================
CORPUS CONTEXT: 970 constraints
  Types: 145 mountain, 24 rope, 593 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 803 omegas (741 critical)
  Confidence: 723 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/witness_obligation_without_recipient.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: witness_obligation_without_recipient...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: witness_obligation_without_recipient

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: witness_obligation_without_recipient (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] witness_obligation_without_recipient from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] witness_obligation_without_recipient from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] witness_obligation_without_recipient from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX OK] witness_obligation_without_recipient from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=rope
  [INDEX MISMATCH] witness_obligation_without_recipient from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=piton, computed=naturalized
  [INDEX OK] witness_obligation_without_recipient from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  witness_obligation_without_recipient:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.45,0.68)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.5063333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.38)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  witness_obligation_without_recipient -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  witness_obligation_without_recipient (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: witness_obligation_without_recipient ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.506 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.506333 (borderline)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9713 (snare_leaning)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5063
    Effective purity:   0.0000
    Propagation delta:  -0.5063

    Network neighbors (21):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | adversarial_truth_decay | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | attention_as_bottleneck_resource | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | awareness_without_leverage | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | belief_argument_conclusion | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | bureaucratic_legibility_collapse | tangled_rope | shared_beneficiary | 0.30 | 0.3304 |
    | cognitive_induction_gap | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | confirmation_bias | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | consensus_without_truth | snare | shared_victim | 0.30 | 0.0718 |
    | coordination_extraction_invisibility | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | data_laundering_pipeline | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | epistemic_free_rider_problem | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | epistemic_overload_collapse | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | error_induced_stability | snare | shared_victim | 0.30 | 0.0718 |
    | information_foraging_theory | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | memetic_fitness_vs_truth | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | meritocratic_ideology_as_error_propagation | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | model_collapse_feedback_loop | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | narrative_overfitting | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | reputational_cascade_failure | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | theatrical_neutrality | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | utopia_apocalypse_fragility | tangled_rope | shared_victim | 0.30 | 0.0000 |

  Purity degraded from 0.5063 to 0.0000 by contamination from 21 neighbor(s), primarily adversarial_truth_decay (shared_victim, purity 0.0000).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_witness_obligation_without_recipient
    Severity Score:    0.546
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F568

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       0.9660 (deep)
    Rival Type:       snare (P=0.0340)
    Margin:           +0.9320
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9660 (deep)
  Rival Type:    snare (P=0.0340)
  Margin:        +0.9320
  Entropy:       0.0830
  Distribution:  tangled_rope: 0.966, snare: 0.034, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9653)
  Entropy:       0.0844
  Distribution:  tangled_rope: 0.965, snare: 0.035, piton: 0.000

  Classical/Indexed TV Distance: 0.0006 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,10,0.45,0.68),warning),drift(extraction_accumulation,evidence(extraction_delta,0,10,0.32,0.48),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.5063333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.38)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(adversarial_truth_decay,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.66)]),contagion(attention_as_bottleneck_resource,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(awareness_without_leverage,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5)]),contagion(belief_argument_conclusion,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(bureaucratic_legibility_collapse,0.011412500000000004,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(cognitive_induction_gap,0.011725000000000008,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5)]),contagion(confirmation_bias,0.011725000000000008,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(consensus_without_truth,0.02907500000000001,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(coordination_extraction_invisibility,0.011412500000000004,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)]),contagion(data_laundering_pipeline,0.009625000000000005,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.42999999999999994)]),contagion(epistemic_free_rider_problem,0.011725000000000008,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(epistemic_overload_collapse,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(error_induced_stability,0.02907500000000001,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.66)]),contagion(information_foraging_theory,0.0072125000000000045,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.36)]),contagion(memetic_fitness_vs_truth,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(model_collapse_feedback_loop,0.010637500000000006,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.37)]),contagion(reputational_cascade_failure,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(theatrical_neutrality,0.014537500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5599999999999999)]),contagion(utopia_apocalypse_fragility,0.013937500000000005,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.28724333333333335,intrinsic_purity,0.5063333333333334),critical)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      145 mountain | 24 rope | 593 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    160 pristine | 42 sound | 99 borderline | 650 contaminated | 18 degraded
    Coupling:  739 strongly | 21 weakly | 182 independent | 1 inconclusive
    Signature: 557 false_natural_law | 262 false_ci_rope | 140 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 723 deep | 53 moderate | 193 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.0% of corpus shares this signature)
    Purity band: borderline (10.2% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: tangled_rope->snare (164 constraints share this boundary)
    Orbit Family ID:  F568

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
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for witness_obligation_without_recipient

[STRUCTURAL SIGNATURE ANALYSIS]
  witness_obligation_without_recipient: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for witness_obligation_without_recipient: Appears to be rope (indexed_rope_classification) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),excess_above_floor(0.38),nonsensical_coupling(0.16666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: administrative_apparatus
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.52

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  witness_obligation_without_recipient (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [recipient_capability_threshold] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Definition of recipient capability for witness accounts
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

  ┌─ [witness_identity_fusion_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Mechanism of witness identity lock
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

  ┌─ [archive_latency_value] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether archive has delayed epistemic value
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

  ┌─ [transmission_to_cores_alternative] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether direct transmission to cores resolves witness obligation
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

  ┌─ [omega_extraction_blindness_witness_obligation_without_recipient] CONCEPTUAL CLARIFICATION
  │  Constraint: witness_obligation_without_recipient
  │  Gap: Constraint witness_obligation_without_recipient appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from witness_obligation_without_recipient?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does witness_obligation_without_recipient serve?
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

