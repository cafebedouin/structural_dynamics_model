
============================================================
CONSTRAINT REPORT: substrate_as_unrecognized_archive
============================================================
CORPUS CONTEXT: 967 constraints
  Types: 144 mountain, 23 rope, 592 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 802 omegas (740 critical)
  Confidence: 720 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/substrate_as_unrecognized_archive.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: substrate_as_unrecognized_archive...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: substrate_as_unrecognized_archive

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: substrate_as_unrecognized_archive (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] substrate_as_unrecognized_archive from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] substrate_as_unrecognized_archive from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] substrate_as_unrecognized_archive from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] substrate_as_unrecognized_archive from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] substrate_as_unrecognized_archive from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  substrate_as_unrecognized_archive:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  substrate_as_unrecognized_archive (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: substrate_as_unrecognized_archive ===
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

    Type:      144 mountain | 23 rope | 592 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    158 pristine | 42 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 180 independent | 1 inconclusive
    Signature: 557 false_natural_law | 260 false_ci_rope | 139 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.4% of corpus shares this signature)
    Purity band: pristine (16.3% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: mountain->rope (144 constraints share this boundary)

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
  ! ALERT [severe]: type_1_false_summit detected for substrate_as_unrecognized_archive

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  substrate_as_unrecognized_archive: natural_law (confidence: low)
    → NATURAL LAW signature for substrate_as_unrecognized_archive: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.02, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: legibility_asymmetry_as_survival
============================================================
CORPUS CONTEXT: 967 constraints
  Types: 144 mountain, 23 rope, 592 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 802 omegas (740 critical)
  Confidence: 720 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/legibility_asymmetry_as_survival.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: legibility_asymmetry_as_survival...
  [BRIDGE] Derived has_sunset_clause(legibility_asymmetry_as_survival) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: legibility_asymmetry_as_survival

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: legibility_asymmetry_as_survival (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(powerless),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(moderate),time_horizon(generational),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(organized),time_horizon(civilizational),exit_options(constrained),spatial_scope(global)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 1 | Watch: 1

  legibility_asymmetry_as_survival:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.12,0.18)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.9600000000000001,decline_signals,[extraction_rising,theater_rising,excess_above_floor(0.09999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  legibility_asymmetry_as_survival -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  legibility_asymmetry_as_survival (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: legibility_asymmetry_as_survival ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=negligible  suppression=moderate
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.960 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.96 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — extraction_accumulation, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9600
    Effective purity:   0.9600
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_perspectival_legibility_asymmetry_as_survival
    Severity Score:    0.186
    Gap Class:         consensus
    Gap Pattern:       general_type_mismatch
    Family ID:         F356

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.8742 (deep)
    Rival Type:       scaffold (P=0.1258)
    Margin:           +0.7484
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.8742 (deep)
  Rival Type:    scaffold (P=0.1258)
  Margin:        +0.7484
  Entropy:       0.2112
  Distribution:  rope: 0.874, scaffold: 0.126, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9407)
  Entropy:       0.1256
  Distribution:  rope: 0.941, scaffold: 0.059, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0665 (moderate)
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

    Type:      144 mountain | 23 rope | 592 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    158 pristine | 42 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 180 independent | 1 inconclusive
    Signature: 557 false_natural_law | 260 false_ci_rope | 139 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.9% of corpus shares this signature)
    Purity band: pristine (16.3% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: rope->scaffold (12 constraints share this boundary)
    Orbit Family ID:  F356

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       6
  Types Produced:      3

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
  legibility_asymmetry_as_survival: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for legibility_asymmetry_as_survival: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.09999999999999999)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: monitored_communities
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.46

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  legibility_asymmetry_as_survival (rope vs mountain):
    ! MANDATROPHY GAP: delta_chi = 0.21 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [substrate_decoding_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Timeline for surveillance apparatus to decode substrate complexity
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

  ┌─ [compliance_theater_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where compliance performance becomes net burden
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

  ┌─ [intergenerational_transmission_fidelity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether substrate encoding preserves or degrades practice fidelity
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

  ┌─ [omega_perspectival_legibility_asymmetry_as_survival] CONCEPTUAL CLARIFICATION
  │  Constraint: legibility_asymmetry_as_survival
  │  Gap: Constraint legibility_asymmetry_as_survival appears as rope to individuals but mountain to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive legibility_asymmetry_as_survival
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

====================================================


============================================================
CONSTRAINT REPORT: distributed_memory_as_counter_disposal
============================================================
CORPUS CONTEXT: 967 constraints
  Types: 144 mountain, 23 rope, 592 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 802 omegas (740 critical)
  Confidence: 720 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/distributed_memory_as_counter_disposal.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: distributed_memory_as_counter_disposal...
  [BRIDGE] Derived has_sunset_clause(distributed_memory_as_counter_disposal) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: distributed_memory_as_counter_disposal

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: distributed_memory_as_counter_disposal (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] distributed_memory_as_counter_disposal from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX OK] distributed_memory_as_counter_disposal from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] distributed_memory_as_counter_disposal from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=scaffold
  [INDEX OK] distributed_memory_as_counter_disposal from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] distributed_memory_as_counter_disposal from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] distributed_memory_as_counter_disposal from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  distributed_memory_as_counter_disposal:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.35,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3941666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  distributed_memory_as_counter_disposal -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  distributed_memory_as_counter_disposal (ε=0.48):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.48 × 0.65 × 0.80 = 0.250
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.550 f(d)=0.78 χ = 0.48 × 0.78 × 1.00 = 0.373
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: distributed_memory_as_counter_disposal ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.394 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.723333 (sound)
    Coupling:         independent (score: 0.25)
    Boltzmann:        compliant
    Drift events:     2 — extraction_accumulation, purity_drift
    Tangled psi:      0.9528 (snare_leaning)
    Coalition:        other

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.7233
    Effective purity:   0.7233
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_perspectival_distributed_memory_as_counter_disposal
    Severity Score:    0.426
    Gap Class:         consensus
    Gap Pattern:       general_type_mismatch
    Family ID:         F220

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate)
    Confidence:       0.9795 (deep)
    Rival Type:       snare (P=0.0205)
    Margin:           +0.9590
    Boundary:         tangled_rope->snare
    H^1 band:         4 — Hub 2 (effective immutability) drives a 2+2 split: powerless, institutional see naturalized; moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9795 (deep)
  Rival Type:    snare (P=0.0205)
  Margin:        +0.9590
  Entropy:       0.0559
  Distribution:  tangled_rope: 0.979, snare: 0.020, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9792)
  Entropy:       0.0566
  Distribution:  tangled_rope: 0.979, snare: 0.021

  Classical/Indexed TV Distance: 0.0004 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | hub_conflict | 0.83 | hub_conflict_band | genuine | Hub 1 and Hub 2 produce conflicting classification signals at this constraint. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.75 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    maxent, signature, boltzmann, purity, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (3):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(hub_conflict,0.83,hub_conflict_band,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.75,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  T6 (Hub Correspondence — Hub 2): H^1 = 4 maps to Hub 2 (effective immutability). Two pairs of observers disagree, producing a 2+2 classification split driven by the immutability axis.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      144 mountain | 23 rope | 592 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    158 pristine | 42 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 180 independent | 1 inconclusive
    Signature: 557 false_natural_law | 260 false_ci_rope | 139 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.9% of corpus shares this signature)
    Purity band: sound (4.3% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: tangled_rope->snare (163 constraints share this boundary)
    Orbit Family ID:  F220

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
  ! ALERT [informational]: perspectival_incoherence detected for distributed_memory_as_counter_disposal

[STRUCTURAL SIGNATURE ANALYSIS]
  distributed_memory_as_counter_disposal: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for distributed_memory_as_counter_disposal: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.39999999999999997),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: warren_collective
    → Institutional d=0.550

Aggregate Magnitude (Kappa) at Tn: 0.52

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  distributed_memory_as_counter_disposal (snare vs tangled_rope):
    ! MANDATROPHY GAP: delta_chi = 0.12 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [substrate_persistence_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Critical mass threshold for distributed memory persistence
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

  ┌─ [institutional_adaptation_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Institutional learning rate and strategic response to distributed memory
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

  ┌─ [legibility_extraction_tradeoff] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether observable coordination creates exploitable vulnerability
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

  ┌─ [substrate_signal_fidelity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Signal fidelity across distributed transmission cycles
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

  ┌─ [omega_perspectival_distributed_memory_as_counter_disposal] CONCEPTUAL CLARIFICATION
  │  Constraint: distributed_memory_as_counter_disposal
  │  Gap: Constraint distributed_memory_as_counter_disposal appears as snare to individuals but tangled_rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive distributed_memory_as_counter_disposal
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

====================================================

