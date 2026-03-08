
============================================================
CONSTRAINT REPORT: unsanctioned_substrate_access
============================================================
CORPUS CONTEXT: 964 constraints
  Types: 143 mountain, 22 rope, 591 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 800 omegas (740 critical)
  Confidence: 717 deep (74%) | 53 moderate (6%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/unsanctioned_substrate_access.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: unsanctioned_substrate_access...
  [BRIDGE] Derived has_sunset_clause(unsanctioned_substrate_access) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: unsanctioned_substrate_access

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: unsanctioned_substrate_access (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] unsanctioned_substrate_access from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] unsanctioned_substrate_access from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] unsanctioned_substrate_access from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] unsanctioned_substrate_access from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] unsanctioned_substrate_access from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] unsanctioned_substrate_access from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] unsanctioned_substrate_access from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 0 | Warning: 4 | Watch: 0

  unsanctioned_substrate_access:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,6,0.35,0.58)
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.28,0.42)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,0.875,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.39,decline_signals,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  unsanctioned_substrate_access -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  unsanctioned_substrate_access (ε=0.42):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.42 × 1.36 × 0.80 = 0.456
    moderate@national: d=0.700 f(d)=1.11 χ = 0.42 × 1.11 × 1.00 = 0.465
    institutional@national: d=0.780 f(d)=1.23 χ = 0.42 × 1.23 × 1.00 = 0.518
    analytical@global: d=0.720 f(d)=1.14 χ = 0.42 × 1.14 × 1.20 = 0.575

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: unsanctioned_substrate_access ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=extreme
  Coupling:   strongly_coupled (score=0.875, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,institutional,local-national,1.0),coupled(power_scope,institutional,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(0.875,0.25))
  Purity:     0.390 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_natural_law
    Purity:           0.431667 (contaminated)
    Coupling:         strongly_coupled (score: 0.875)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.4762 (genuinely_tangled)
    Coalition:        uniform_tangled

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4317
    Effective purity:   0.4317
    Propagation delta:  +0.0000

    Network neighbors (3):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | embodied_knowledge_transmission | unknown | explicit | 1.00 | N/A |
    | sanctioned_protocol_compliance | unknown | explicit | 1.00 | N/A |
    | surveillance_infrastructure_investment | unknown | explicit | 1.00 | N/A |

  No significant contamination — purity unchanged across 3 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_perspectival_unsanctioned_substrate_access
    Severity Score:    0.414
    Gap Class:         consensus
    Gap Pattern:       general_type_mismatch
    Family ID:         F052

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.9500 (deep)
    Rival Type:       mountain (P=0.0100)
    Margin:           +0.9400
    Boundary:         tangled_rope->mountain
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9500 (deep)
  Rival Type:    mountain (P=0.0100)
  Margin:        +0.9400
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

  Agreements (10 subsystems):
    maxent, cohomology, abductive, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts (2):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type

  Convergent Rejections: none

  Tensions: none

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      143 mountain | 22 rope | 591 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    156 pristine | 41 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 177 independent | 1 inconclusive
    Signature: 557 false_natural_law | 258 false_ci_rope | 138 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 717 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (57.8% of corpus shares this signature)
    Purity band: contaminated (67.4% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: tangled_rope->mountain (412 constraints share this boundary)
    Orbit Family ID:  F052

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 8
    Unique Type Shifts:  rope -> tangled_rope, rope -> unknown, unknown -> tangled_rope

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  unsanctioned_substrate_access: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for unsanctioned_substrate_access: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=0.875 with 6 coupled dimension pairs. Excess extraction=0.39999999999999997. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [detection_threshold_stability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Stability of detection threshold gap between substrate and monitoring
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

  ┌─ [embodied_knowledge_transferability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether substrate access requires embodied practice or can be codified
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

  ┌─ [coordination_vs_evasion_ratio] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Ratio of coordination function to evasion function in substrate usage
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
  │  Gap: Institutional adaptation speed to substrate access discovery
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

  ┌─ [omega_perspectival_unsanctioned_substrate_access] CONCEPTUAL CLARIFICATION
  │  Constraint: unsanctioned_substrate_access
  │  Gap: Constraint unsanctioned_substrate_access appears as snare to individuals but tangled_rope to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive unsanctioned_substrate_access
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
CONSTRAINT REPORT: enforcement_gap_exploitation
============================================================
CORPUS CONTEXT: 964 constraints
  Types: 143 mountain, 22 rope, 591 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 800 omegas (740 critical)
  Confidence: 717 deep (74%) | 53 moderate (6%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/enforcement_gap_exploitation.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: enforcement_gap_exploitation...
  [BRIDGE] Derived has_sunset_clause(enforcement_gap_exploitation) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: enforcement_gap_exploitation

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: enforcement_gap_exploitation (0-5)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] enforcement_gap_exploitation from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] enforcement_gap_exploitation from context(agent_power(organized),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] enforcement_gap_exploitation from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=scaffold, computed=tangled_rope
  [INDEX OK] enforcement_gap_exploitation from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] enforcement_gap_exploitation from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  enforcement_gap_exploitation:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  enforcement_gap_exploitation (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.350 f(d)=0.29 χ = 0.18 × 0.29 × 1.00 = 0.052
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: enforcement_gap_exploitation ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=moderate
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
    Drift events:     1 — purity_drift

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

  Omega: omega_perspectival_enforcement_gap_exploitation
    Severity Score:    0.195
    Gap Class:         consensus
    Gap Pattern:       general_type_mismatch
    Family ID:         F052

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.8661 (deep)
    Rival Type:       scaffold (P=0.1339)
    Margin:           +0.7323
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.8661 (deep)
  Rival Type:    scaffold (P=0.1339)
  Margin:        +0.7323
  Entropy:       0.2198
  Distribution:  rope: 0.866, scaffold: 0.134, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9366)
  Entropy:       0.1319
  Distribution:  rope: 0.937, scaffold: 0.063, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0705 (moderate)
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

    Type:      143 mountain | 22 rope | 591 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    156 pristine | 41 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 177 independent | 1 inconclusive
    Signature: 557 false_natural_law | 258 false_ci_rope | 138 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 717 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.8% of corpus shares this signature)
    Purity band: pristine (16.2% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: rope->scaffold (11 constraints share this boundary)
    Orbit Family ID:  F052

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6 (high variance)
  Index Configs:       5
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 5
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  enforcement_gap_exploitation: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for enforcement_gap_exploitation: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.16)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: warehouse_floor_workers
    → Institutional d=0.350

Aggregate Magnitude (Kappa) at Tn: 0.47

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  enforcement_gap_exploitation (rope vs scaffold):
    ! MANDATROPHY GAP: delta_chi = 0.15 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 1 resolution scenario(s):

  ┌─ [omega_perspectival_enforcement_gap_exploitation] CONCEPTUAL CLARIFICATION
  │  Constraint: enforcement_gap_exploitation
  │  Gap: Constraint enforcement_gap_exploitation appears as rope to individuals but scaffold to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive enforcement_gap_exploitation
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

