
============================================================
CONSTRAINT REPORT: architectural_constraint_as_dual_substrate
============================================================
CORPUS CONTEXT: 988 constraints
  Types: 149 mountain, 26 rope, 601 tangled_rope, 192 snare, 9 piton, 10 scaffold
  Network stability: cascading | 817 omegas (755 critical)
  Confidence: 731 deep (74%) | 57 moderate (6%) | 199 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/architectural_constraint_as_dual_substrate.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: architectural_constraint_as_dual_substrate...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: architectural_constraint_as_dual_substrate

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: architectural_constraint_as_dual_substrate (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] architectural_constraint_as_dual_substrate from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] architectural_constraint_as_dual_substrate from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] architectural_constraint_as_dual_substrate from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] architectural_constraint_as_dual_substrate from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] architectural_constraint_as_dual_substrate from context(agent_power(powerful),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  architectural_constraint_as_dual_substrate:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9600000000000001,decline_signals,[excess_above_floor(0.09999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  architectural_constraint_as_dual_substrate (ε=0.12):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.12 × 1.42 × 0.80 = 0.136
    moderate@national: d=0.646 f(d)=1.00 χ = 0.12 × 1.00 × 1.00 = 0.120
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.12 × -0.12 × 1.00 = -0.014
    analytical@global: d=0.725 f(d)=1.15 χ = 0.12 × 1.15 × 1.20 = 0.166

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: architectural_constraint_as_dual_substrate ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.960 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.96 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9600
    Effective purity:   0.9600
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | collective_action_as_leverage_conversion | tangled_rope | explicit | 1.00 | 0.4205 |

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

    Type:      149 mountain | 26 rope | 601 tangled_rope | 192 snare | 9 piton | 10 scaffold
    Purity:    164 pristine | 42 sound | 102 borderline | 660 contaminated | 19 degraded
    Coupling:  752 strongly | 22 weakly | 186 independent | 1 inconclusive
    Signature: 560 false_natural_law | 273 false_ci_rope | 144 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 731 deep | 57 moderate | 199 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.6% of corpus shares this signature)
    Purity band: pristine (16.6% of corpus in this band)
    Confidence band: deep (74.1% of corpus in this band)
    Boundary zone: mountain->rope (149 constraints share this boundary)

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
  ! ALERT [severe]: type_1_false_summit detected for architectural_constraint_as_dual_substrate

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  architectural_constraint_as_dual_substrate: natural_law (confidence: low)
    → NATURAL LAW signature for architectural_constraint_as_dual_substrate: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.03, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: collective_action_as_leverage_conversion
============================================================
CORPUS CONTEXT: 988 constraints
  Types: 149 mountain, 26 rope, 601 tangled_rope, 192 snare, 9 piton, 10 scaffold
  Network stability: cascading | 817 omegas (755 critical)
  Confidence: 731 deep (74%) | 57 moderate (6%) | 199 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/collective_action_as_leverage_conversion.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: collective_action_as_leverage_conversion...
  [BRIDGE] Derived has_sunset_clause(collective_action_as_leverage_conversion) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: collective_action_as_leverage_conversion

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: collective_action_as_leverage_conversion (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=tangled_rope, computed=scaffold
  [INDEX OK] collective_action_as_leverage_conversion from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] collective_action_as_leverage_conversion from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 0 | Warning: 3 | Watch: 0

  collective_action_as_leverage_conversion:
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.28,0.38)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4205,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.23)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  collective_action_as_leverage_conversion -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  collective_action_as_leverage_conversion (ε=0.38):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.38 × 1.36 × 0.80 = 0.413
    moderate@national: d=0.700 f(d)=1.11 χ = 0.38 × 1.11 × 1.00 = 0.420
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.38 × -0.04 × 1.00 = -0.016
    analytical@global: d=0.720 f(d)=1.14 χ = 0.38 × 1.14 × 1.20 = 0.521

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: collective_action_as_leverage_conversion ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=scaffold  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,has_beneficiaries,sunset]
  Voids:      [self_sustaining_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.420 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (maxent, abductive)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.4205 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     3 — extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4205
    Effective purity:   0.4205
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | architectural_constraint_as_dual_substrate | mountain | explicit | 1.00 | 0.9600 |

  No significant contamination — purity unchanged across 1 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_collective_action_as_leverage_conversion
    Severity Score:    0.396
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F182

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       scaffold (powerless), tangled_rope (analytical)
    Confidence:       0.2746 (borderline)
    Rival Type:       tangled_rope (P=0.6867)
    Margin:           -0.4121
    Boundary:         rope->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: analytical sees tangled_rope while powerless, moderate, institutional see scaffold.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says rope, MaxEnt says tangled_rope
  Confidence:    0.2746 (borderline)
  Rival Type:    tangled_rope (P=0.6867)
  Margin:        -0.4121
  Entropy:       0.4124
  Distribution:  tangled_rope: 0.687, rope: 0.275, scaffold: 0.039

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9935)
  Entropy:       0.0219
  Distribution:  tangled_rope: 0.993, scaffold: 0.006, snare: 0.000

  Classical/Indexed TV Distance: 0.3068 (large)
  Significant divergence — observer-dependence changes the probabilistic landscape. Classical Oracle Gap (Theorem 4): single-position analysis misses this structure.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_divergence | 0.88 | indexing_divergence_with_obstruction | genuine | Indexed and classical MaxEnt disagree: observer-dependence has probabilistic consequences beyond categorical shifts (Theorem 4). |
  | confirmed_liminal | 0.85 | triple_confirmed_liminality | genuine | Triple confirmation of genuine liminality: high entropy, multi-type orbit, and active drift. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    signature, purity, fingerprint_voids, drift, context_gap, fcr_gate

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
    maxent: entropy_flag(0.41238909210244096)
    abductive: abductive_tension([trigger(confirmed_liminal,0.85,triple_confirmed_liminality,genuine),trigger(maxent_divergence,0.88,indexing_divergence_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **4 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      149 mountain | 26 rope | 601 tangled_rope | 192 snare | 9 piton | 10 scaffold
    Purity:    164 pristine | 42 sound | 102 borderline | 660 contaminated | 19 degraded
    Coupling:  752 strongly | 22 weakly | 186 independent | 1 inconclusive
    Signature: 560 false_natural_law | 273 false_ci_rope | 144 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 731 deep | 57 moderate | 199 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.6% of corpus shares this signature)
    Purity band: contaminated (66.8% of corpus in this band)
    Confidence band: borderline (20.2% of corpus in this band)
    Boundary zone: rope->tangled_rope (8 constraints share this boundary)
    Orbit Family ID:  F182

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 4
    Unique Type Shifts:  scaffold -> unknown

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for collective_action_as_leverage_conversion

[STRUCTURAL SIGNATURE ANALYSIS]
  collective_action_as_leverage_conversion: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for collective_action_as_leverage_conversion: Appears to be rope (explicit_rope_claim) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([scaffold,unknown]),excess_above_floor(0.23),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: organized_workers
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.49

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  collective_action_as_leverage_conversion (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.43 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [cohesion_maintenance_cost_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where cohesion maintenance costs exceed collective bargaining benefits
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

  ┌─ [free_rider_extraction_magnitude] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether free-rider problem constitutes extraction or coordination cost
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

  ┌─ [institutional_capture_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Timeline and inevitability of institutional capture in labor organizations
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

  ┌─ [sectoral_vs_enterprise_bargaining_efficiency] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Comparative efficiency of sectoral vs enterprise bargaining structures
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

  ┌─ [omega_extraction_blindness_collective_action_as_leverage_conversion] CONCEPTUAL CLARIFICATION
  │  Constraint: collective_action_as_leverage_conversion
  │  Gap: Constraint collective_action_as_leverage_conversion appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from collective_action_as_leverage_conversion?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does collective_action_as_leverage_conversion serve?
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
CONSTRAINT REPORT: bureaucratic_accommodation_as_extraction_persistence
============================================================
CORPUS CONTEXT: 988 constraints
  Types: 149 mountain, 26 rope, 601 tangled_rope, 192 snare, 9 piton, 10 scaffold
  Network stability: cascading | 817 omegas (755 critical)
  Confidence: 731 deep (74%) | 57 moderate (6%) | 199 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/bureaucratic_accommodation_as_extraction_persistence.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: bureaucratic_accommodation_as_extraction_persistence...
  [BRIDGE] Derived has_sunset_clause(bureaucratic_accommodation_as_extraction_persistence) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: bureaucratic_accommodation_as_extraction_persistence

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: bureaucratic_accommodation_as_extraction_persistence (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=snare
  [INDEX OK] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(powerful),time_horizon(immediate),exit_options(mobile),spatial_scope(local)): declared=rope, computed=piton
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=piton
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(national)): declared=piton, computed=rope
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 3 | Warning: 1 | Watch: 0

  bureaucratic_accommodation_as_extraction_persistence:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.55,0.78)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5800000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  bureaucratic_accommodation_as_extraction_persistence -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  bureaucratic_accommodation_as_extraction_persistence (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: bureaucratic_accommodation_as_extraction_persistence ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9974 (snare_leaning)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3529
    Propagation delta:  -0.0013

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | the_calm_protocol_suppression | tangled_rope | shared_beneficiary | 0.30 | 0.3205 |

  Purity degraded from 0.3542 to 0.3529 by contamination from 1 neighbor(s), primarily the_calm_protocol_suppression (shared_beneficiary, purity 0.3205).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_bureaucratic_accommodation_as_extraction_persistence
    Severity Score:    0.716
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F149

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    Confidence:       0.6136 (moderate)
    Rival Type:       snare (P=0.3828)
    Margin:           +0.2308
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.6136 (moderate)
  Rival Type:    snare (P=0.3828)
  Margin:        +0.2308
  Entropy:       0.3837
  Distribution:  tangled_rope: 0.614, snare: 0.383, piton: 0.004

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.6170)
  Entropy:       0.3748
  Distribution:  tangled_rope: 0.617, snare: 0.382, piton: 0.001

  Classical/Indexed TV Distance: 0.0034 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (5 subsystems):
    purity, dirac, fingerprint_voids, context_gap, fcr_gate

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

  Tensions (2):
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,10,0.55,0.78),critical),drift(extraction_accumulation,evidence(extraction_delta,0,10,0.48,0.68),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5800000000000001)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(the_calm_protocol_suppression,0.0025250000000000008,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.3529041666666667,intrinsic_purity,0.3541666666666667),warning)])

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

    Type:      149 mountain | 26 rope | 601 tangled_rope | 192 snare | 9 piton | 10 scaffold
    Purity:    164 pristine | 42 sound | 102 borderline | 660 contaminated | 19 degraded
    Coupling:  752 strongly | 22 weakly | 186 independent | 1 inconclusive
    Signature: 560 false_natural_law | 273 false_ci_rope | 144 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 731 deep | 57 moderate | 199 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.6% of corpus shares this signature)
    Purity band: contaminated (66.8% of corpus in this band)
    Confidence band: moderate (5.8% of corpus in this band)
    Boundary zone: tangled_rope->snare (170 constraints share this boundary)
    Orbit Family ID:  F149

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twin Group:
    Signature:   (0.7, 0.7, False, True)
    Group Size:  31
    Types:       snare, tangled_rope

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  piton -> snare, piton -> tangled_rope, rope -> piton, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for bureaucratic_accommodation_as_extraction_persistence

[STRUCTURAL SIGNATURE ANALYSIS]
  bureaucratic_accommodation_as_extraction_persistence: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for bureaucratic_accommodation_as_extraction_persistence: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),scope_variant([snare,tangled_rope]),excess_above_floor(0.5800000000000001),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_authority
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  bureaucratic_accommodation_as_extraction_persistence (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.77 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [approval_stratification_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether approval stratification reflects feasibility or institutional preference
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

  ┌─ [procedural_cost_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Procedural cost threshold distinguishing coordination from extraction
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

  ┌─ [universal_design_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether universal design sunset is structurally achievable
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

  ┌─ [architectural_substrate_mutability] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether architectural barriers are structurally necessary or institutionally maintained
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

  ┌─ [omega_extraction_blindness_bureaucratic_accommodation_as_extraction_persistence] CONCEPTUAL CLARIFICATION
  │  Constraint: bureaucratic_accommodation_as_extraction_persistence
  │  Gap: Constraint bureaucratic_accommodation_as_extraction_persistence appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from bureaucratic_accommodation_as_extraction_persistence?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does bureaucratic_accommodation_as_extraction_persistence serve?
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

