
============================================================
CONSTRAINT REPORT: categorical_violence_as_structural_exclusion
============================================================
CORPUS CONTEXT: 973 constraints
  Types: 146 mountain, 25 rope, 594 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 805 omegas (743 critical)
  Confidence: 720 deep (74%) | 58 moderate (6%) | 194 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/categorical_violence_as_structural_exclusion.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: categorical_violence_as_structural_exclusion...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: categorical_violence_as_structural_exclusion

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: categorical_violence_as_structural_exclusion (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] categorical_violence_as_structural_exclusion from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] categorical_violence_as_structural_exclusion from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] categorical_violence_as_structural_exclusion from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] categorical_violence_as_structural_exclusion from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=rope
  [INDEX MISMATCH] categorical_violence_as_structural_exclusion from context(agent_power(institutional),time_horizon(civilizational),exit_options(constrained),spatial_scope(national)): declared=piton, computed=rope
  [INDEX MISMATCH] categorical_violence_as_structural_exclusion from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 0 | Warning: 4 | Watch: 0

  categorical_violence_as_structural_exclusion:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.42,0.58)
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.28,0.38)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4621666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.23)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  categorical_violence_as_structural_exclusion -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  categorical_violence_as_structural_exclusion (ε=0.38):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.38 × 1.36 × 0.80 = 0.413
    moderate@national: d=0.700 f(d)=1.11 χ = 0.38 × 1.11 × 1.00 = 0.420
    institutional@national: d=0.480 f(d)=0.60 χ = 0.38 × 0.60 × 1.00 = 0.228
    analytical@global: d=0.720 f(d)=1.14 χ = 0.38 × 1.14 × 1.20 = 0.521

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: categorical_violence_as_structural_exclusion ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [no_exit_for_victims]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.462 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.462167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4622
    Effective purity:   0.4458
    Propagation delta:  -0.0164

    Network neighbors (3):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | access_arbitrage | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |
    | poetic_verse_and_past | tangled_rope | shared_beneficiary | 0.30 | 0.3388 |
    | protocol_rigidity_under_unclassified_variance | tangled_rope | shared_beneficiary | 0.30 | 0.8147 |

  Purity degraded from 0.4622 to 0.4458 by contamination from 3 neighbor(s), primarily access_arbitrage (shared_beneficiary, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_categorical_violence_as_structural_exclusion
    Severity Score:    0.496
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F158

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       0.0001 (borderline)
    Rival Type:       tangled_rope (P=0.9988)
    Margin:           -0.9987
    Boundary:         rope->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says rope, MaxEnt says tangled_rope
  Confidence:    0.0001 (borderline)
  Rival Type:    tangled_rope (P=0.9988)
  Margin:        -0.9987
  Entropy:       0.0055
  Distribution:  tangled_rope: 0.999, snare: 0.001, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9989)
  Entropy:       0.0049
  Distribution:  tangled_rope: 0.999, snare: 0.001, piton: 0.000

  Classical/Indexed TV Distance: 0.0001 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate

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

  Tensions (1):
    abductive: abductive_tension([trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      146 mountain | 25 rope | 594 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 99 borderline | 651 contaminated | 19 degraded
    Coupling:  741 strongly | 21 weakly | 183 independent | 1 inconclusive
    Signature: 557 false_natural_law | 264 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 58 moderate | 194 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.1% of corpus shares this signature)
    Purity band: contaminated (66.9% of corpus in this band)
    Confidence band: borderline (20.0% of corpus in this band)
    Boundary zone: rope->tangled_rope (7 constraints share this boundary)
    Orbit Family ID:  F158

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 8
    Unique Type Shifts:  rope -> tangled_rope, rope -> unknown, unknown -> tangled_rope

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for categorical_violence_as_structural_exclusion

[STRUCTURAL SIGNATURE ANALYSIS]
  categorical_violence_as_structural_exclusion: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for categorical_violence_as_structural_exclusion: Appears to be rope (explicit_rope_claim) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([rope,tangled_rope]),excess_above_floor(0.23),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_administrators
    → Institutional d=0.480

Aggregate Magnitude (Kappa) at Tn: 0.52

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  categorical_violence_as_structural_exclusion (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.19 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [category_boundary_arbitrariness] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether category boundaries reflect medical necessity or administrative cost containment
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

  ┌─ [exception_clause_accessibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether exception processes provide genuine recourse or theater
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

  ┌─ [frontline_worker_moral_injury] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Proportion of worker burnout attributable to categorical enforcement
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

  ┌─ [category_expansion_trajectory] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Trajectory of category boundary evolution over time
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

  ┌─ [omega_extraction_blindness_categorical_violence_as_structural_exclusion] CONCEPTUAL CLARIFICATION
  │  Constraint: categorical_violence_as_structural_exclusion
  │  Gap: Constraint categorical_violence_as_structural_exclusion appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from categorical_violence_as_structural_exclusion?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does categorical_violence_as_structural_exclusion serve?
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
CONSTRAINT REPORT: extraction_residue_as_bodily_inscription
============================================================
CORPUS CONTEXT: 973 constraints
  Types: 146 mountain, 25 rope, 594 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 805 omegas (743 critical)
  Confidence: 720 deep (74%) | 58 moderate (6%) | 194 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/extraction_residue_as_bodily_inscription.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: extraction_residue_as_bodily_inscription...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: extraction_residue_as_bodily_inscription

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: extraction_residue_as_bodily_inscription (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] extraction_residue_as_bodily_inscription from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] extraction_residue_as_bodily_inscription from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] extraction_residue_as_bodily_inscription from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] extraction_residue_as_bodily_inscription from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] extraction_residue_as_bodily_inscription from context(agent_power(powerful),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  extraction_residue_as_bodily_inscription:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  extraction_residue_as_bodily_inscription (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: extraction_residue_as_bodily_inscription ===
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

    Type:      146 mountain | 25 rope | 594 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 99 borderline | 651 contaminated | 19 degraded
    Coupling:  741 strongly | 21 weakly | 183 independent | 1 inconclusive
    Signature: 557 false_natural_law | 264 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 58 moderate | 194 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.5% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.1% of corpus in this band)
    Boundary zone: mountain->rope (146 constraints share this boundary)

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
  ! ALERT [severe]: type_1_false_summit detected for extraction_residue_as_bodily_inscription

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  extraction_residue_as_bodily_inscription: natural_law (confidence: low)
    → NATURAL LAW signature for extraction_residue_as_bodily_inscription: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.03, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: protocol_inversion_as_constraint_escape
============================================================
CORPUS CONTEXT: 973 constraints
  Types: 146 mountain, 25 rope, 594 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 805 omegas (743 critical)
  Confidence: 720 deep (74%) | 58 moderate (6%) | 194 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/protocol_inversion_as_constraint_escape.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: protocol_inversion_as_constraint_escape...
  [BRIDGE] Derived has_sunset_clause(protocol_inversion_as_constraint_escape) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: protocol_inversion_as_constraint_escape

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: protocol_inversion_as_constraint_escape (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] protocol_inversion_as_constraint_escape from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX OK] protocol_inversion_as_constraint_escape from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] protocol_inversion_as_constraint_escape from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] protocol_inversion_as_constraint_escape from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] protocol_inversion_as_constraint_escape from context(agent_power(powerless),time_horizon(generational),exit_options(identity_locked),spatial_scope(national)): declared=snare, computed=scaffold
  [INDEX OK] protocol_inversion_as_constraint_escape from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] protocol_inversion_as_constraint_escape from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  protocol_inversion_as_constraint_escape:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,9,0.35,0.52)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,9,0.48,0.6)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3205,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  protocol_inversion_as_constraint_escape -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  protocol_inversion_as_constraint_escape (ε=0.58):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.58 × 0.65 × 0.80 = 0.302
    moderate@national: d=0.700 f(d)=1.11 χ = 0.58 × 1.11 × 1.00 = 0.642
    institutional@national: d=0.480 f(d)=0.60 χ = 0.58 × 0.60 × 1.00 = 0.347
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: protocol_inversion_as_constraint_escape ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.321 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.278833 (degraded)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9920 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.2788
    Effective purity:   0.2788
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_protocol_inversion_as_constraint_escape
    Severity Score:    0.516
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F453

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate), snare (analytical)
    Confidence:       0.8762 (deep)
    Rival Type:       snare (P=0.1238)
    Margin:           +0.7524
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: powerless, institutional → naturalized; moderate → tangled_rope; analytical → snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.8762 (deep)
  Rival Type:    snare (P=0.1238)
  Margin:        +0.7524
  Entropy:       0.2090
  Distribution:  tangled_rope: 0.876, snare: 0.124

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.8737)
  Entropy:       0.2117
  Distribution:  tangled_rope: 0.874, snare: 0.126

  Classical/Indexed TV Distance: 0.0026 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,9,0.35,0.52),warning),drift(extraction_accumulation,evidence(extraction_delta,0,9,0.48,0.6),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.2788333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)]),critical)])

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      146 mountain | 25 rope | 594 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 99 borderline | 651 contaminated | 19 degraded
    Coupling:  741 strongly | 21 weakly | 183 independent | 1 inconclusive
    Signature: 557 false_natural_law | 264 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 58 moderate | 194 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.1% of corpus shares this signature)
    Purity band: degraded (2.0% of corpus in this band)
    Confidence band: deep (74.1% of corpus in this band)
    Boundary zone: tangled_rope->snare (165 constraints share this boundary)
    Orbit Family ID:  F453

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5714285714285714 (high variance)
  Index Configs:       7
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
  ! ALERT [informational]: perspectival_incoherence detected for protocol_inversion_as_constraint_escape

[STRUCTURAL SIGNATURE ANALYSIS]
  protocol_inversion_as_constraint_escape: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for protocol_inversion_as_constraint_escape: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),scope_variant([snare,tangled_rope]),excess_above_floor(0.48),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: workers_post_recovery
    → Institutional d=0.480

Aggregate Magnitude (Kappa) at Tn: 0.52

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  protocol_inversion_as_constraint_escape (snare vs tangled_rope):
    ! MANDATROPHY GAP: delta_chi = 0.05 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [outcome_measurement_ambiguity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether outcome gap reflects measurement failure or physiological unpredictability
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

  ┌─ [protocol_intent_ambiguity] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether protocol elimination outcome was designed or emergent
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

  ┌─ [recovery_pathway_sufficiency] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether post-protocol recovery represents genuine constraint escape
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

  ┌─ [institutional_epistemic_loss] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether institutional inability to explain outcomes is epistemic loss or strategic
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

  ┌─ [omega_extraction_blindness_protocol_inversion_as_constraint_escape] CONCEPTUAL CLARIFICATION
  │  Constraint: protocol_inversion_as_constraint_escape
  │  Gap: Constraint protocol_inversion_as_constraint_escape appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from protocol_inversion_as_constraint_escape?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does protocol_inversion_as_constraint_escape serve?
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

