
============================================================
CONSTRAINT REPORT: power_indexed_classification_variance
============================================================
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
[SCENARIO MANAGER] Loading: testsets/power_indexed_classification_variance.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: power_indexed_classification_variance...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: power_indexed_classification_variance

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: power_indexed_classification_variance (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] power_indexed_classification_variance from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] power_indexed_classification_variance from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] power_indexed_classification_variance from context(agent_power(powerful),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] power_indexed_classification_variance from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=mountain, computed=mountain
  [INDEX OK] power_indexed_classification_variance from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] power_indexed_classification_variance from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  power_indexed_classification_variance:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  power_indexed_classification_variance (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: power_indexed_classification_variance ===
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

    Network neighbors (2):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | coordination_extraction_invisibility | tangled_rope | explicit | 1.00 | 0.0000 |
    | false_mountain_persistence | tangled_rope | explicit | 1.00 | 0.5527 |

  No significant contamination — purity unchanged across 2 neighbor(s).

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

    Type:      133 mountain | 18 rope | 577 tangled_rope | 178 snare | 9 piton | 10 scaffold
    Purity:    142 pristine | 41 sound | 89 borderline | 635 contaminated | 18 degraded
    Coupling:  715 strongly | 21 weakly | 163 independent | 1 inconclusive
    Signature: 552 false_natural_law | 236 false_ci_rope | 128 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 51 moderate | 184 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (13.8% of corpus shares this signature)
    Purity band: pristine (15.3% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: mountain->rope (133 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.16666666666666666 (low variance)
  Index Configs:       6
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
  ! ALERT [severe]: type_1_false_summit detected for power_indexed_classification_variance

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  power_indexed_classification_variance: natural_law (confidence: low)
    → NATURAL LAW signature for power_indexed_classification_variance: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.03, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: false_mountain_persistence
============================================================
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
[SCENARIO MANAGER] Loading: testsets/false_mountain_persistence.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: false_mountain_persistence...
  [BRIDGE] Derived has_sunset_clause(false_mountain_persistence) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: false_mountain_persistence

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: false_mountain_persistence (0-20)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] false_mountain_persistence from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] false_mountain_persistence from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] false_mountain_persistence from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] false_mountain_persistence from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] false_mountain_persistence from context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(regional)): declared=piton, computed=scaffold
  [INDEX OK] false_mountain_persistence from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  false_mountain_persistence:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,20,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,20,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.35250000000000004,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  false_mountain_persistence -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  false_mountain_persistence (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.280 f(d)=0.16 χ = 0.48 × 0.16 × 1.00 = 0.076
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: false_mountain_persistence ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.353 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.685833 (borderline)
    Coupling:         weakly_coupled (score: 0.375)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9672 (snare_leaning)
    Coalition:        other

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6858
    Effective purity:   0.5527
    Propagation delta:  -0.1332

    Network neighbors (8):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | agentive_optimism_2026 | snare | shared_beneficiary | 0.30 | 0.4294 |
    | bayes_theorem | tangled_rope | shared_beneficiary | 0.30 | 0.3330 |
    | digital_credentialing_verification | tangled_rope | shared_beneficiary | 0.30 | 0.3266 |
    | duty_contamination_by_extraction | tangled_rope | shared_beneficiary | 0.30 | 0.6283 |
    | false_mountain_naturalization | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |
    | harry_potter_liberalism | tangled_rope | shared_beneficiary | 0.30 | 0.4953 |
    | power_indexed_classification_variance | mountain | explicit | 1.00 | 0.9760 |
    | publishing_embargo | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |

  Purity degraded from 0.6858 to 0.5527 by contamination from 8 neighbor(s), primarily false_mountain_naturalization (shared_beneficiary, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_false_mountain_persistence
    Severity Score:    0.426
    Gap Class:         analytical_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F239

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), naturalized (institutional)
    Confidence:       0.9705 (deep)
    Rival Type:       snare (P=0.0295)
    Margin:           +0.9409
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees naturalized while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9705 (deep)
  Rival Type:    snare (P=0.0295)
  Margin:        +0.9409
  Entropy:       0.0743
  Distribution:  tangled_rope: 0.970, snare: 0.030, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9699)
  Entropy:       0.0754
  Distribution:  tangled_rope: 0.970, snare: 0.030, piton: 0.000

  Classical/Indexed TV Distance: 0.0005 (near_zero)
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

    Type:      133 mountain | 18 rope | 577 tangled_rope | 178 snare | 9 piton | 10 scaffold
    Purity:    142 pristine | 41 sound | 89 borderline | 635 contaminated | 18 degraded
    Coupling:  715 strongly | 21 weakly | 163 independent | 1 inconclusive
    Signature: 552 false_natural_law | 236 false_ci_rope | 128 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 690 deep | 51 moderate | 184 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.5% of corpus shares this signature)
    Purity band: borderline (9.6% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: tangled_rope->snare (151 constraints share this boundary)
    Orbit Family ID:  F239

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.8333333333333334 (high variance)
  Index Configs:       6
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 14
    Unique Type Shifts:  naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 20
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for false_mountain_persistence

[STRUCTURAL SIGNATURE ANALYSIS]
  false_mountain_persistence: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for false_mountain_persistence: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.39999999999999997),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: constraint_enforcers
    → Institutional d=0.280

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  false_mountain_persistence (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.45 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [naturalization_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Epistemic isolation threshold for sustaining false mountain misclassification
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

  ┌─ [coordination_extraction_boundary] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Boundary between legitimate and extractive naturalization
  │
  │  NOTE: Not resolvable via data or logic alone
  │
  │  RESOLUTION STRATEGY:
  │  1. Document competing value frameworks:
  │     - What values support current unknown?
  │     - What values oppose it?
  │     - Are these incommensurable?
  │  2. Propose scaffolded solution:
  │     - Design mechanism respecting both value sets
  │     - Create exit options for dissenters
  │     - Allow preference-based sorting
  │  3. Accept unresolvability if necessary:
  │     - Some omegas represent genuine value pluralism
  │     - Solution: coexistence, not consensus
  └─

  ┌─ [collective_action_recovery_rate] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Recovery rate of collective action capacity after false mountain recognition
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

  ┌─ [enforcer_belief_sincerity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Sincerity of enforcer belief in naturalization narratives
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

  ┌─ [omega_extraction_blindness_false_mountain_persistence] CONCEPTUAL CLARIFICATION
  │  Constraint: false_mountain_persistence
  │  Gap: Constraint false_mountain_persistence appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from false_mountain_persistence?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does false_mountain_persistence serve?
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
CONSTRAINT REPORT: coordination_extraction_invisibility
============================================================
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

