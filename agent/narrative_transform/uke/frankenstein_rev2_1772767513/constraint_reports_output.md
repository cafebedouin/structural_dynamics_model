
============================================================
CONSTRAINT REPORT: capability_compulsion_gradient
============================================================
CORPUS CONTEXT: 982 constraints
  Types: 147 mountain, 25 rope, 599 tangled_rope, 191 snare, 9 piton, 10 scaffold
  Network stability: cascading | 813 omegas (751 critical)
  Confidence: 728 deep (74%) | 56 moderate (6%) | 197 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/capability_compulsion_gradient.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: capability_compulsion_gradient...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: capability_compulsion_gradient

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: capability_compulsion_gradient (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] capability_compulsion_gradient from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(powerful),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  capability_compulsion_gradient:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9440000000000001,decline_signals,[excess_above_floor(0.14)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  capability_compulsion_gradient (ε=0.22):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.22 × 1.42 × 0.80 = 0.250
    moderate@national: d=0.646 f(d)=1.00 χ = 0.22 × 1.00 × 1.00 = 0.220
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.22 × -0.12 × 1.00 = -0.026
    analytical@global: d=0.725 f(d)=1.15 χ = 0.22 × 1.15 × 1.20 = 0.304

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: capability_compulsion_gradient ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.944 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.944 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9440
    Effective purity:   0.9440
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

    Type:      147 mountain | 25 rope | 599 tangled_rope | 191 snare | 9 piton | 10 scaffold
    Purity:    162 pristine | 42 sound | 101 borderline | 657 contaminated | 19 degraded
    Coupling:  748 strongly | 22 weakly | 184 independent | 1 inconclusive
    Signature: 560 false_natural_law | 269 false_ci_rope | 142 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 728 deep | 56 moderate | 197 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.5% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.2% of corpus in this band)
    Boundary zone: mountain->rope (147 constraints share this boundary)

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
  ! ALERT [severe]: type_1_false_summit detected for capability_compulsion_gradient

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  capability_compulsion_gradient: natural_law (confidence: low)
    → NATURAL LAW signature for capability_compulsion_gradient: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.04, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================


============================================================
CONSTRAINT REPORT: optimization_as_entrapment
============================================================
CORPUS CONTEXT: 982 constraints
  Types: 147 mountain, 25 rope, 599 tangled_rope, 191 snare, 9 piton, 10 scaffold
  Network stability: cascading | 813 omegas (751 critical)
  Confidence: 728 deep (74%) | 56 moderate (6%) | 197 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/optimization_as_entrapment.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: optimization_as_entrapment...
  [BRIDGE] Derived has_sunset_clause(optimization_as_entrapment) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: optimization_as_entrapment

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: optimization_as_entrapment (0-9)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] optimization_as_entrapment from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX MISMATCH] optimization_as_entrapment from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] optimization_as_entrapment from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] optimization_as_entrapment from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] optimization_as_entrapment from context(agent_power(institutional),time_horizon(civilizational),exit_options(constrained),spatial_scope(global)): declared=piton, computed=scaffold
  [INDEX MISMATCH] optimization_as_entrapment from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  optimization_as_entrapment:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,9,0.35,0.52)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,9,0.38,0.58)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.49999999999999994)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  optimization_as_entrapment -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  optimization_as_entrapment (ε=0.58):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.58 × 0.65 × 0.80 = 0.302
    moderate@national: d=0.700 f(d)=1.11 χ = 0.58 × 1.11 × 1.00 = 0.642
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.58 × -0.04 × 1.00 = -0.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: optimization_as_entrapment ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.3125 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9964 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3125
    Effective purity:   0.3007
    Propagation delta:  -0.0118

    Network neighbors (2):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | epistemic_overload_collapse | tangled_rope | shared_victim | 0.30 | 0.0000 |
    | social_credit_architecture | tangled_rope | shared_beneficiary | 0.30 | 0.3111 |

  Purity degraded from 0.3125 to 0.3007 by contamination from 2 neighbor(s), primarily epistemic_overload_collapse (shared_victim, purity 0.0000).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, snare, tangled_rope]
  Orbit Span:         4
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_optimization_as_entrapment
    Severity Score:    0.654
    Gap Class:         powerless_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F427

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate), rope (institutional), snare (analytical)
    Confidence:       0.7214 (moderate)
    Rival Type:       snare (P=0.2786)
    Margin:           +0.4429
    Boundary:         tangled_rope->snare
    H^1 band:         6 — Maximally fractured — all 4 observers disagree: powerless → naturalized; moderate → tangled_rope; institutional → rope; analytical → snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7214 (moderate)
  Rival Type:    snare (P=0.2786)
  Margin:        +0.4429
  Entropy:       0.3302
  Distribution:  tangled_rope: 0.721, snare: 0.279

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.7167)
  Entropy:       0.3326
  Distribution:  tangled_rope: 0.717, snare: 0.283

  Classical/Indexed TV Distance: 0.0047 (near_zero)
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
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])

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

    Type:      147 mountain | 25 rope | 599 tangled_rope | 191 snare | 9 piton | 10 scaffold
    Purity:    162 pristine | 42 sound | 101 borderline | 657 contaminated | 19 degraded
    Coupling:  748 strongly | 22 weakly | 184 independent | 1 inconclusive
    Signature: 560 false_natural_law | 269 false_ci_rope | 142 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 728 deep | 56 moderate | 197 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.4% of corpus shares this signature)
    Purity band: contaminated (66.9% of corpus in this band)
    Confidence band: moderate (5.7% of corpus in this band)
    Boundary zone: tangled_rope->snare (168 constraints share this boundary)
    Orbit Family ID:  F427

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.8333333333333334 (high variance)
  Index Configs:       6
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 15
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 9
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for optimization_as_entrapment

[STRUCTURAL SIGNATURE ANALYSIS]
  optimization_as_entrapment: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for optimization_as_entrapment: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([snare,tangled_rope]),excess_above_floor(0.49999999999999994),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: system_administrators
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  optimization_as_entrapment (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.33 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [deliberation_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Decision latency threshold for deliberative capacity loss
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

  ┌─ [identity_fusion_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether identity fusion is inherent to optimization
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

  ┌─ [ethical_override_training_effectiveness] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether deliberative preservation protocols work
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

  ┌─ [suppression_mechanism_ambiguity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Structural vs. internalized suppression mechanism
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

  ┌─ [omega_extraction_blindness_optimization_as_entrapment] CONCEPTUAL CLARIFICATION
  │  Constraint: optimization_as_entrapment
  │  Gap: Constraint optimization_as_entrapment appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from optimization_as_entrapment?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does optimization_as_entrapment serve?
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
CONSTRAINT REPORT: complicity_through_competence
============================================================
CORPUS CONTEXT: 982 constraints
  Types: 147 mountain, 25 rope, 599 tangled_rope, 191 snare, 9 piton, 10 scaffold
  Network stability: cascading | 813 omegas (751 critical)
  Confidence: 728 deep (74%) | 56 moderate (6%) | 197 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/complicity_through_competence.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: complicity_through_competence...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: complicity_through_competence

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: complicity_through_competence (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] complicity_through_competence from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=naturalized
  [INDEX OK] complicity_through_competence from context(agent_power(moderate),time_horizon(immediate),exit_options(constrained),spatial_scope(regional)): declared=snare, computed=snare
  [INDEX OK] complicity_through_competence from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] complicity_through_competence from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] complicity_through_competence from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  complicity_through_competence:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  complicity_through_competence -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  complicity_through_competence (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: complicity_through_competence ===
  Shift (computed via dr_type/3):
    powerless=naturalized  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_ci_rope
    Purity:           0.3125 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3125
    Effective purity:   0.3124
    Propagation delta:  -0.0001

    Network neighbors (3):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | iron_law_of_oligarchy | tangled_rope | shared_beneficiary | 0.30 | 0.3088 |
    | ritualistic_transition_scaffold | tangled_rope | shared_beneficiary | 0.30 | 0.6906 |
    | role_capture_through_cost_asymmetry | tangled_rope | shared_beneficiary | 0.30 | 0.5676 |

  Purity degraded from 0.3125 to 0.3124 by contamination from 3 neighbor(s), primarily iron_law_of_oligarchy (shared_beneficiary, purity 0.3088).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, snare]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_complicity_through_competence
    Severity Score:    0.716
    Gap Class:         powerless_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F186

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), snare (moderate), rope (institutional)
    Confidence:       0.3408 (borderline)
    Rival Type:       tangled_rope (P=0.6592)
    Margin:           -0.3184
    Boundary:         snare->tangled_rope
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → naturalized; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says snare, MaxEnt says tangled_rope
  Confidence:    0.3408 (borderline)
  Rival Type:    tangled_rope (P=0.6592)
  Margin:        -0.3184
  Entropy:       0.3581
  Distribution:  tangled_rope: 0.659, snare: 0.341, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.6608)
  Entropy:       0.3575
  Distribution:  tangled_rope: 0.661, snare: 0.339, piton: 0.000

  Classical/Indexed TV Distance: 0.0016 (near_zero)
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

    Type:      147 mountain | 25 rope | 599 tangled_rope | 191 snare | 9 piton | 10 scaffold
    Purity:    162 pristine | 42 sound | 101 borderline | 657 contaminated | 19 degraded
    Coupling:  748 strongly | 22 weakly | 184 independent | 1 inconclusive
    Signature: 560 false_natural_law | 269 false_ci_rope | 142 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 728 deep | 56 moderate | 197 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.4% of corpus shares this signature)
    Purity band: contaminated (66.9% of corpus in this band)
    Confidence band: borderline (20.1% of corpus in this band)
    Boundary zone: snare->tangled_rope (188 constraints share this boundary)
    Orbit Family ID:  F186

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6 (high variance)
  Index Configs:       5
  Types Produced:      3

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
  ! ALERT [informational]: perspectival_incoherence detected for complicity_through_competence

[STRUCTURAL SIGNATURE ANALYSIS]
  complicity_through_competence: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for complicity_through_competence: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([snare,tangled_rope]),excess_above_floor(0.6000000000000001),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_continuity
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  complicity_through_competence (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.38 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [moral_knowledge_threshold] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Threshold at which professional competence implies moral knowledge of harm
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

  ┌─ [exit_cost_vs_complicity_severity] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Relative weighting of exit cost vs complicity severity
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

  ┌─ [routine_as_suppression_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether routine itself suppresses moral awareness or requires active suppression
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

  ┌─ [competence_signal_vs_harm_correlation] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether professional excellence is genuinely correlated with harm or spuriously
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

  ┌─ [omega_extraction_blindness_complicity_through_competence] CONCEPTUAL CLARIFICATION
  │  Constraint: complicity_through_competence
  │  Gap: Constraint complicity_through_competence appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from complicity_through_competence?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does complicity_through_competence serve?
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

