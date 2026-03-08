CORPUS CONTEXT: 923 constraints
  Types: 132 mountain, 18 rope, 576 tangled_rope, 177 snare, 9 piton, 10 scaffold
  Network stability: cascading | 774 omegas (718 critical)
  Confidence: 688 deep (75%) | 51 moderate (6%) | 183 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/collective_action_threshold.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: collective_action_threshold...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: collective_action_threshold

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: collective_action_threshold (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] collective_action_threshold from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] collective_action_threshold from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] collective_action_threshold from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] collective_action_threshold from context(agent_power(organized),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] collective_action_threshold from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX OK] collective_action_threshold from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 1 | Watch: 0

  collective_action_threshold:
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3925,decline_signals,[coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.3)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  collective_action_threshold (ε=0.38):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.38 × 1.36 × 0.80 = 0.413
    moderate@national: d=0.700 f(d)=1.11 χ = 0.38 × 1.11 × 1.00 = 0.420
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.38 × -0.04 × 1.00 = -0.016
    analytical@global: d=0.720 f(d)=1.14 χ = 0.38 × 1.14 × 1.20 = 0.521

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: collective_action_threshold ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,has_beneficiaries,sunset]
  Voids:      [self_sustaining_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=falling  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.393 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     scaffold
    Signature:        false_natural_law
    Purity:           0.3925 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3925
    Effective purity:   0.3925
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: collective_action_threshold
    Severity Score:    0.334
    Gap Class:         consensus
    Gap Pattern:       unknown
    Family ID:         F044

  Omega: omega_learned_helplessness_collective_action_threshold
    Severity Score:    0.334
    Gap Class:         consensus
    Gap Pattern:       snare_mountain_confusion
    Family ID:         F044

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.0100 (borderline)
    Rival Type:       tangled_rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         scaffold->tangled_rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says scaffold, MaxEnt says tangled_rope
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

    Type:      132 mountain | 18 rope | 576 tangled_rope | 177 snare | 9 piton | 10 scaffold
    Purity:    141 pristine | 41 sound | 88 borderline | 634 contaminated | 18 degraded
    Coupling:  714 strongly | 20 weakly | 162 independent | 1 inconclusive
    Signature: 551 false_natural_law | 235 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 688 deep | 51 moderate | 183 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (59.7% of corpus shares this signature)
    Purity band: contaminated (68.7% of corpus in this band)
    Confidence band: borderline (19.8% of corpus in this band)
    Boundary zone: scaffold->tangled_rope (7 constraints share this boundary)
    Orbit Family ID:  F044

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.8333333333333334 (high variance)
  Index Configs:       6
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 4
    Unique Type Shifts:  scaffold -> unknown

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  collective_action_threshold: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for collective_action_threshold: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=1.000 with 6 coupled dimension pairs. Excess extraction=0.3. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.50

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  collective_action_threshold (snare vs mountain):
    ! MANDATROPHY GAP: delta_chi = 0.43 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [threshold_height_decomposition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Decomposition of threshold into inherent vs imposed components
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

  ┌─ [critical_mass_stability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Stability of critical mass threshold across suppression regimes
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

  ┌─ [sunset_mechanism_reliability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Reliability of sunset mechanism post-threshold
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

  ┌─ [index_transformation_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Reversibility of power position transformation
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

  ┌─ [omega_learned_helplessness_collective_action_threshold] CONCEPTUAL CLARIFICATION
  │  Constraint: collective_action_threshold
  │  Gap: Constraint collective_action_threshold appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...
  │
  │  CRITICAL: Learned Helplessness Pattern
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: MOUNTAIN (unchangeable law)
  │
  │  RESOLUTION STRATEGY:
  │  1. Test changeability:
  │     - Can institutions modify collective_action_threshold?
  │     - What legal/political mechanisms exist?
  │     - Historical precedents of change?
  │  2. Test extraction:
  │     - Is benefit flow symmetric or asymmetric?
  │     - Who has veto power over changes?
  │  3. Decision tree:
  │     IF truly unchangeable + extractive → MANDATROPHY
  │     IF changeable + extractive → Correct to SNARE
  │     IF unchangeable + fair → Correct to MOUNTAIN
  │     IF institutions falsely claim necessity → SNARE + fraud flag
  └─

====================================================
