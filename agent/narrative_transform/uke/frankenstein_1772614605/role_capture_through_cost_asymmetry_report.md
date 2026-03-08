CORPUS CONTEXT: 954 constraints
  Types: 142 mountain, 18 rope, 588 tangled_rope, 186 snare, 9 piton, 10 scaffold
  Network stability: cascading | 794 omegas (736 critical)
  Confidence: 710 deep (75%) | 52 moderate (5%) | 191 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/role_capture_through_cost_asymmetry.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: role_capture_through_cost_asymmetry...
  [BRIDGE] Derived has_sunset_clause(role_capture_through_cost_asymmetry) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: role_capture_through_cost_asymmetry

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: role_capture_through_cost_asymmetry (0-30)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] role_capture_through_cost_asymmetry from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX OK] role_capture_through_cost_asymmetry from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] role_capture_through_cost_asymmetry from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] role_capture_through_cost_asymmetry from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] role_capture_through_cost_asymmetry from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] role_capture_through_cost_asymmetry from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=piton, computed=scaffold
  [INDEX OK] role_capture_through_cost_asymmetry from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  role_capture_through_cost_asymmetry:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,30,0.35,0.62)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,30,0.38,0.5)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4021666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.38)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  role_capture_through_cost_asymmetry -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  role_capture_through_cost_asymmetry (ε=0.48):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.48 × 0.65 × 0.80 = 0.250
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: role_capture_through_cost_asymmetry ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.402 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.623 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9680 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6230
    Effective purity:   0.6048
    Propagation delta:  -0.0181

    Network neighbors (4):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | gita_kurukshetra | tangled_rope | shared_victim | 0.30 | 0.3205 |
    | institutional_capture_of_regulators | unknown | explicit | 1.00 | N/A |
    | organizational_omerta | unknown | explicit | 1.00 | N/A |
    | professional_identity_lock | unknown | explicit | 1.00 | N/A |

  Purity degraded from 0.6230 to 0.6048 by contamination from 4 neighbor(s), primarily gita_kurukshetra (shared_victim, purity 0.3205).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_role_capture_through_cost_asymmetry
    Severity Score:    0.546
    Gap Class:         powerless_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F460

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate), rope (institutional)
    Confidence:       0.9697 (deep)
    Rival Type:       snare (P=0.0303)
    Margin:           +0.9394
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → tangled_rope; powerless → naturalized; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9697 (deep)
  Rival Type:    snare (P=0.0303)
  Margin:        +0.9394
  Entropy:       0.0758
  Distribution:  tangled_rope: 0.970, snare: 0.030, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9691)
  Entropy:       0.0769
  Distribution:  tangled_rope: 0.969, snare: 0.031, piton: 0.000

  Classical/Indexed TV Distance: 0.0005 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,30,0.35,0.62),warning),drift(extraction_accumulation,evidence(extraction_delta,0,30,0.38,0.5),critical),drift(coupling_drift,evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.623,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.38)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(gita_kurukshetra,0.0226875,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.60485,intrinsic_purity,0.623),critical)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      142 mountain | 18 rope | 588 tangled_rope | 186 snare | 9 piton | 10 scaffold
    Purity:    151 pristine | 41 sound | 96 borderline | 647 contaminated | 18 degraded
    Coupling:  733 strongly | 21 weakly | 172 independent | 1 inconclusive
    Signature: 556 false_natural_law | 250 false_ci_rope | 137 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 710 deep | 52 moderate | 191 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.2% of corpus shares this signature)
    Purity band: borderline (10.1% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: tangled_rope->snare (160 constraints share this boundary)
    Orbit Family ID:  F460

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 14
    Unique Type Shifts:  naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 30
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for role_capture_through_cost_asymmetry

[STRUCTURAL SIGNATURE ANALYSIS]
  role_capture_through_cost_asymmetry: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for role_capture_through_cost_asymmetry: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.38),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_continuity
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  role_capture_through_cost_asymmetry (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.27 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [moral_injury_quantification] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether delayed moral injury costs offset immediate intervention costs
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

  ┌─ [retaliation_base_rate] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Gap between perceived and actual retaliation risk
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

  ┌─ [alternative_structure_viability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether alternative structures can match hierarchical efficiency
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

  ┌─ [legal_protection_effectiveness] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether legal protections reduce intervention costs in practice
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

  ┌─ [omega_extraction_blindness_role_capture_through_cost_asymmetry] CONCEPTUAL CLARIFICATION
  │  Constraint: role_capture_through_cost_asymmetry
  │  Gap: Constraint role_capture_through_cost_asymmetry appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from role_capture_through_cost_asymmetry?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does role_capture_through_cost_asymmetry serve?
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
