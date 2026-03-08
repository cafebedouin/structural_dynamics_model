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
