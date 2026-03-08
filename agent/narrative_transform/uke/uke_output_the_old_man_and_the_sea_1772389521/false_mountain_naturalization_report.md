CORPUS CONTEXT: 920 constraints
  Types: 132 mountain, 18 rope, 574 tangled_rope, 177 snare, 9 piton, 9 scaffold
  Network stability: cascading | 771 omegas (716 critical)
  Confidence: 686 deep (75%) | 51 moderate (6%) | 182 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/false_mountain_naturalization.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: false_mountain_naturalization...
  [BRIDGE] Derived has_sunset_clause(false_mountain_naturalization) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: false_mountain_naturalization

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: false_mountain_naturalization (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] false_mountain_naturalization from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] false_mountain_naturalization from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] false_mountain_naturalization from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX OK] false_mountain_naturalization from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] false_mountain_naturalization from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] false_mountain_naturalization from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INDEX OK] false_mountain_naturalization from context(agent_power(analytical),time_horizon(generational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  false_mountain_naturalization:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.42,0.58)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.49999999999999994)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  false_mountain_naturalization -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  false_mountain_naturalization (ε=0.58):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.58 × 1.36 × 0.80 = 0.630
    moderate@national: d=0.500 f(d)=0.65 χ = 0.58 × 0.65 × 1.00 = 0.377
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.58 × -0.04 × 1.00 = -0.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: false_mountain_naturalization ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_natural_law
    Purity:           0.3125 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     3 — extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3125
    Effective purity:   0.3125
    Propagation delta:  +0.0000

    Network neighbors (6):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | agentive_optimism_2026 | snare | shared_beneficiary | 0.30 | 0.4297 |
    | bayes_theorem | tangled_rope | shared_beneficiary | 0.30 | 0.3340 |
    | digital_credentialing_verification | tangled_rope | shared_beneficiary | 0.30 | 0.3343 |
    | harry_potter_liberalism | tangled_rope | shared_beneficiary | 0.30 | 0.4972 |
    | indexical_extraction_asymmetry | snare | explicit | 1.00 | 0.3195 |
    | publishing_embargo | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |

  No significant contamination — purity unchanged across 6 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_false_mountain_naturalization
    Severity Score:    0.506
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F235

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

  No abductive triggers fired. All diagnostic paths agree.

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (9 subsystems):
    maxent, cohomology, abductive, signature, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions: none

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      132 mountain | 18 rope | 574 tangled_rope | 177 snare | 9 piton | 9 scaffold
    Purity:    141 pristine | 40 sound | 88 borderline | 632 contaminated | 18 degraded
    Coupling:  712 strongly | 20 weakly | 161 independent | 1 inconclusive
    Signature: 550 false_natural_law | 233 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 686 deep | 51 moderate | 182 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_natural_law (59.8% of corpus shares this signature)
    Purity band: contaminated (68.7% of corpus in this band)
    Confidence band: borderline (19.8% of corpus in this band)
    Boundary zone: snare->tangled_rope (174 constraints share this boundary)
    Orbit Family ID:  F235

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

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
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  false_mountain_naturalization: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for false_mountain_naturalization: Claims naturality (indexed_mountain_classification) but fails Boltzmann independence test. Coupling score=1.000 with 6 coupled dimension pairs. Excess extraction=0.49999999999999994. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  false_mountain_naturalization (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.65 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 6 resolution scenario(s):

  ┌─ [naturalization_reversibility_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether internalized naturalization can be reversed within biographical time
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

  ┌─ [collective_action_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Critical mass required for denaturalization to enable collective action
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

  ┌─ [enforcement_mechanism_visibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether enforcement visibility breaks naturalization frame
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

  ┌─ [coordination_function_separability] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether coordination and extraction components can be decoupled
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

  ┌─ [intergenerational_transmission_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Primary mechanism of naturalization transmission across generations
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

  ┌─ [omega_extraction_blindness_false_mountain_naturalization] CONCEPTUAL CLARIFICATION
  │  Constraint: false_mountain_naturalization
  │  Gap: Constraint false_mountain_naturalization appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from false_mountain_naturalization?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does false_mountain_naturalization serve?
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
