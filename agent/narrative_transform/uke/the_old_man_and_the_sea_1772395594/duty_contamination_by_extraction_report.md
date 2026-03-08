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
[SCENARIO MANAGER] Loading: testsets/duty_contamination_by_extraction.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: duty_contamination_by_extraction...
  [BRIDGE] Derived has_sunset_clause(duty_contamination_by_extraction) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: duty_contamination_by_extraction

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: duty_contamination_by_extraction (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] duty_contamination_by_extraction from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] duty_contamination_by_extraction from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] duty_contamination_by_extraction from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] duty_contamination_by_extraction from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] duty_contamination_by_extraction from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=scaffold, computed=scaffold
  [INDEX OK] duty_contamination_by_extraction from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  duty_contamination_by_extraction:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.15,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3941666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  duty_contamination_by_extraction -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  duty_contamination_by_extraction (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.220 f(d)=0.07 χ = 0.48 × 0.07 × 1.00 = 0.032
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: duty_contamination_by_extraction ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=high
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
    Purity:           0.84 (sound)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     3 — extraction_accumulation, purity_drift, network_drift
    Tangled psi:      0.3639 (genuinely_tangled)
    Coalition:        other

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.8400
    Effective purity:   0.6457
    Propagation delta:  -0.1943

    Network neighbors (6):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | agentive_optimism_2026 | snare | shared_beneficiary | 0.30 | 0.4294 |
    | bayes_theorem | tangled_rope | shared_beneficiary | 0.30 | 0.3335 |
    | digital_credentialing_verification | tangled_rope | shared_beneficiary | 0.30 | 0.3268 |
    | false_mountain_naturalization | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |
    | harry_potter_liberalism | tangled_rope | shared_beneficiary | 0.30 | 0.4967 |
    | publishing_embargo | tangled_rope | shared_beneficiary | 0.30 | 0.3125 |

  Purity degraded from 0.8400 to 0.6457 by contamination from 6 neighbor(s), primarily false_mountain_naturalization (shared_beneficiary, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_duty_contamination_by_extraction
    Severity Score:    0.396
    Gap Class:         analytical_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F212

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), naturalized (institutional)
    Confidence:       0.9993 (deep)
    Rival Type:       snare (P=0.0006)
    Margin:           +0.9988
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees naturalized while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9993 (deep)
  Rival Type:    snare (P=0.0006)
  Margin:        +0.9988
  Entropy:       0.0032
  Distribution:  tangled_rope: 0.999, snare: 0.001, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9994)
  Entropy:       0.0029
  Distribution:  tangled_rope: 0.999, snare: 0.001

  Classical/Indexed TV Distance: 0.0001 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      132 mountain | 18 rope | 576 tangled_rope | 177 snare | 9 piton | 10 scaffold
    Purity:    141 pristine | 41 sound | 88 borderline | 634 contaminated | 18 degraded
    Coupling:  714 strongly | 20 weakly | 162 independent | 1 inconclusive
    Signature: 551 false_natural_law | 235 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 688 deep | 51 moderate | 183 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.5% of corpus shares this signature)
    Purity band: sound (4.4% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: tangled_rope->snare (150 constraints share this boundary)
    Orbit Family ID:  F212

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.6666666666666666 (high variance)
  Index Configs:       6
  Types Produced:      4

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 10
    Unique Type Shifts:  naturalized -> tangled_rope, rope -> naturalized

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for duty_contamination_by_extraction

[STRUCTURAL SIGNATURE ANALYSIS]
  duty_contamination_by_extraction: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for duty_contamination_by_extraction: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.39999999999999997),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: enforcers_of_contaminating_system
    → Institutional d=0.220

Aggregate Magnitude (Kappa) at Tn: 0.50

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  duty_contamination_by_extraction (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.49 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [intrinsic_vs_effective_purity_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Contamination threshold for reciprocity-to-extraction shift
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

  ┌─ [workaround_as_resistance_or_adaptation] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether workarounds resist or stabilize contamination
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

  ┌─ [contamination_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether contamination causes permanent norm degradation
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

  ┌─ [identity_lock_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether identity lock precedes or follows contamination
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

  ┌─ [omega_extraction_blindness_duty_contamination_by_extraction] CONCEPTUAL CLARIFICATION
  │  Constraint: duty_contamination_by_extraction
  │  Gap: Constraint duty_contamination_by_extraction appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from duty_contamination_by_extraction?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does duty_contamination_by_extraction serve?
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
