CORPUS CONTEXT: 945 constraints
  Types: 140 mountain, 18 rope, 584 tangled_rope, 183 snare, 9 piton, 10 scaffold
  Network stability: cascading | 787 omegas (730 critical)
  Confidence: 707 deep (75%) | 49 moderate (5%) | 188 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/exclusionary_coordination_asymmetry.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: exclusionary_coordination_asymmetry...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: exclusionary_coordination_asymmetry

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: exclusionary_coordination_asymmetry (0-15)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] exclusionary_coordination_asymmetry from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] exclusionary_coordination_asymmetry from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] exclusionary_coordination_asymmetry from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] exclusionary_coordination_asymmetry from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX OK] exclusionary_coordination_asymmetry from context(agent_power(powerful),time_horizon(biographical),exit_options(arbitrage),spatial_scope(regional)): declared=rope, computed=rope
  [INDEX OK] exclusionary_coordination_asymmetry from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  exclusionary_coordination_asymmetry:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,12,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,15,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4983333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  exclusionary_coordination_asymmetry -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  exclusionary_coordination_asymmetry (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: exclusionary_coordination_asymmetry ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.498 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.498333 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9674 (snare_leaning)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4983
    Effective purity:   0.4983
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_exclusionary_coordination_asymmetry
    Severity Score:    0.546
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F241

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       0.9703 (deep)
    Rival Type:       snare (P=0.0297)
    Margin:           +0.9406
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9703 (deep)
  Rival Type:    snare (P=0.0297)
  Margin:        +0.9406
  Entropy:       0.0747
  Distribution:  tangled_rope: 0.970, snare: 0.030, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9698)
  Entropy:       0.0757
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

    Type:      140 mountain | 18 rope | 584 tangled_rope | 183 snare | 9 piton | 10 scaffold
    Purity:    149 pristine | 41 sound | 94 borderline | 642 contaminated | 18 degraded
    Coupling:  726 strongly | 21 weakly | 170 independent | 1 inconclusive
    Signature: 555 false_natural_law | 244 false_ci_rope | 135 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 707 deep | 49 moderate | 188 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.8% of corpus shares this signature)
    Purity band: contaminated (67.9% of corpus in this band)
    Confidence band: deep (74.9% of corpus in this band)
    Boundary zone: tangled_rope->snare (156 constraints share this boundary)
    Orbit Family ID:  F241

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       6
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 14
    Unique Type Shifts:  naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 15
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for exclusionary_coordination_asymmetry

[STRUCTURAL SIGNATURE ANALYSIS]
  exclusionary_coordination_asymmetry: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for exclusionary_coordination_asymmetry: Appears to be rope (indexed_rope_classification) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),excess_above_floor(0.39999999999999997),nonsensical_coupling(0.16666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: powerful_insiders
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  exclusionary_coordination_asymmetry (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [coordination_necessity_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold distinguishing necessary coordination barriers from extractive exclusion
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

  ┌─ [alternative_coordination_viability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether non-exclusionary coordination can match exclusionary system efficiency
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

  ┌─ [insider_naturalization_mechanism] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether insider Rope classification reflects genuine experience or naturalized extraction
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

  ┌─ [suppression_mechanism_decomposition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Decomposition of suppression into structural vs internalized components
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

  ┌─ [omega_extraction_blindness_exclusionary_coordination_asymmetry] CONCEPTUAL CLARIFICATION
  │  Constraint: exclusionary_coordination_asymmetry
  │  Gap: Constraint exclusionary_coordination_asymmetry appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from exclusionary_coordination_asymmetry?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does exclusionary_coordination_asymmetry serve?
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
