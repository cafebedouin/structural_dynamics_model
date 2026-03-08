CORPUS CONTEXT: 936 constraints
  Types: 138 mountain, 18 rope, 580 tangled_rope, 180 snare, 9 piton, 10 scaffold
  Network stability: cascading | 780 omegas (723 critical)
  Confidence: 698 deep (75%) | 52 moderate (6%) | 185 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fragmentation_as_arbitrage.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fragmentation_as_arbitrage...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: fragmentation_as_arbitrage

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fragmentation_as_arbitrage (0-30)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] fragmentation_as_arbitrage from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(global)): declared=snare, computed=tangled_rope
  [INDEX OK] fragmentation_as_arbitrage from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] fragmentation_as_arbitrage from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=rope
  [INDEX MISMATCH] fragmentation_as_arbitrage from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] fragmentation_as_arbitrage from context(agent_power(institutional),time_horizon(civilizational),exit_options(constrained),spatial_scope(continental)): declared=piton, computed=naturalized
  [INDEX OK] fragmentation_as_arbitrage from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  fragmentation_as_arbitrage:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,30,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,30,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.615,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fragmentation_as_arbitrage -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fragmentation_as_arbitrage (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fragmentation_as_arbitrage ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=high
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.615 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.615 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.4301 (genuinely_tangled)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6150
    Effective purity:   0.6150
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_fragmentation_as_arbitrage
    Severity Score:    0.516
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F257

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       0.9992 (deep)
    Rival Type:       snare (P=0.0008)
    Margin:           +0.9985
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9992 (deep)
  Rival Type:    snare (P=0.0008)
  Margin:        +0.9985
  Entropy:       0.0035
  Distribution:  tangled_rope: 0.999, snare: 0.001, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9992)
  Entropy:       0.0035
  Distribution:  tangled_rope: 0.999, snare: 0.001, piton: 0.000

  Classical/Indexed TV Distance: 0.0000 (near_zero)
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

    Type:      138 mountain | 18 rope | 580 tangled_rope | 180 snare | 9 piton | 10 scaffold
    Purity:    147 pristine | 41 sound | 90 borderline | 639 contaminated | 18 degraded
    Coupling:  719 strongly | 21 weakly | 168 independent | 1 inconclusive
    Signature: 553 false_natural_law | 239 false_ci_rope | 133 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 698 deep | 52 moderate | 185 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.5% of corpus shares this signature)
    Purity band: borderline (9.6% of corpus in this band)
    Confidence band: deep (74.7% of corpus in this band)
    Boundary zone: tangled_rope->snare (153 constraints share this boundary)
    Orbit Family ID:  F257

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
Timeline:       0 to 30
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for fragmentation_as_arbitrage

[STRUCTURAL SIGNATURE ANALYSIS]
  fragmentation_as_arbitrage: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for fragmentation_as_arbitrage: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.29),excess_above_floor(0.39999999999999997)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_mediators
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.50

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  fragmentation_as_arbitrage (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [optimal_fragmentation_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold distinguishing coordination-optimal from extraction-driven fragmentation
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

  ┌─ [technological_synthesis_substitution] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether technological synthesis reduces or relocates mediator extraction
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

  ┌─ [credentialing_necessity_vs_theater] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Signal vs noise ratio in credentialing systems
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

  ┌─ [fragmentation_intentionality] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether fragmentation is emergent or designed
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

  ┌─ [omega_extraction_blindness_fragmentation_as_arbitrage] CONCEPTUAL CLARIFICATION
  │  Constraint: fragmentation_as_arbitrage
  │  Gap: Constraint fragmentation_as_arbitrage appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from fragmentation_as_arbitrage?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does fragmentation_as_arbitrage serve?
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
