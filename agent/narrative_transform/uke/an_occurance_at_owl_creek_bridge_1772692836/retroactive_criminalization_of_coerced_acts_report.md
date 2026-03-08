CORPUS CONTEXT: 976 constraints
  Types: 146 mountain, 25 rope, 596 tangled_rope, 189 snare, 9 piton, 10 scaffold
  Network stability: cascading | 808 omegas (746 critical)
  Confidence: 725 deep (74%) | 55 moderate (6%) | 195 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/retroactive_criminalization_of_coerced_acts.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: retroactive_criminalization_of_coerced_acts...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: retroactive_criminalization_of_coerced_acts

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: retroactive_criminalization_of_coerced_acts (0-15)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] retroactive_criminalization_of_coerced_acts from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] retroactive_criminalization_of_coerced_acts from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=snare, computed=snare
  [INDEX OK] retroactive_criminalization_of_coerced_acts from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] retroactive_criminalization_of_coerced_acts from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] retroactive_criminalization_of_coerced_acts from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] retroactive_criminalization_of_coerced_acts from context(agent_power(powerful),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] retroactive_criminalization_of_coerced_acts from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  retroactive_criminalization_of_coerced_acts:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,12,0.45,0.65)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,15,0.62,0.78)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.5,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.68)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  retroactive_criminalization_of_coerced_acts -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  retroactive_criminalization_of_coerced_acts (ε=0.78):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.78 × 0.65 × 0.80 = 0.406
    moderate@national: d=0.700 f(d)=1.11 χ = 0.78 × 1.11 × 1.00 = 0.863
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.78 × -0.04 × 1.00 = -0.033
    analytical@global: d=0.720 f(d)=1.14 χ = 0.78 × 1.14 × 1.20 = 1.069

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: retroactive_criminalization_of_coerced_acts ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.500 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_ci_rope
    Purity:           0.5 (borderline)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5000
    Effective purity:   0.5000
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_retroactive_criminalization_of_coerced_acts
    Severity Score:    0.814
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F467

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), snare (moderate), rope (institutional)
    Confidence:       0.0083 (borderline)
    Rival Type:       tangled_rope (P=0.9905)
    Margin:           -0.9822
    Boundary:         snare->tangled_rope
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → tangled_rope; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says snare, MaxEnt says tangled_rope
  Confidence:    0.0083 (borderline)
  Rival Type:    tangled_rope (P=0.9905)
  Margin:        -0.9822
  Entropy:       0.0321
  Distribution:  tangled_rope: 0.990, snare: 0.008, piton: 0.001

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9923)
  Entropy:       0.0254
  Distribution:  tangled_rope: 0.992, snare: 0.008, piton: 0.000

  Classical/Indexed TV Distance: 0.0018 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.84 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
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
    abductive: abductive_tension([trigger(convergent_structural_stress,0.84,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])

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

    Type:      146 mountain | 25 rope | 596 tangled_rope | 189 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 100 borderline | 653 contaminated | 19 degraded
    Coupling:  744 strongly | 21 weakly | 183 independent | 1 inconclusive
    Signature: 557 false_natural_law | 267 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 725 deep | 55 moderate | 195 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.4% of corpus shares this signature)
    Purity band: borderline (10.2% of corpus in this band)
    Confidence band: borderline (20.0% of corpus in this band)
    Boundary zone: snare->tangled_rope (186 constraints share this boundary)
    Orbit Family ID:  F467

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.42857142857142855 (low variance)
  Index Configs:       7
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 11
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 15
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for retroactive_criminalization_of_coerced_acts

[STRUCTURAL SIGNATURE ANALYSIS]
  retroactive_criminalization_of_coerced_acts: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for retroactive_criminalization_of_coerced_acts: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),excess_above_floor(0.68)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: contract_enforcement_division
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.59

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  retroactive_criminalization_of_coerced_acts (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.44 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [institutional_encouragement_legibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Evidentiary threshold for proving institutional coercion in tribunal proceedings
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

  ┌─ [operational_necessity_vs_liability_externalization] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether operational security justification is genuine or post-hoc rationalization
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

  ┌─ [debt_leverage_necessity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether debt leverage is necessary for the retroactive criminalization mechanism
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

  ┌─ [selective_enforcement_pattern] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether enforcement is selective or follows consistent criteria
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

  ┌─ [omega_extraction_blindness_retroactive_criminalization_of_coerced_acts] CONCEPTUAL CLARIFICATION
  │  Constraint: retroactive_criminalization_of_coerced_acts
  │  Gap: Constraint retroactive_criminalization_of_coerced_acts appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from retroactive_criminalization_of_coerced_acts?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does retroactive_criminalization_of_coerced_acts serve?
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
