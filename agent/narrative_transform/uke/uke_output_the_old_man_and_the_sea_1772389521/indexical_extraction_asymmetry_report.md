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
[SCENARIO MANAGER] Loading: testsets/indexical_extraction_asymmetry.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: indexical_extraction_asymmetry...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: indexical_extraction_asymmetry

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: indexical_extraction_asymmetry (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] indexical_extraction_asymmetry from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=tangled_rope
  [INDEX OK] indexical_extraction_asymmetry from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] indexical_extraction_asymmetry from context(agent_power(powerful),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=naturalized
  [INDEX OK] indexical_extraction_asymmetry from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=rope, computed=rope
  [INDEX MISMATCH] indexical_extraction_asymmetry from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] indexical_extraction_asymmetry from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  indexical_extraction_asymmetry:
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.48,0.58)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3205,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  indexical_extraction_asymmetry -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  indexical_extraction_asymmetry (ε=0.58):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.58 × 1.36 × 0.80 = 0.630
    moderate@national: d=0.700 f(d)=1.11 χ = 0.58 × 1.11 × 1.00 = 0.642
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.58 × -0.04 × 1.00 = -0.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: indexical_extraction_asymmetry ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.321 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.3205 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9962 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3205
    Effective purity:   0.3195
    Propagation delta:  -0.0010

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | false_mountain_naturalization | tangled_rope | explicit | 1.00 | 0.3125 |

  Purity degraded from 0.3205 to 0.3195 by contamination from 1 neighbor(s), primarily false_mountain_naturalization (explicit, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_indexical_extraction_asymmetry
    Severity Score:    0.654
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F300

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional), snare (analytical)
    Confidence:       0.7377 (moderate)
    Rival Type:       snare (P=0.2623)
    Margin:           +0.4754
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: powerless, moderate → tangled_rope; institutional → rope; analytical → snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7377 (moderate)
  Rival Type:    snare (P=0.2623)
  Margin:        +0.4754
  Entropy:       0.3212
  Distribution:  tangled_rope: 0.738, snare: 0.262

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.7325)
  Entropy:       0.3241
  Distribution:  tangled_rope: 0.733, snare: 0.267

  Classical/Indexed TV Distance: 0.0051 (near_zero)
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

    Type:      132 mountain | 18 rope | 574 tangled_rope | 177 snare | 9 piton | 9 scaffold
    Purity:    141 pristine | 40 sound | 88 borderline | 632 contaminated | 18 degraded
    Coupling:  712 strongly | 20 weakly | 161 independent | 1 inconclusive
    Signature: 550 false_natural_law | 233 false_ci_rope | 127 natural_law | 6 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 686 deep | 51 moderate | 182 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.3% of corpus shares this signature)
    Purity band: contaminated (68.7% of corpus in this band)
    Confidence band: moderate (5.5% of corpus in this band)
    Boundary zone: tangled_rope->snare (148 constraints share this boundary)
    Orbit Family ID:  F300

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       6
  Types Produced:      3

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
  ! ALERT [informational]: perspectival_incoherence detected for indexical_extraction_asymmetry

[STRUCTURAL SIGNATURE ANALYSIS]
  indexical_extraction_asymmetry: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for indexical_extraction_asymmetry: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),scope_variant([snare,tangled_rope]),excess_above_floor(0.48),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: powerful_agents_with_exit_options
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  indexical_extraction_asymmetry (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.65 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [coordination_necessity_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Proportion of enforcement mechanism necessary for coordination
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

  ┌─ [exit_option_accessibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether exit options are endogenous or exogenous to constraint
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

  ┌─ [agency_depletion_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Reversibility of agency depletion after constraint removal
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

  ┌─ [coordination_benefit_distribution] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Distribution of coordination benefits across power positions
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

  ┌─ [omega_extraction_blindness_indexical_extraction_asymmetry] CONCEPTUAL CLARIFICATION
  │  Constraint: indexical_extraction_asymmetry
  │  Gap: Constraint indexical_extraction_asymmetry appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from indexical_extraction_asymmetry?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does indexical_extraction_asymmetry serve?
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
