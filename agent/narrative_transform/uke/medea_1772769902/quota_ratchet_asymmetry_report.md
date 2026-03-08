CORPUS CONTEXT: 985 constraints
  Types: 148 mountain, 25 rope, 600 tangled_rope, 192 snare, 9 piton, 10 scaffold
  Network stability: cascading | 815 omegas (753 critical)
  Confidence: 729 deep (74%) | 57 moderate (6%) | 198 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/quota_ratchet_asymmetry.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: quota_ratchet_asymmetry...
  [BRIDGE] Derived has_sunset_clause(quota_ratchet_asymmetry) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: quota_ratchet_asymmetry

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: quota_ratchet_asymmetry (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] quota_ratchet_asymmetry from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX MISMATCH] quota_ratchet_asymmetry from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] quota_ratchet_asymmetry from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] quota_ratchet_asymmetry from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] quota_ratchet_asymmetry from context(agent_power(institutional),time_horizon(civilizational),exit_options(mobile),spatial_scope(global)): declared=piton, computed=scaffold
  [INDEX MISMATCH] quota_ratchet_asymmetry from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  quota_ratchet_asymmetry:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.52)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.38,0.58)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3405,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.42999999999999994)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  quota_ratchet_asymmetry -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  quota_ratchet_asymmetry (ε=0.58):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.58 × 0.65 × 0.80 = 0.302
    moderate@national: d=0.580 f(d)=0.85 χ = 0.58 × 0.85 × 1.00 = 0.493
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.58 × -0.04 × 1.00 = -0.025
    analytical@global: d=0.720 f(d)=1.14 χ = 0.58 × 1.14 × 1.20 = 0.795

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: quota_ratchet_asymmetry ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.341 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.3405 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9964 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3405
    Effective purity:   0.3405
    Propagation delta:  +0.0000

    Network neighbors (3):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | gig_economy_rating_systems | unknown | explicit | 1.00 | N/A |
    | platform_algorithmic_management | unknown | explicit | 1.00 | N/A |
    | warehouse_productivity_tracking | unknown | explicit | 1.00 | N/A |

  No significant contamination — purity unchanged across 3 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, snare, tangled_rope]
  Orbit Span:         4
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_quota_ratchet_asymmetry
    Severity Score:    0.654
    Gap Class:         powerless_blind
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F465

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), tangled_rope (moderate), rope (institutional), snare (analytical)
    Confidence:       0.7249 (moderate)
    Rival Type:       snare (P=0.2751)
    Margin:           +0.4498
    Boundary:         tangled_rope->snare
    H^1 band:         6 — Maximally fractured — all 4 observers disagree: powerless → naturalized; moderate → tangled_rope; institutional → rope; analytical → snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.7249 (moderate)
  Rival Type:    snare (P=0.2751)
  Margin:        +0.4498
  Entropy:       0.3283
  Distribution:  tangled_rope: 0.725, snare: 0.275

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.7202)
  Entropy:       0.3308
  Distribution:  tangled_rope: 0.720, snare: 0.280

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

    Type:      148 mountain | 25 rope | 600 tangled_rope | 192 snare | 9 piton | 10 scaffold
    Purity:    163 pristine | 42 sound | 102 borderline | 658 contaminated | 19 degraded
    Coupling:  750 strongly | 22 weakly | 185 independent | 1 inconclusive
    Signature: 560 false_natural_law | 271 false_ci_rope | 143 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 729 deep | 57 moderate | 198 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.5% of corpus shares this signature)
    Purity band: contaminated (66.8% of corpus in this band)
    Confidence band: moderate (5.8% of corpus in this band)
    Boundary zone: tangled_rope->snare (169 constraints share this boundary)
    Orbit Family ID:  F465

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
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for quota_ratchet_asymmetry

[STRUCTURAL SIGNATURE ANALYSIS]
  quota_ratchet_asymmetry: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for quota_ratchet_asymmetry: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([snare,tangled_rope]),excess_above_floor(0.42999999999999994),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_budget_managers
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.53

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  quota_ratchet_asymmetry (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.33 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [capacity_measurement_ambiguity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: What decline signals organizations recognize as legitimate quota adjustment triggers
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

  ┌─ [efficiency_attribution_problem] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether quota increases capture efficiency gains or extract effort intensification
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

  ┌─ [exit_option_variance] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: How worker exit capacity varies across contexts
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

  ┌─ [bidirectional_adjustment_feasibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether symmetric adjustment protocols can maintain coordination function
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

  ┌─ [omega_extraction_blindness_quota_ratchet_asymmetry] CONCEPTUAL CLARIFICATION
  │  Constraint: quota_ratchet_asymmetry
  │  Gap: Constraint quota_ratchet_asymmetry appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from quota_ratchet_asymmetry?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does quota_ratchet_asymmetry serve?
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
