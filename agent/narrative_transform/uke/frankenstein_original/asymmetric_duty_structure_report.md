CORPUS CONTEXT: 942 constraints
  Types: 139 mountain, 18 rope, 583 tangled_rope, 182 snare, 9 piton, 10 scaffold
  Network stability: cascading | 785 omegas (728 critical)
  Confidence: 705 deep (75%) | 49 moderate (5%) | 187 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/asymmetric_duty_structure.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: asymmetric_duty_structure...
  [BRIDGE] Derived has_sunset_clause(asymmetric_duty_structure) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: asymmetric_duty_structure

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: asymmetric_duty_structure (0-9)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] asymmetric_duty_structure from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] asymmetric_duty_structure from context(agent_power(moderate),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] asymmetric_duty_structure from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(regional)): declared=rope, computed=scaffold
  [INDEX MISMATCH] asymmetric_duty_structure from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] asymmetric_duty_structure from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] asymmetric_duty_structure from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=piton, computed=scaffold
  [INDEX MISMATCH] asymmetric_duty_structure from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  asymmetric_duty_structure:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,9,0.42,0.68)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,9,0.48,0.64)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.39583333333333337,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.56)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  asymmetric_duty_structure -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  asymmetric_duty_structure (ε=0.64):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.64 × 1.36 × 0.80 = 0.696
    moderate@national: d=0.200 f(d)=0.04 χ = 0.64 × 0.04 × 1.00 = 0.026
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.64 × -0.04 × 1.00 = -0.027
    analytical@global: d=0.720 f(d)=1.14 χ = 0.64 × 1.14 × 1.20 = 0.877

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: asymmetric_duty_structure ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=scaffold  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=concentrated  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.396 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.508333 (borderline)
    Coupling:         strongly_coupled (score: 0.625)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.9974 (snare_leaning)
    Coalition:        split_field

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5083
    Effective purity:   0.5083
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, snare]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_asymmetric_duty_structure
    Severity Score:    0.696
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F119

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), naturalized (moderate), rope (institutional)
    Confidence:       0.6219 (moderate)
    Rival Type:       snare (P=0.3780)
    Margin:           +0.2439
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: powerless, analytical → snare; moderate → naturalized; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.6219 (moderate)
  Rival Type:    snare (P=0.3780)
  Margin:        +0.2439
  Entropy:       0.3706
  Distribution:  tangled_rope: 0.622, snare: 0.378, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.6198)
  Entropy:       0.3709
  Distribution:  tangled_rope: 0.620, snare: 0.380, piton: 0.000

  Classical/Indexed TV Distance: 0.0021 (near_zero)
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

    Type:      139 mountain | 18 rope | 583 tangled_rope | 182 snare | 9 piton | 10 scaffold
    Purity:    148 pristine | 41 sound | 93 borderline | 641 contaminated | 18 degraded
    Coupling:  724 strongly | 21 weakly | 169 independent | 1 inconclusive
    Signature: 554 false_natural_law | 243 false_ci_rope | 134 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 705 deep | 49 moderate | 187 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (25.8% of corpus shares this signature)
    Purity band: borderline (9.9% of corpus in this band)
    Confidence band: moderate (5.2% of corpus in this band)
    Boundary zone: tangled_rope->snare (155 constraints share this boundary)
    Orbit Family ID:  F119

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 9
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for asymmetric_duty_structure

[STRUCTURAL SIGNATURE ANALYSIS]
  asymmetric_duty_structure: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for asymmetric_duty_structure: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([snare,tangled_rope]),excess_above_floor(0.56),nonsensical_coupling(0.16666666666666666)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: duty_bearer_ego_protection
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.55

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  asymmetric_duty_structure (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.72 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [duty_fulfillment_measurement] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold between duty fulfillment and duty performance theater
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

  ┌─ [identity_lock_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Timeline for duty-bearer identity lock dissolution
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

  ┌─ [suppression_mechanism_ratio] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Structural vs internalized suppression mechanism ratio
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

  ┌─ [care_ethics_sunset_timeline] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Timeline and likelihood of care ethics norm shift
  │
  │  NOTE: Not resolvable via data or logic alone
  │
  │  RESOLUTION STRATEGY:
  │  1. Document competing value frameworks:
  │     - What values support current unknown?
  │     - What values oppose it?
  │     - Are these incommensurable?
  │  2. Propose scaffolded solution:
  │     - Design mechanism respecting both value sets
  │     - Create exit options for dissenters
  │     - Allow preference-based sorting
  │  3. Accept unresolvability if necessary:
  │     - Some omegas represent genuine value pluralism
  │     - Solution: coexistence, not consensus
  └─

  ┌─ [omega_extraction_blindness_asymmetric_duty_structure] CONCEPTUAL CLARIFICATION
  │  Constraint: asymmetric_duty_structure
  │  Gap: Constraint asymmetric_duty_structure appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from asymmetric_duty_structure?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does asymmetric_duty_structure serve?
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
