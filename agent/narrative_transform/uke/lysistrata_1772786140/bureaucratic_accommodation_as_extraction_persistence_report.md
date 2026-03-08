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
[SCENARIO MANAGER] Loading: testsets/bureaucratic_accommodation_as_extraction_persistence.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: bureaucratic_accommodation_as_extraction_persistence...
  [BRIDGE] Derived has_sunset_clause(bureaucratic_accommodation_as_extraction_persistence) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: bureaucratic_accommodation_as_extraction_persistence

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: bureaucratic_accommodation_as_extraction_persistence (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=snare
  [INDEX OK] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(powerful),time_horizon(immediate),exit_options(mobile),spatial_scope(local)): declared=rope, computed=piton
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=piton
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(national)): declared=piton, computed=rope
  [INDEX MISMATCH] bureaucratic_accommodation_as_extraction_persistence from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 3 | Warning: 1 | Watch: 0

  bureaucratic_accommodation_as_extraction_persistence:
    [critical] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.55,0.78)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5800000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  bureaucratic_accommodation_as_extraction_persistence -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  bureaucratic_accommodation_as_extraction_persistence (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: bureaucratic_accommodation_as_extraction_persistence ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, drift)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.9974 (snare_leaning)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3529
    Propagation delta:  -0.0013

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | the_calm_protocol_suppression | tangled_rope | shared_beneficiary | 0.30 | 0.3205 |

  Purity degraded from 0.3542 to 0.3529 by contamination from 1 neighbor(s), primarily the_calm_protocol_suppression (shared_beneficiary, purity 0.3205).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_bureaucratic_accommodation_as_extraction_persistence
    Severity Score:    0.716
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F149

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    Confidence:       0.6136 (moderate)
    Rival Type:       snare (P=0.3828)
    Margin:           +0.2308
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.6136 (moderate)
  Rival Type:    snare (P=0.3828)
  Margin:        +0.2308
  Entropy:       0.3837
  Distribution:  tangled_rope: 0.614, snare: 0.383, piton: 0.004

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.6170)
  Entropy:       0.3748
  Distribution:  tangled_rope: 0.617, snare: 0.382, piton: 0.001

  Classical/Indexed TV Distance: 0.0034 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (5 subsystems):
    purity, dirac, fingerprint_voids, context_gap, fcr_gate

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

  Tensions (2):
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    drift: critical_drift([drift(metric_substitution,evidence(theater_delta,0,10,0.55,0.78),critical),drift(extraction_accumulation,evidence(extraction_delta,0,10,0.48,0.68),critical),drift(coupling_drift,evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.5800000000000001)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(the_calm_protocol_suppression,0.0025250000000000008,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.48)])],effective_purity,0.3529041666666667,intrinsic_purity,0.3541666666666667),warning)])

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **6 of 6 theorems active.**

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
    Confidence band: moderate (5.8% of corpus in this band)
    Boundary zone: tangled_rope->snare (170 constraints share this boundary)
    Orbit Family ID:  F149

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twin Group:
    Signature:   (0.7, 0.7, False, True)
    Group Size:  31
    Types:       snare, tangled_rope

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  piton -> snare, piton -> tangled_rope, rope -> piton, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for bureaucratic_accommodation_as_extraction_persistence

[STRUCTURAL SIGNATURE ANALYSIS]
  bureaucratic_accommodation_as_extraction_persistence: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for bureaucratic_accommodation_as_extraction_persistence: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.33),scope_variant([snare,tangled_rope]),excess_above_floor(0.5800000000000001),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: institutional_authority
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.54

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  bureaucratic_accommodation_as_extraction_persistence (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.77 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [approval_stratification_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether approval stratification reflects feasibility or institutional preference
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

  ┌─ [procedural_cost_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Procedural cost threshold distinguishing coordination from extraction
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

  ┌─ [universal_design_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether universal design sunset is structurally achievable
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

  ┌─ [architectural_substrate_mutability] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether architectural barriers are structurally necessary or institutionally maintained
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

  ┌─ [omega_extraction_blindness_bureaucratic_accommodation_as_extraction_persistence] CONCEPTUAL CLARIFICATION
  │  Constraint: bureaucratic_accommodation_as_extraction_persistence
  │  Gap: Constraint bureaucratic_accommodation_as_extraction_persistence appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from bureaucratic_accommodation_as_extraction_persistence?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does bureaucratic_accommodation_as_extraction_persistence serve?
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
