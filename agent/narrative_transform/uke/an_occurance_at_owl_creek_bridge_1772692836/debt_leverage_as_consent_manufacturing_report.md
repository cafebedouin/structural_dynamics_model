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
[SCENARIO MANAGER] Loading: testsets/debt_leverage_as_consent_manufacturing.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: debt_leverage_as_consent_manufacturing...
  [BRIDGE] Derived has_sunset_clause(debt_leverage_as_consent_manufacturing) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: debt_leverage_as_consent_manufacturing

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: debt_leverage_as_consent_manufacturing (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] debt_leverage_as_consent_manufacturing from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(local)): declared=snare, computed=scaffold
  [INDEX MISMATCH] debt_leverage_as_consent_manufacturing from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] debt_leverage_as_consent_manufacturing from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] debt_leverage_as_consent_manufacturing from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] debt_leverage_as_consent_manufacturing from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] debt_leverage_as_consent_manufacturing from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=piton, computed=scaffold
  [INDEX OK] debt_leverage_as_consent_manufacturing from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 0 | Warning: 4 | Watch: 0

  debt_leverage_as_consent_manufacturing:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.58)
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.28,0.38)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4205,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.23)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  debt_leverage_as_consent_manufacturing -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  debt_leverage_as_consent_manufacturing (ε=0.38):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.38 × 1.36 × 0.80 = 0.413
    moderate@national: d=0.700 f(d)=1.11 χ = 0.38 × 1.11 × 1.00 = 0.420
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.38 × -0.04 × 1.00 = -0.016
    analytical@global: d=0.720 f(d)=1.14 χ = 0.38 × 1.14 × 1.20 = 0.521

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: debt_leverage_as_consent_manufacturing ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=scaffold  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.420 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.462167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     5 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift, network_drift
    Tangled psi:      0.5485 (genuinely_tangled)
    Coalition:        institutional_dissent

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4622
    Effective purity:   0.4023
    Propagation delta:  -0.0599

    Network neighbors (4):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | employer_sponsored_healthcare | unknown | explicit | 1.00 | N/A |
    | informant_recruitment_through_false_solidarity | snare | explicit | 1.00 | 0.3125 |
    | non_compete_enforcement | unknown | explicit | 1.00 | N/A |
    | occupational_licensing_barriers | unknown | explicit | 1.00 | N/A |

  Purity degraded from 0.4622 to 0.4023 by contamination from 4 neighbor(s), primarily informant_recruitment_through_false_solidarity (explicit, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_debt_leverage_as_consent_manufacturing
    Severity Score:    0.496
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F208

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       0.9986 (deep)
    Rival Type:       snare (P=0.0013)
    Margin:           +0.9973
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9986 (deep)
  Rival Type:    snare (P=0.0013)
  Margin:        +0.9973
  Entropy:       0.0061
  Distribution:  tangled_rope: 0.999, snare: 0.001, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9987)
  Entropy:       0.0055
  Distribution:  tangled_rope: 0.999, snare: 0.001, piton: 0.000

  Classical/Indexed TV Distance: 0.0001 (near_zero)
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

    Type:      146 mountain | 25 rope | 596 tangled_rope | 189 snare | 9 piton | 10 scaffold
    Purity:    161 pristine | 42 sound | 100 borderline | 653 contaminated | 19 degraded
    Coupling:  744 strongly | 21 weakly | 183 independent | 1 inconclusive
    Signature: 557 false_natural_law | 267 false_ci_rope | 141 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 725 deep | 55 moderate | 195 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.4% of corpus shares this signature)
    Purity band: contaminated (66.9% of corpus in this band)
    Confidence band: deep (74.4% of corpus in this band)
    Boundary zone: tangled_rope->snare (167 constraints share this boundary)
    Orbit Family ID:  F208

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 8
    Unique Type Shifts:  rope -> tangled_rope, rope -> unknown, unknown -> tangled_rope

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for debt_leverage_as_consent_manufacturing

[STRUCTURAL SIGNATURE ANALYSIS]
  debt_leverage_as_consent_manufacturing: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for debt_leverage_as_consent_manufacturing: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.3),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.23),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: employing_institution
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.52

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  debt_leverage_as_consent_manufacturing (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.43 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [liquidity_alternative_sufficiency] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether alternative liquidity mechanisms can replace signing bonuses without household welfare loss
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

  ┌─ [identity_lock_prevalence] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Distribution of binding mechanisms across worker population
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

  ┌─ [household_dependency_causality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether household dependency is cause, selection criterion, or both
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

  ┌─ [consent_authenticity_threshold] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Debt-to-income threshold distinguishing manufactured consent from coercion
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

  ┌─ [omega_extraction_blindness_debt_leverage_as_consent_manufacturing] CONCEPTUAL CLARIFICATION
  │  Constraint: debt_leverage_as_consent_manufacturing
  │  Gap: Constraint debt_leverage_as_consent_manufacturing appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from debt_leverage_as_consent_manufacturing?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does debt_leverage_as_consent_manufacturing serve?
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
