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
[SCENARIO MANAGER] Loading: testsets/debt_trap_compounding.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: debt_trap_compounding...
  [BRIDGE] Derived has_sunset_clause(debt_trap_compounding) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: debt_trap_compounding

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: debt_trap_compounding (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] debt_trap_compounding from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX OK] debt_trap_compounding from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=snare
  [INDEX MISMATCH] debt_trap_compounding from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=snare
  [INDEX MISMATCH] debt_trap_compounding from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] debt_trap_compounding from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] debt_trap_compounding from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=piton, computed=scaffold
  [INDEX MISMATCH] debt_trap_compounding from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  debt_trap_compounding:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.45,0.68)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.58,0.78)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.63)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  debt_trap_compounding -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  debt_trap_compounding (ε=0.78):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.78 × 1.36 × 0.80 = 0.848
    moderate@national: d=0.700 f(d)=1.11 χ = 0.78 × 1.11 × 1.00 = 0.863
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.78 × -0.04 × 1.00 = -0.033
    analytical@global: d=0.720 f(d)=1.14 χ = 0.78 × 1.14 × 1.20 = 1.069

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: debt_trap_compounding ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=scaffold  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.3))
  Purity:     0.575 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        false_ci_rope
    Purity:           0.575 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5750
    Effective purity:   0.5750
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_debt_trap_compounding
    Severity Score:    0.796
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F213

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    Confidence:       0.0492 (borderline)
    Rival Type:       tangled_rope (P=0.9488)
    Margin:           -0.8996
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says snare, MaxEnt says tangled_rope
  Confidence:    0.0492 (borderline)
  Rival Type:    tangled_rope (P=0.9488)
  Margin:        -0.8996
  Entropy:       0.1174
  Distribution:  tangled_rope: 0.949, snare: 0.049, piton: 0.002

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9540)
  Entropy:       0.1044
  Distribution:  tangled_rope: 0.954, snare: 0.046, piton: 0.000

  Classical/Indexed TV Distance: 0.0052 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

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

    Type:      148 mountain | 25 rope | 600 tangled_rope | 192 snare | 9 piton | 10 scaffold
    Purity:    163 pristine | 42 sound | 102 borderline | 658 contaminated | 19 degraded
    Coupling:  750 strongly | 22 weakly | 185 independent | 1 inconclusive
    Signature: 560 false_natural_law | 271 false_ci_rope | 143 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 729 deep | 57 moderate | 198 borderline (mean: 0.752)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.5% of corpus shares this signature)
    Purity band: borderline (10.4% of corpus in this band)
    Confidence band: borderline (20.1% of corpus in this band)
    Boundary zone: snare->tangled_rope (189 constraints share this boundary)
    Orbit Family ID:  F213

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.7142857142857143 (high variance)
  Index Configs:       7
  Types Produced:      5

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 11
    Unique Type Shifts:  naturalized -> snare, naturalized -> tangled_rope, rope -> naturalized, tangled_rope -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for debt_trap_compounding

[STRUCTURAL SIGNATURE ANALYSIS]
  debt_trap_compounding: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for debt_trap_compounding: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.3),excess_above_floor(0.63)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: loan_administrators
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.56

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  debt_trap_compounding (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.88 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [interest_rate_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Interest rate threshold for distinguishing risk compensation from extraction
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

  ┌─ [wage_growth_counterfactual] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Counterfactual wage levels without institutional lending
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

  ┌─ [default_trigger_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Primary driver of default: worker behavior vs structural features
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

  ┌─ [exit_capital_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Savings threshold for exit from debt-trapped employment
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

  ┌─ [omega_extraction_blindness_debt_trap_compounding] CONCEPTUAL CLARIFICATION
  │  Constraint: debt_trap_compounding
  │  Gap: Constraint debt_trap_compounding appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from debt_trap_compounding?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does debt_trap_compounding serve?
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
