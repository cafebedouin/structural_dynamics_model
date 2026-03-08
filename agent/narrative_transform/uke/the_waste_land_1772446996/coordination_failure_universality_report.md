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
[SCENARIO MANAGER] Loading: testsets/coordination_failure_universality.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: coordination_failure_universality...
  [BRIDGE] Derived constraint_beneficiary(coordination_failure_universality, inferred_institutional) from metrics (E=0.68, S=0.72)
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: coordination_failure_universality

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: coordination_failure_universality (0-15)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] coordination_failure_universality from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=snare, computed=naturalized
  [INDEX OK] coordination_failure_universality from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=snare, computed=snare
  [INDEX MISMATCH] coordination_failure_universality from context(agent_power(powerful),time_horizon(biographical),exit_options(mobile),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] coordination_failure_universality from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=snare, computed=naturalized
  [INDEX MISMATCH] coordination_failure_universality from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(global)): declared=snare, computed=rope
  [INDEX OK] coordination_failure_universality from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=snare, computed=snare
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  coordination_failure_universality:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,15,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,15,0.48,0.68)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3125,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.53)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  coordination_failure_universality -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  coordination_failure_universality (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: coordination_failure_universality ===
  Shift (computed via dr_type/3):
    powerless=naturalized  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.312 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Signature:        constructed_high_extraction
    Purity:           0.3125 (contaminated)
    Coupling:         nonsensically_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3125
    Effective purity:   0.3125
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [naturalized, rope, snare]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       naturalized (powerless), snare (moderate), rope (institutional)
    Confidence:       0.9546 (deep)
    Rival Type:       tangled_rope (P=0.0454)
    Margin:           +0.9092
    Boundary:         snare->tangled_rope
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → naturalized; institutional → rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9546 (deep)
  Rival Type:    tangled_rope (P=0.0454)
  Margin:        +0.9092
  Entropy:       0.1032
  Distribution:  snare: 0.955, tangled_rope: 0.045, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9541)
  Entropy:       0.1040
  Distribution:  snare: 0.954, tangled_rope: 0.046, piton: 0.000

  Classical/Indexed TV Distance: 0.0005 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (3):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)

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

    Type:      138 mountain | 18 rope | 580 tangled_rope | 180 snare | 9 piton | 10 scaffold
    Purity:    147 pristine | 41 sound | 90 borderline | 639 contaminated | 18 degraded
    Coupling:  719 strongly | 21 weakly | 168 independent | 1 inconclusive
    Signature: 553 false_natural_law | 239 false_ci_rope | 133 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 698 deep | 52 moderate | 185 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a constructed_high_extraction (0.7% of corpus shares this signature)
    Purity band: contaminated (68.3% of corpus in this band)
    Confidence band: deep (74.7% of corpus in this band)
    Boundary zone: snare->tangled_rope (177 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.16666666666666666 (low variance)
  Index Configs:       6
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:
    Missed Transitions: 13
    Unique Type Shifts:  naturalized -> snare, naturalized -> unknown, rope -> naturalized, unknown -> snare

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 15
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for coordination_failure_universality

[STRUCTURAL SIGNATURE ANALYSIS]
  coordination_failure_universality: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for coordination_failure_universality: Enforcement present (suppression=0.72, resistance=0.50) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa) at Tn: 0.55

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [coordination_necessity_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where coordination necessity suppresses extraction visibility
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

  ┌─ [beneficiary_existence_ambiguity] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether universal extraction is structurally possible or empirically rare
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

  ┌─ [extraction_measurement_frame] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether universal extraction is measurement artifact or structural reality
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

  ┌─ [temporal_beneficiary_emergence] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether beneficiaries emerge as coordination mechanism matures
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

  ┌─ [coordination_function_illusion] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether coordination function is genuine or theatrical
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

====================================================
