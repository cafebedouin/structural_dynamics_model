CORPUS CONTEXT: 970 constraints
  Types: 145 mountain, 24 rope, 593 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 803 omegas (741 critical)
  Confidence: 723 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/measurement_apparatus_bidirectionality.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: measurement_apparatus_bidirectionality...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: measurement_apparatus_bidirectionality

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: measurement_apparatus_bidirectionality (0-100)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] measurement_apparatus_bidirectionality from context(agent_power(powerful),time_horizon(immediate),exit_options(arbitrage),spatial_scope(continental)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  measurement_apparatus_bidirectionality:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  measurement_apparatus_bidirectionality (ε=0.08):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.08 × 1.42 × 0.80 = 0.091
    moderate@national: d=0.646 f(d)=1.00 χ = 0.08 × 1.00 × 1.00 = 0.080
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.08 × -0.12 × 1.00 = -0.010
    analytical@global: d=0.725 f(d)=1.15 χ = 0.08 × 1.15 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: measurement_apparatus_bidirectionality ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.976 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.976 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9760
    Effective purity:   0.9760
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [mountain]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       mountain (powerless)
    Confidence:       0.9500 (deep)
    Rival Type:       rope (P=0.0100)
    Margin:           +0.9400
    Boundary:         mountain->rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9500 (deep)
  Rival Type:    rope (P=0.0100)
  Margin:        +0.9400
  Entropy:       0.1557
  Distribution:  mountain: 0.950, rope: 0.010, tangled_rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      mountain (P=0.9500)
  Entropy:       0.1557
  Distribution:  mountain: 0.950, rope: 0.010, tangled_rope: 0.010

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (10 subsystems):
    maxent, cohomology, signature, boltzmann, purity, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts (1):
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      145 mountain | 24 rope | 593 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    160 pristine | 42 sound | 99 borderline | 650 contaminated | 18 degraded
    Coupling:  739 strongly | 21 weakly | 182 independent | 1 inconclusive
    Signature: 557 false_natural_law | 262 false_ci_rope | 140 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 723 deep | 53 moderate | 193 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.4% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: mountain->rope (145 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.16666666666666666 (low variance)
  Index Configs:       6
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 100
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for measurement_apparatus_bidirectionality

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  measurement_apparatus_bidirectionality: natural_law (confidence: low)
    → NATURAL LAW signature for measurement_apparatus_bidirectionality: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.02, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.39

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [contact_duration_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Duration threshold for structurally significant bidirectionality
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

  ┌─ [substrate_coupling_mechanism] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether bidirectionality is universal or technology-contingent
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

  ┌─ [observer_awareness_necessity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether observer awareness is necessary for bidirectionality
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
