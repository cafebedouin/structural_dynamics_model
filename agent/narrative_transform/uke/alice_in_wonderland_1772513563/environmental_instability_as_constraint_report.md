CORPUS CONTEXT: 945 constraints
  Types: 140 mountain, 18 rope, 584 tangled_rope, 183 snare, 9 piton, 10 scaffold
  Network stability: cascading | 787 omegas (730 critical)
  Confidence: 707 deep (75%) | 49 moderate (5%) | 188 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/environmental_instability_as_constraint.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: environmental_instability_as_constraint...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: environmental_instability_as_constraint

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: environmental_instability_as_constraint (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] environmental_instability_as_constraint from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] environmental_instability_as_constraint from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] environmental_instability_as_constraint from context(agent_power(powerful),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] environmental_instability_as_constraint from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] environmental_instability_as_constraint from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  environmental_instability_as_constraint:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  environmental_instability_as_constraint (ε=0.18):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.18 × 1.42 × 0.80 = 0.204
    moderate@national: d=0.646 f(d)=1.00 χ = 0.18 × 1.00 × 1.00 = 0.180
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.18 × -0.12 × 1.00 = -0.021
    analytical@global: d=0.725 f(d)=1.15 χ = 0.18 × 1.15 × 1.20 = 0.248

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: environmental_instability_as_constraint ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.936 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.936 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9360
    Effective purity:   0.9360
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

    Type:      140 mountain | 18 rope | 584 tangled_rope | 183 snare | 9 piton | 10 scaffold
    Purity:    149 pristine | 41 sound | 94 borderline | 642 contaminated | 18 degraded
    Coupling:  726 strongly | 21 weakly | 170 independent | 1 inconclusive
    Signature: 555 false_natural_law | 244 false_ci_rope | 135 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 707 deep | 49 moderate | 188 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.3% of corpus shares this signature)
    Purity band: pristine (15.8% of corpus in this band)
    Confidence band: deep (74.9% of corpus in this band)
    Boundary zone: mountain->rope (140 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.2 (low variance)
  Index Configs:       5
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for environmental_instability_as_constraint

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  environmental_instability_as_constraint: natural_law (confidence: low)
    → NATURAL LAW signature for environmental_instability_as_constraint: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.03, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [instability_source_decomposition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether instability is natural or artificially induced
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

  ┌─ [time_horizon_compression_mechanism] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether instability compresses time horizons or reveals existing bias
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

  ┌─ [exit_option_recognition_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Stability threshold for exit option recognition
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
