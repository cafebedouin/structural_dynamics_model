CORPUS CONTEXT: 962 constraints
  Types: 143 mountain, 21 rope, 590 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 798 omegas (740 critical)
  Confidence: 715 deep (74%) | 53 moderate (6%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/protocol_rigidity_under_unclassified_variance.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: protocol_rigidity_under_unclassified_variance...
  [BRIDGE] Derived has_sunset_clause(protocol_rigidity_under_unclassified_variance) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: protocol_rigidity_under_unclassified_variance

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: protocol_rigidity_under_unclassified_variance (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] protocol_rigidity_under_unclassified_variance from context(agent_power(institutional),time_horizon(biographical),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] protocol_rigidity_under_unclassified_variance from context(agent_power(moderate),time_horizon(immediate),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] protocol_rigidity_under_unclassified_variance from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] protocol_rigidity_under_unclassified_variance from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=scaffold, computed=tangled_rope
  [INDEX OK] protocol_rigidity_under_unclassified_variance from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] protocol_rigidity_under_unclassified_variance from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 1 | Watch: 1

  protocol_rigidity_under_unclassified_variance:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.18,0.22)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.92,decline_signals,[theater_rising,excess_above_floor(0.2)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  protocol_rigidity_under_unclassified_variance -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  protocol_rigidity_under_unclassified_variance (ε=0.22):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.22 × 1.39 × 0.80 = 0.245
    moderate@national: d=0.650 f(d)=1.01 χ = 0.22 × 1.01 × 1.00 = 0.222
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.22 × -0.04 × 1.00 = -0.009
    analytical@global: d=0.720 f(d)=1.14 χ = 0.22 × 1.14 × 1.20 = 0.301

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: protocol_rigidity_under_unclassified_variance ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=rising
  Zone:       extraction=negligible  suppression=moderate
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.920 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.92 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     3 — extraction_accumulation, purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9200
    Effective purity:   0.8471
    Propagation delta:  -0.0729

    Network neighbors (1):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | documentation_fidelity_collapse | snare | shared_beneficiary | 0.30 | 0.3125 |

  Purity degraded from 0.9200 to 0.8471 by contamination from 1 neighbor(s), primarily documentation_fidelity_collapse (shared_beneficiary, purity 0.3125).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.9954 (deep)
    Rival Type:       scaffold (P=0.0041)
    Margin:           +0.9913
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9954 (deep)
  Rival Type:    scaffold (P=0.0041)
  Margin:        +0.9913
  Entropy:       0.0173
  Distribution:  rope: 0.995, scaffold: 0.004, tangled_rope: 0.001

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9964)
  Entropy:       0.0140
  Distribution:  rope: 0.996, scaffold: 0.003, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0010 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    cohomology, signature, boltzmann, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

═══ LEVEL 3: CORPUS POSITIONING ═══


--- CORPUS DISTRIBUTION ---

    Type:      143 mountain | 21 rope | 590 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    155 pristine | 41 sound | 98 borderline | 649 contaminated | 18 degraded
    Coupling:  737 strongly | 21 weakly | 176 independent | 1 inconclusive
    Signature: 556 false_natural_law | 257 false_ci_rope | 138 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 715 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.7% of corpus shares this signature)
    Purity band: pristine (16.1% of corpus in this band)
    Confidence band: deep (74.4% of corpus in this band)
    Boundary zone: rope->scaffold (10 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.5 (low variance)
  Index Configs:       6
  Types Produced:      3

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  protocol_rigidity_under_unclassified_variance: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for protocol_rigidity_under_unclassified_variance: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.2)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: protocol_designers
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.47

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [schema_revision_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Optimal threshold for triggering protocol revision
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

  ┌─ [adaptive_system_maturity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Maturity timeline for adaptive classification systems
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

  ┌─ [unclassified_variance_distribution] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Distribution pattern of unclassified variance
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
