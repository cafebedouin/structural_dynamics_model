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
[SCENARIO MANAGER] Loading: testsets/categorical_instrument_blindness.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: categorical_instrument_blindness...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: categorical_instrument_blindness

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: categorical_instrument_blindness (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(institutional),time_horizon(biographical),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(powerful),time_horizon(biographical),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(analytical),time_horizon(generational),exit_options(analytical),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_instrument_blindness from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  categorical_instrument_blindness:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.15,0.18)
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  categorical_instrument_blindness -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  categorical_instrument_blindness (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: categorical_instrument_blindness ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.936 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.936 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     3 — extraction_accumulation, purity_drift, network_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9360
    Effective purity:   0.8556
    Propagation delta:  -0.0804

    Network neighbors (2):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | documentation_fidelity_collapse | snare | shared_beneficiary | 0.30 | 0.3125 |
    | protocol_rigidity_under_unclassified_variance | tangled_rope | shared_beneficiary | 0.30 | 0.8432 |

  Purity degraded from 0.9360 to 0.8556 by contamination from 2 neighbor(s), primarily documentation_fidelity_collapse (shared_beneficiary, purity 0.3125).

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
    Confidence:       0.9609 (deep)
    Rival Type:       scaffold (P=0.0391)
    Margin:           +0.9218
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9609 (deep)
  Rival Type:    scaffold (P=0.0391)
  Margin:        +0.9218
  Entropy:       0.0921
  Distribution:  rope: 0.961, scaffold: 0.039, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9825)
  Entropy:       0.0492
  Distribution:  rope: 0.982, scaffold: 0.018, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0216 (moderate)
  Moderate divergence — observer-dependence shifts probabilistic weights without changing the top classification.

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

    Type:      145 mountain | 24 rope | 593 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    160 pristine | 42 sound | 99 borderline | 650 contaminated | 18 degraded
    Coupling:  739 strongly | 21 weakly | 182 independent | 1 inconclusive
    Signature: 557 false_natural_law | 262 false_ci_rope | 140 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 723 deep | 53 moderate | 193 borderline (mean: 0.754)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (27.0% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.6% of corpus in this band)
    Boundary zone: rope->scaffold (13 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.3333333333333333 (low variance)
  Index Configs:       6
  Types Produced:      2

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  categorical_instrument_blindness: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for categorical_instrument_blindness: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.16)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: administrative_systems
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.45

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [substrate_recovery_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where substrate information loss becomes epistemic damage
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

  ┌─ [schema_lock_in_dynamics] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether categorical schemas create path-dependent lock-in
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

  ┌─ [biometric_accumulation_externality] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether substrate blindness creates uncompensated health externalities
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
