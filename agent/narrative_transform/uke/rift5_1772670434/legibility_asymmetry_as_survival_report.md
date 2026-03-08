CORPUS CONTEXT: 967 constraints
  Types: 144 mountain, 23 rope, 592 tangled_rope, 188 snare, 9 piton, 10 scaffold
  Network stability: cascading | 802 omegas (740 critical)
  Confidence: 720 deep (75%) | 53 moderate (5%) | 193 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/legibility_asymmetry_as_survival.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: legibility_asymmetry_as_survival...
  [BRIDGE] Derived has_sunset_clause(legibility_asymmetry_as_survival) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: legibility_asymmetry_as_survival

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: legibility_asymmetry_as_survival (0-6)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(powerless),time_horizon(biographical),exit_options(constrained),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(moderate),time_horizon(generational),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=mountain, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(organized),time_horizon(civilizational),exit_options(constrained),spatial_scope(global)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] legibility_asymmetry_as_survival from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 1 | Watch: 1

  legibility_asymmetry_as_survival:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.12,0.18)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.9600000000000001,decline_signals,[extraction_rising,theater_rising,excess_above_floor(0.09999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  legibility_asymmetry_as_survival -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  legibility_asymmetry_as_survival (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: legibility_asymmetry_as_survival ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=negligible  suppression=moderate
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.960 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.96 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     2 — extraction_accumulation, purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9600
    Effective purity:   0.9600
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_perspectival_legibility_asymmetry_as_survival
    Severity Score:    0.186
    Gap Class:         consensus
    Gap Pattern:       general_type_mismatch
    Family ID:         F356

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.8742 (deep)
    Rival Type:       scaffold (P=0.1258)
    Margin:           +0.7484
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.8742 (deep)
  Rival Type:    scaffold (P=0.1258)
  Margin:        +0.7484
  Entropy:       0.2112
  Distribution:  rope: 0.874, scaffold: 0.126, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9407)
  Entropy:       0.1256
  Distribution:  rope: 0.941, scaffold: 0.059, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0665 (moderate)
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

    Type:      144 mountain | 23 rope | 592 tangled_rope | 188 snare | 9 piton | 10 scaffold
    Purity:    158 pristine | 42 sound | 98 borderline | 650 contaminated | 18 degraded
    Coupling:  738 strongly | 21 weakly | 180 independent | 1 inconclusive
    Signature: 557 false_natural_law | 260 false_ci_rope | 139 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 720 deep | 53 moderate | 193 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.9% of corpus shares this signature)
    Purity band: pristine (16.3% of corpus in this band)
    Confidence band: deep (74.5% of corpus in this band)
    Boundary zone: rope->scaffold (12 constraints share this boundary)
    Orbit Family ID:  F356

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
  legibility_asymmetry_as_survival: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for legibility_asymmetry_as_survival: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.09999999999999999)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: monitored_communities
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.46

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  legibility_asymmetry_as_survival (rope vs mountain):
    ! MANDATROPHY GAP: delta_chi = 0.21 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [substrate_decoding_timeline] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Timeline for surveillance apparatus to decode substrate complexity
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

  ┌─ [compliance_theater_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Threshold where compliance performance becomes net burden
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

  ┌─ [intergenerational_transmission_fidelity] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether substrate encoding preserves or degrades practice fidelity
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

  ┌─ [omega_perspectival_legibility_asymmetry_as_survival] CONCEPTUAL CLARIFICATION
  │  Constraint: legibility_asymmetry_as_survival
  │  Gap: Constraint legibility_asymmetry_as_survival appears as rope to individuals but mountain to institutions...
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive legibility_asymmetry_as_survival
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

====================================================
