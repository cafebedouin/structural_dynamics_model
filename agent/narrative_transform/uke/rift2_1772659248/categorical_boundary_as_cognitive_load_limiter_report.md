CORPUS CONTEXT: 959 constraints
  Types: 143 mountain, 20 rope, 589 tangled_rope, 187 snare, 9 piton, 10 scaffold
  Network stability: cascading | 796 omegas (738 critical)
  Confidence: 713 deep (74%) | 53 moderate (6%) | 192 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/categorical_boundary_as_cognitive_load_limiter.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: categorical_boundary_as_cognitive_load_limiter...
  [BRIDGE] Derived has_sunset_clause(categorical_boundary_as_cognitive_load_limiter) from scaffold declaration
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: categorical_boundary_as_cognitive_load_limiter

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: categorical_boundary_as_cognitive_load_limiter (0-50)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] categorical_boundary_as_cognitive_load_limiter from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(local)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_boundary_as_cognitive_load_limiter from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(regional)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_boundary_as_cognitive_load_limiter from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] categorical_boundary_as_cognitive_load_limiter from context(agent_power(organized),time_horizon(civilizational),exit_options(constrained),spatial_scope(global)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] categorical_boundary_as_cognitive_load_limiter from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  categorical_boundary_as_cognitive_load_limiter:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.936,decline_signals,[excess_above_floor(0.16)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  categorical_boundary_as_cognitive_load_limiter (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: categorical_boundary_as_cognitive_load_limiter ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
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
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9360
    Effective purity:   0.9360
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

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
    Confidence:       0.9613 (deep)
    Rival Type:       scaffold (P=0.0387)
    Margin:           +0.9227
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.9613 (deep)
  Rival Type:    scaffold (P=0.0387)
  Margin:        +0.9227
  Entropy:       0.0913
  Distribution:  rope: 0.961, scaffold: 0.039, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9827)
  Entropy:       0.0488
  Distribution:  rope: 0.983, scaffold: 0.017, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0213 (moderate)
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

    Type:      143 mountain | 20 rope | 589 tangled_rope | 187 snare | 9 piton | 10 scaffold
    Purity:    154 pristine | 41 sound | 97 borderline | 648 contaminated | 18 degraded
    Coupling:  735 strongly | 21 weakly | 175 independent | 1 inconclusive
    Signature: 556 false_natural_law | 254 false_ci_rope | 138 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 713 deep | 53 moderate | 192 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (26.5% of corpus shares this signature)
    Purity band: pristine (16.1% of corpus in this band)
    Confidence band: deep (74.4% of corpus in this band)
    Boundary zone: rope->scaffold (9 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.4 (low variance)
  Index Configs:       5
  Types Produced:      2

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 50
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  categorical_boundary_as_cognitive_load_limiter: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for categorical_boundary_as_cognitive_load_limiter: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.16)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: role_specialized_practitioners
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.44

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================
