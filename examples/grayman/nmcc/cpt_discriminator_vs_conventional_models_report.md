CORPUS CONTEXT: 1045 constraints
  Types: 167 mountain, 37 rope, 621 tangled_rope, 200 snare, 9 piton, 10 scaffold
  Network stability: cascading | 846 omegas (781 critical)
  Confidence: 766 deep (73%) | 70 moderate (7%) | 208 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/cpt_discriminator_vs_conventional_models.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: cpt_discriminator_vs_conventional_models...
  [FIXED] Imputed 28 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: cpt_discriminator_vs_conventional_models

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: cpt_discriminator_vs_conventional_models (0-50)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] cpt_discriminator_vs_conventional_models from context(agent_power(institutional),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] cpt_discriminator_vs_conventional_models from context(agent_power(institutional),time_horizon(biographical),exit_options(mobile),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] cpt_discriminator_vs_conventional_models from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] cpt_discriminator_vs_conventional_models from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] cpt_discriminator_vs_conventional_models from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  cpt_discriminator_vs_conventional_models:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  cpt_discriminator_vs_conventional_models (ε=0.08):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.08 × 1.39 × 0.80 = 0.089
    moderate@national: d=0.650 f(d)=1.01 χ = 0.08 × 1.01 × 1.00 = 0.081
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.08 × -0.04 × 1.00 = -0.003
    analytical@global: d=0.720 f(d)=1.14 χ = 0.08 × 1.14 × 1.20 = 0.110

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: cpt_discriminator_vs_conventional_models ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.976 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
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

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    Confidence:       0.6730 (moderate)
    Rival Type:       scaffold (P=0.3270)
    Margin:           +0.3460
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.6730 (moderate)
  Rival Type:    scaffold (P=0.3270)
  Margin:        +0.3460
  Entropy:       0.3528
  Distribution:  rope: 0.673, scaffold: 0.327

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      scaffold (P=0.9378)
  Entropy:       0.1300
  Distribution:  scaffold: 0.938, rope: 0.062

  Classical/Indexed TV Distance: 0.6108 (large)
  Significant divergence — observer-dependence changes the probabilistic landscape. Classical Oracle Gap (Theorem 4): single-position analysis misses this structure.

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

    Type:      167 mountain | 37 rope | 621 tangled_rope | 200 snare | 9 piton | 10 scaffold
    Purity:    192 pristine | 43 sound | 115 borderline | 674 contaminated | 20 degraded
    Coupling:  779 strongly | 23 weakly | 214 independent | 1 inconclusive
    Signature: 574 false_natural_law | 298 false_ci_rope | 162 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 766 deep | 70 moderate | 208 borderline (mean: 0.751)

  --- CONSTRAINT POSITIONING ---
    This constraint is a false_ci_rope (28.5% of corpus shares this signature)
    Purity band: pristine (18.4% of corpus in this band)
    Confidence band: moderate (6.7% of corpus in this band)
    Boundary zone: rope->scaffold (23 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.2 (low variance)
  Index Configs:       5
  Types Produced:      1

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
  cpt_discriminator_vs_conventional_models: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for cpt_discriminator_vs_conventional_models: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.06)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: experimental_monopole_search_programs
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.41

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================
