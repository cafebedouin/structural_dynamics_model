CORPUS CONTEXT: 982 constraints
  Types: 147 mountain, 25 rope, 599 tangled_rope, 191 snare, 9 piton, 10 scaffold
  Network stability: cascading | 813 omegas (751 critical)
  Confidence: 728 deep (74%) | 56 moderate (6%) | 197 borderline (20%)

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/capability_compulsion_gradient.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: capability_compulsion_gradient...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: capability_compulsion_gradient

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: capability_compulsion_gradient (0-0)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX OK] capability_compulsion_gradient from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(powerful),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(global)): declared=mountain, computed=mountain
  [INDEX OK] capability_compulsion_gradient from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=mountain
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  capability_compulsion_gradient:
    [watch] purity_drift
        Evidence: evidence(current_purity,0.9440000000000001,decline_signals,[excess_above_floor(0.14)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  capability_compulsion_gradient (ε=0.22):
    powerless@local: d=1.000 f(d)=1.42 χ = 0.22 × 1.42 × 0.80 = 0.250
    moderate@national: d=0.646 f(d)=1.00 χ = 0.22 × 1.00 × 1.00 = 0.220
    institutional@national: d=0.000 f(d)=-0.12 χ = 0.22 × -0.12 × 1.00 = -0.026
    analytical@global: d=0.725 f(d)=1.15 χ = 0.22 × 1.15 × 1.20 = 0.304

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: capability_compulsion_gradient ===
  Shift (computed via dr_type/3):
    powerless=mountain  moderate=mountain  institutional=mountain  analytical=mountain
  Properties: [has_temporal_data,natural]
  Voids:      []
  Actors:     beneficiaries=none  victims=none
  Drift:      extraction=unknown  suppression=unknown  theater=unknown
  Zone:       extraction=negligible  suppression=negligible
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.944 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        natural_law
    Purity:           0.944 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Drift events:     1 — purity_drift

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9440
    Effective purity:   0.9440
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

    Type:      147 mountain | 25 rope | 599 tangled_rope | 191 snare | 9 piton | 10 scaffold
    Purity:    162 pristine | 42 sound | 101 borderline | 657 contaminated | 19 degraded
    Coupling:  748 strongly | 22 weakly | 184 independent | 1 inconclusive
    Signature: 560 false_natural_law | 269 false_ci_rope | 142 natural_law | 7 constructed_high_extraction | 4 coupling_invariant_rope
    Confidence: 728 deep | 56 moderate | 197 borderline (mean: 0.753)

  --- CONSTRAINT POSITIONING ---
    This constraint is a natural_law (14.5% of corpus shares this signature)
    Purity band: pristine (16.5% of corpus in this band)
    Confidence band: deep (74.2% of corpus in this band)
    Boundary zone: mountain->rope (147 constraints share this boundary)

--- STRUCTURAL CONTEXT ---

  Variance Ratio:      0.2 (low variance)
  Index Configs:       5
  Types Produced:      1

  Structural Twins:     [not found in batch twin analysis]

  Covering Analysis:    [not found in batch covering analysis]

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 0
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for capability_compulsion_gradient

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  All mountains are structurally validated.

[STRUCTURAL SIGNATURE ANALYSIS]
  capability_compulsion_gradient: natural_law (confidence: low)
    → NATURAL LAW signature for capability_compulsion_gradient: Extreme inaccessibility (collapse=0.92) with minimal enforcement (suppression=0.04, resistance=0.08). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.

Aggregate Magnitude (Kappa) at Tn: 0.42

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  No unresolved Omegas. System is epistemically complete.
====================================================
