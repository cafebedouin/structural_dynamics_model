CORPUS CONTEXT: 195 constraints
  Types: 5 mountain, 13 rope, 151 tangled_rope, 23 snare, 1 piton, 1 scaffold
  Network stability: cascading | 181 omegas (164 critical)
  Confidence: 162 deep (84%) | 8 moderate (4%) | 24 borderline (12%)
  CS patterns: 148 classified | 1 anchored_fixity_brittle, 24 anchored_fixity_with_accretion, 10 diffuse_reconstruction, 2 implicit_practice, 78 interpretive_accretion, 1 marked_revision | 56 verdicts fired
  CS grounding mismatches: 105 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/elite_legitimacy_collapse.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: elite_legitimacy_collapse...
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: elite_legitimacy_collapse

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: elite_legitimacy_collapse (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] elite_legitimacy_collapse from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX OK] elite_legitimacy_collapse from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=tangled_rope
  [INDEX OK] elite_legitimacy_collapse from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] elite_legitimacy_collapse from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(continental)): declared=tangled_rope, computed=naturalized
  [INDEX MISMATCH] elite_legitimacy_collapse from context(agent_power(institutional),time_horizon(civilizational),exit_options(constrained),spatial_scope(national)): declared=piton, computed=naturalized
  [INDEX OK] elite_legitimacy_collapse from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  elite_legitimacy_collapse:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.32,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.615,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  elite_legitimacy_collapse -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  elite_legitimacy_collapse (ε=0.48):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.48 × 1.36 × 0.80 = 0.522
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.48 × -0.04 × 1.00 = -0.020
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: elite_legitimacy_collapse ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=high
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.615 (borderline)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.615 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.0467 (rope_leaning)
    Coalition:        institutional_dissent


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6150
    Effective purity:   0.6150
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- HUSK SIGNATURE ---

  Existence:  NO  (type unstable across series)

  Saturation boundary:    -0.0200
    full glide regime: native floor (0.08) ≥ -0.0200 — EX glides across floor
    [floor-invariant, series-determined; for generated constraints reflects
     ε authoring, not an observed property of the underlying phenomenon]

  Saturation crossover:   NOT REACHED  (max ε < 0.58 — EX was positive throughout;
                           EP decay, if any, came from F/SI/CC, not EX fall)

  EP at native floor (0.08):
    NOT cross-comparable across constraint families; native floor varies by coordination type.
    [context: powerless canonical — victim perspective]
    t=  0  EP=0.6790
    t=  5  EP=0.6430
    t= 10  EP=0.6150

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_elite_legitimacy_collapse
    Severity Score:    0.516
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F019

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       1.0000 (deep)
    Rival Type:       snare (P=0.0000)
    Margin:           +0.9999
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    1.0000 (deep)
  Rival Type:    snare (P=0.0000)
  Margin:        +0.9999
  Entropy:       0.0003
  Distribution:  tangled_rope: 1.000, snare: 0.000, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9999)
  Entropy:       0.0003
  Distribution:  tangled_rope: 1.000, snare: 0.000, piton: 0.000

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.06823764, Var_fd=0.06905412 (101.2%), Var_scope=0.00365914 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.521705 | 1.358606 | 0.8000 |
| moderate | 0.531116 | 1.106492 | 1.0000 |
| institutional | -0.020281 | -0.042252 | 1.0000 |
| analytical | 0.657567 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (4):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     high

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for elite_legitimacy_collapse

[STRUCTURAL SIGNATURE ANALYSIS]
  elite_legitimacy_collapse: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for elite_legitimacy_collapse: Appears to be rope (indexed_rope_classification) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.29),excess_above_floor(0.39999999999999997)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: populist_party_leadership
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.50

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  elite_legitimacy_collapse (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.54 (high)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [grievance_legitimacy_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Economic threshold distinguishing legitimate grievance from extractive scapegoating
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

  ┌─ [populist_governance_counterfactual] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether populist governance delivers material improvements to core voters
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

  ┌─ [cordon_sanitaire_effectiveness] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether cordon sanitaire prevents or merely delays populist influence
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

  ┌─ [cultural_vs_economic_primacy] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Relative weight of economic vs cultural drivers of legitimacy collapse
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

  ┌─ [omega_extraction_blindness_elite_legitimacy_collapse] CONCEPTUAL CLARIFICATION
  │  Constraint: elite_legitimacy_collapse
  │  Gap: Constraint elite_legitimacy_collapse appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from elite_legitimacy_collapse?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does elite_legitimacy_collapse serve?
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
