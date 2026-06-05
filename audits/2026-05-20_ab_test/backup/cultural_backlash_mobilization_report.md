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
[SCENARIO MANAGER] Loading: testsets/cultural_backlash_mobilization.pl...
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: cultural_backlash_mobilization...
  [BRIDGE] Derived has_sunset_clause(cultural_backlash_mobilization) from scaffold declaration
  [FIXED] Imputed 24 missing vectors using domain priors

>>> INITIATING DR-AUDIT SUITE: cultural_backlash_mobilization

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: cultural_backlash_mobilization (0-10)
--- [END] Data Verification Complete ---
[OK] Verification passed.

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] cultural_backlash_mobilization from context(agent_power(powerless),time_horizon(biographical),exit_options(identity_locked),spatial_scope(national)): declared=snare, computed=tangled_rope
  [INDEX MISMATCH] cultural_backlash_mobilization from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX MISMATCH] cultural_backlash_mobilization from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX MISMATCH] cultural_backlash_mobilization from context(agent_power(moderate),time_horizon(generational),exit_options(constrained),spatial_scope(regional)): declared=tangled_rope, computed=scaffold
  [INDEX OK] cultural_backlash_mobilization from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] cultural_backlash_mobilization from context(agent_power(institutional),time_horizon(civilizational),exit_options(arbitrage),spatial_scope(national)): declared=piton, computed=scaffold
  [INDEX OK] cultural_backlash_mobilization from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: high)

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 0 | Warning: 4 | Watch: 0

  cultural_backlash_mobilization:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,10,0.42,0.58)
    [warning] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.28,0.38)
    [warning] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.3925,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.3)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  cultural_backlash_mobilization -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  cultural_backlash_mobilization (ε=0.38):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.38 × 1.36 × 0.80 = 0.413
    moderate@national: d=0.700 f(d)=1.11 χ = 0.38 × 1.11 × 1.00 = 0.420
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.38 × -0.04 × 1.00 = -0.016
    analytical@global: d=0.720 f(d)=1.14 χ = 0.38 × 1.14 × 1.20 = 0.521

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: cultural_backlash_mobilization ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=scaffold  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=low  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,local-national,1.0),coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,global-local,1.0),coupled(power_scope,analytical,global-national,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.393 (contaminated)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        false_ci_rope
    Purity:           0.434167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Drift events:     4 — metric_substitution, extraction_accumulation, coupling_drift, purity_drift
    Tangled psi:      0.0000 (rope_leaning)
    Coalition:        institutional_dissent


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4342
    Effective purity:   0.4342
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- HUSK SIGNATURE ---

  Existence:  NO  (type unstable across series)

  Saturation boundary:    -0.1200
    full glide regime: native floor (0.08) ≥ -0.1200 — EX glides across floor
    [floor-invariant, series-determined; for generated constraints reflects
     ε authoring, not an observed property of the underlying phenomenon]

  Saturation crossover:   NOT REACHED  (max ε < 0.58 — EX was positive throughout;
                           EP decay, if any, came from F/SI/CC, not EX fall)

  EP at native floor (0.08):
    NOT cross-comparable across constraint families; native floor varies by coordination type.
    [context: powerless canonical — victim perspective]
    t=  0  EP=0.4742
    t=  5  EP=0.4542
    t= 10  EP=0.4342

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_cultural_backlash_mobilization
    Severity Score:    0.466
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F019

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional)
    Confidence:       1.0000 (deep)
    Rival Type:       mountain (P=0.0000)
    Margin:           +1.0000
    Boundary:         tangled_rope->mountain
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    1.0000 (deep)
  Rival Type:    mountain (P=0.0000)
  Margin:        +1.0000
  Entropy:       0.0000
  Distribution:  tangled_rope: 1.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=1.0000)
  Entropy:       0.0000
  Distribution:  tangled_rope: 1.000, piton: 0.000

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
  Var_total=0.04276703, Var_fd=0.04327871 (101.2%), Var_scope=0.00229331 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.413016 | 1.358606 | 0.8000 |
| moderate | 0.420467 | 1.106492 | 1.0000 |
| institutional | -0.016056 | -0.042252 | 1.0000 |
| analytical | 0.520574 | 1.141609 | 1.2000 |


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
  ! ALERT [informational]: perspectival_incoherence detected for cultural_backlash_mobilization

[STRUCTURAL SIGNATURE ANALYSIS]
  cultural_backlash_mobilization: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for cultural_backlash_mobilization: Appears to be rope (indexed_rope_classification) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.29),scope_variant([scaffold,tangled_rope]),excess_above_floor(0.3),nonsensical_coupling(0.5)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: conservative_political_entrepreneurs
    → Institutional d=0.120

Aggregate Magnitude (Kappa) at Tn: 0.50

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  cultural_backlash_mobilization (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.43 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [material_vs_symbolic_driver] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether backlash is materially or symbolically driven
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

  ┌─ [elite_manipulation_vs_organic_demand] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Elite supply vs organic demand for backlash mobilization
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

  ┌─ [sunset_timeline_demographic_shift] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Timeline and mechanism of backlash coalition decline
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

  ┌─ [bundling_economic_harm] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Material harm from bundled economic policies
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

  ┌─ [omega_extraction_blindness_cultural_backlash_mobilization] CONCEPTUAL CLARIFICATION
  │  Constraint: cultural_backlash_mobilization
  │  Gap: Constraint cultural_backlash_mobilization appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from cultural_backlash_mobilization?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does cultural_backlash_mobilization serve?
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
