CORPUS CONTEXT: 48 constraints
  Types: 8 mountain, 9 rope, 18 tangled_rope, 10 snare, 1 piton
  Network stability: cascading | 37 omegas (33 critical)
  Confidence: 19 deep (41%) | 2 moderate (4%) | 25 borderline (54%)
  CS patterns: 23 classified | 3 anchored_fixity_with_accretion, 3 diffuse_reconstruction, 16 interpretive_accretion | 7 verdicts fired
  CS grounding mismatches: 15 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/regulatory_measurement_gap.pl...
[SHIM] grid injection DISABLED (grid_shim_enabled=false; OQ-96 interim / OQ-93 probe) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: regulatory_measurement_gap...
  [BRIDGE] Derived has_sunset_clause(regulatory_measurement_gap) from scaffold declaration
  [OPEN] grid imputation DISABLED (grid_shim_enabled=false): 32/32 grid points absent — expected-and-witnessed (OQ-93/OQ-96)
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: regulatory_measurement_gap

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: regulatory_measurement_gap (0-10)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 10 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 10 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 10 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 10 in regulatory_measurement_gap (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] regulatory_measurement_gap from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] regulatory_measurement_gap from context(agent_power(organized),time_horizon(biographical),exit_options(mobile),spatial_scope(global)): declared=rope, computed=tangled_rope
  [INDEX OK] regulatory_measurement_gap from context(agent_power(powerful),time_horizon(immediate),exit_options(arbitrage),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] regulatory_measurement_gap from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(global)): declared=scaffold, computed=tangled_rope
  [INDEX MISMATCH] regulatory_measurement_gap from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=rope, computed=tangled_rope
  [INDEX MISMATCH] regulatory_measurement_gap from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: low) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 0 | Warning: 1 | Watch: 2

  regulatory_measurement_gap:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,10,0.15,0.28)
    [watch] coupling_drift
        Evidence: evidence(coupling_score,0.375,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.7418333333333333,decline_signals,[extraction_rising,coupling_above_threshold(0.375),theater_rising,excess_above_floor(0.26)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  regulatory_measurement_gap -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  regulatory_measurement_gap (ε=0.28):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.28 × 1.36 × 0.80 = 0.304
    moderate@national: d=0.700 f(d)=1.11 χ = 0.28 × 1.11 × 1.00 = 0.310
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.28 × -0.04 × 1.00 = -0.012
    analytical@global: d=0.720 f(d)=1.14 χ = 0.28 × 1.14 × 1.20 = 0.384

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: regulatory_measurement_gap ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=falling  theater=rising
  Zone:       extraction=low  suppression=moderate
  Coupling:   weakly_coupled (score=0.375, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(0.375,0.25))
  Purity:     0.742 (sound)




╔═══════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                   ║
║  12/12 subsystems — 1 tension(s) (abductive)      ║
╚═══════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Signature:        false_ci_rope
    Purity:           0.741833 (sound)
    Coupling:         weakly_coupled (score: 0.375)
    Boltzmann:        non_compliant
    Live index:       both (scope=2, power=1)
    Drift events:     3 — extraction_accumulation, coupling_drift, purity_drift


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.7418
    Effective purity:   0.7418
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
    Confidence:       0.8216 (deep)
    Rival Type:       tangled_rope (P=0.1769)
    Margin:           +0.6447
    Boundary:         rope->tangled_rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.6224]

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  Confidence:    0.8216 (deep)
  Rival Type:    tangled_rope (P=0.1769)
  Margin:        +0.6447
  Entropy:       0.2664
  Distribution:  rope: 0.822, tangled_rope: 0.177, scaffold: 0.001

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.8222)
  Entropy:       0.2640
  Distribution:  rope: 0.822, tangled_rope: 0.177, scaffold: 0.001

  Classical/Indexed TV Distance: 0.0007 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.02321984, Var_fd=0.02349758 (101.2%), Var_scope=0.00124512 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.304328 | 1.358606 | 0.8000 |
| moderate | 0.309818 | 1.106492 | 1.0000 |
| institutional | -0.011831 | -0.042252 | 1.0000 |
| analytical | 0.383581 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    cohomology, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: stable
Confidence:     low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93)

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  regulatory_measurement_gap: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for regulatory_measurement_gap: Appears to be rope (explicit_rope_claim) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(0.375,0.25),excess_above_floor(0.26),nonsensical_coupling(0.16666666666666666)]. Coupling score=0.375. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: aging_research_community
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [biomarker_sufficiency_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Evidence threshold for biomarker qualification as aging endpoint
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

  ┌─ [composite_endpoint_acceptance] VALUE ARBITRATION
  │  Constraint: unknown
  │  Gap: Whether composite endpoints are acceptable for aging intervention approval
  │
  │  NOTE: Not resolvable via data or logic alone
  │
  │  RESOLUTION STRATEGY:
  │  1. Document competing value frameworks:
  │     - What values support current unknown?
  │     - What values oppose it?
  │     - Are these incommensurable?
  │  2. Propose scaffolded solution:
  │     - Design mechanism respecting both value sets
  │     - Create exit options for dissenters
  │     - Allow preference-based sorting
  │  3. Accept unresolvability if necessary:
  │     - Some omegas represent genuine value pluralism
  │     - Solution: coexistence, not consensus
  └─

  ┌─ [international_harmonization] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether international regulatory standards for aging biomarkers will harmonize
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

  ┌─ [xprize_protocol_adoption] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether XPrize protocol becomes de facto standard before FDA qualification
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
