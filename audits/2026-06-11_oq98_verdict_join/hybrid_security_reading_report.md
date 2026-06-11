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
[SCENARIO MANAGER] Loading: testsets/hybrid_security_reading.pl...
[SHIM] grid injection DISABLED (grid_shim_enabled=false; OQ-96 interim / OQ-93 probe) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: hybrid_security_reading...
  [BRIDGE] Derived has_sunset_clause(hybrid_security_reading) from scaffold declaration
  [OPEN] grid imputation DISABLED (grid_shim_enabled=false): 32/32 grid points absent — expected-and-witnessed (OQ-93/OQ-96)
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: hybrid_security_reading

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: hybrid_security_reading (0-6)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 6 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 6 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 6 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 6 in hybrid_security_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] hybrid_security_reading from context(agent_power(powerless),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=snare, computed=scaffold
  [INDEX OK] hybrid_security_reading from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=tangled_rope, computed=tangled_rope
  [INDEX MISMATCH] hybrid_security_reading from context(agent_power(institutional),time_horizon(immediate),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=scaffold
  [INDEX OK] hybrid_security_reading from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=scaffold, computed=scaffold
  [INDEX MISMATCH] hybrid_security_reading from context(agent_power(institutional),time_horizon(biographical),exit_options(constrained),spatial_scope(national)): declared=tangled_rope, computed=scaffold
  [INDEX OK] hybrid_security_reading from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=tangled_rope, computed=tangled_rope
  [INTENT] Result: stable (Confidence: low) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  hybrid_security_reading:
    [warning] metric_substitution
        Evidence: evidence(theater_delta,0,6,0.35,0.58)
    [critical] extraction_accumulation
        Evidence: evidence(extraction_delta,0,6,0.38,0.48)
    [critical] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning] purity_drift
        Evidence: evidence(current_purity,0.4221666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.32999999999999996)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  hybrid_security_reading -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  hybrid_security_reading (ε=0.48):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.48 × 0.65 × 0.80 = 0.250
    moderate@national: d=0.700 f(d)=1.11 χ = 0.48 × 1.11 × 1.00 = 0.531
    institutional@national: d=0.250 f(d)=0.11 χ = 0.48 × 0.11 × 1.00 = 0.053
    analytical@global: d=0.720 f(d)=1.14 χ = 0.48 × 1.14 × 1.20 = 0.658

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: hybrid_security_reading ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=tangled_rope  institutional=scaffold  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries,sunset]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.422 (contaminated)




╔════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                       ║
║  12/12 subsystems — 1 tension(s) (fingerprint_voids)   ║
║  ! [moderate] signature_correction (signature_grade)   ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)║
╚════════════════════════════════════════════════════════╝

--- KERNEL: employment_boundary ---
  3 readings | 3 diverging pairs | 1 axiom conflicts
 *hybrid_security_reading [terminal=stable_pattern]
  formalist_employment_reading [terminal=husk, unacknowledged]
  substantive_employment_reading [terminal=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Signature:        coupling_invariant_rope
    Purity:           0.751333 (sound)
    Coupling:         independent (score: 0.25)
    Boltzmann:        compliant
    Live index:       scope (scope=2, power=0)
    Drift events:     4 — metric_substitution, extraction_accumulation, purity_drift, network_drift
    Tangled psi:      0.0104 (rope_leaning)
    Coalition:        other


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.7513
    Effective purity:   0.0284
    Propagation delta:  -0.7229

    Network neighbors (8):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | employment_boundary_flat_control | snare | shared_beneficiary | 0.30 | 0.3102 |
    | formalist_employment_reading | snare | explicit | 1.00 | 0.3276 |
    | platform_flexibility_precarity_tradeoff | snare | shared_beneficiary | 0.30 | 0.3869 |
    | retirement_security_deficit | snare | shared_beneficiary | 0.30 | 0.3327 |
    | substantive_employment_reading | snare | explicit | 1.00 | 0.3132 |
    | truth_democracy_disinformation | snare | shared_beneficiary | 0.30 | 0.3102 |
    | wage_convergence_mechanism | tangled_rope | shared_beneficiary | 0.30 | 0.4696 |
    | wage_convergence_sustainability | tangled_rope | shared_beneficiary | 0.30 | 0.4696 |

  Purity degraded from 0.7513 to 0.0284 by contamination from 8 neighbor(s), primarily truth_democracy_disinformation (shared_beneficiary, purity 0.3102).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_extraction_blindness_hybrid_security_reading
    Severity Score:    0.426
    Gap Class:         consensus
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F002

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       rope (powerless)
    Confidence:       0.0100 (borderline)
    Rival Type:       rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         tangled_rope->rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.6224]

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says tangled_rope, MaxEnt says rope
  Confidence:    0.0100 (borderline)
  Rival Type:    rope (P=0.9500)
  Margin:        -0.9400
  Entropy:       0.1557
  Distribution:  rope: 0.950, mountain: 0.010, tangled_rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9500)
  Entropy:       0.1557
  Distribution:  rope: 0.950, mountain: 0.010, tangled_rope: 0.010

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — kernel 'employment_boundary' has no confirmed governing params yet
  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  No abductive triggers fired. All diagnostic paths agree.

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.05592417, Var_fd=0.05367322 (96.0%), Var_scope=0.00397867 (7.1%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.249600 | 1.358606 | 0.8000 |
| moderate | 0.531116 | 1.106492 | 1.0000 |
| institutional | 0.052859 | 0.110123 | 1.0000 |
| analytical | 0.657567 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (11 subsystems):
    maxent, cohomology, abductive, signature, boltzmann, purity, dirac, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts: none

  Convergent Rejections: none

  Tensions (1):
    fingerprint_voids: extractive_voids([drifting_without_limit,no_exit_for_victims,unaccountable_extraction])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: anchored_fixity_with_accretion

  Anchored Fixity (with interpretive buffer): The kernel is formalized; the authority structure grounds its legitimacy in the kernel's unchangeability and extracts substantial benefit from preventing revision. Paired with a functioning interpretive substructure below the kernel that absorbs drift without surfacing revision. The Hindu Vedic-Brahmanical system, post-development Catholic doctrine, and the Confucian commentary tradition operate this way. This configuration can persist millennia: the unrevisable kernel is preserved while the interpretive layer does the acknowledgment work the kernel cannot do.

  Structural signals: kernel_formalized, authority_extraction, interp_layer_present

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor: stable_pattern

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 6
Structural Pattern: stable
Confidence:     low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [informational]: perspectival_incoherence detected for hybrid_security_reading

[STRUCTURAL SIGNATURE ANALYSIS]
  hybrid_security_reading: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for hybrid_security_reading: Enforcement present (suppression=0.62, resistance=0.68) with high extraction (0.48). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  hybrid_security_reading (snare vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.20 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [kernel_reading_ambiguity] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether hybrid category is genuine classification solution or naturalized extraction
  │
  │  RESOLUTION STRATEGY:
  │  1. Map stakeholder perspectives:
  │     - Document how different actors perceive unknown
  │     - Identify source of divergence
  │  2. Gather evidence:
  │     - Empirical metrics (suppression, extraction, resistance)
  │     - Historical behavior patterns
  │  3. Create indexical classification:
  │     - From powerless context: classify as X
  │     - From institutional context: classify as Y
  │     - Add explicit context annotations
  └─

  ┌─ [protection_floor_sufficiency] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether partial protections provide adequate biographical financial security
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

  ┌─ [calcification_vs_transition] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether hybrid category transitions to fuller protections or calcifies
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

  ┌─ [sibling_reading_foreclosure] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether hybrid category forecloses substantive employment reading
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

  ┌─ [omega_extraction_blindness_hybrid_security_reading] CONCEPTUAL CLARIFICATION
  │  Constraint: hybrid_security_reading
  │  Gap: Constraint hybrid_security_reading appears extractive (Snare) to individuals but functional (Rope) to institutions...
  │
  │  CRITICAL: Extraction Masking Detected
  │  Powerless see: SNARE (extractive trap)
  │  Institutions see: ROPE (functional rule)
  │
  │  RESOLUTION STRATEGY:
  │  1. Interview affected individuals (N=10+):
  │     - Who benefits from hybrid_security_reading?
  │     - Can you change/exit this constraint?
  │     - What would happen if you tried?
  │  2. Interview institutional actors (N=10+):
  │     - What function does hybrid_security_reading serve?
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


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: platform_companies (beneficiary: platform_companies, n=8)
  Members: employment_boundary_flat_control, formalist_employment_reading, hybrid_security_reading, platform_flexibility_precarity_tradeoff, retirement_security_deficit, truth_democracy_disinformation, wage_convergence_mechanism, wage_convergence_sustainability

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  constructed_high_extraction
    Constraints:       employment_boundary_flat_control, formalist_employment_reading, platform_flexibility_precarity_tradeoff, retirement_security_deficit, truth_democracy_disinformation

  [convergent_institutional]
    Institutional type: rope
    Analytical type:    snare
    Constraints:        employment_boundary_flat_control, formalist_employment_reading, platform_flexibility_precarity_tradeoff, retirement_security_deficit

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  employment_boundary_flat_control, formalist_employment_reading, hybrid_security_reading, platform_flexibility_precarity_tradeoff, retirement_security_deficit, truth_democracy_disinformation

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Reform interventions must address the group as a coordinated system, not individual constraints.
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Treating all constraints in this set as independent coordination mechanisms
    Ruled out by: Convergent signature and convergent institutional classification: all constraints share the same structural signature and the same institutional observer type, indicating coordinated rather than independent operation.

    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 6 constraints indicates active systemic instability, not constraint-local drift.

  Set: network_602aae01 (network adjacency, n=3)
  Members: formalist_employment_reading, hybrid_security_reading, substantive_employment_reading

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  constructed_high_extraction
    Constraints:       formalist_employment_reading, substantive_employment_reading

  [convergent_institutional]
    Institutional type: rope
    Analytical type:    snare
    Constraints:        formalist_employment_reading, substantive_employment_reading

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  formalist_employment_reading, hybrid_security_reading, substantive_employment_reading

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Reform interventions must address the group as a coordinated system, not individual constraints.
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Treating all constraints in this set as independent coordination mechanisms
    Ruled out by: Convergent signature and convergent institutional classification: all constraints share the same structural signature and the same institutional observer type, indicating coordinated rather than independent operation.

    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 3 constraints indicates active systemic instability, not constraint-local drift.
