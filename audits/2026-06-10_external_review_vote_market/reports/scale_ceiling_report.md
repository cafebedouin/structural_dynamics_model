CORPUS CONTEXT: 45 constraints
  Types: 7 mountain, 8 rope, 17 tangled_rope, 10 snare, 1 piton
  Network stability: cascading | 36 omegas (32 critical)
  Confidence: 18 deep (42%) | 2 moderate (5%) | 23 borderline (53%)
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
[SCENARIO MANAGER] Loading: testsets/scale_ceiling.pl...
  [INJECTED] 8 structural-level 0.5 anchors (m_gen) at hardcoded t=[0,10] — fabricated, see OQ-93
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: scale_ceiling...
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 4 (m_gen) + imputed-from-priors 28 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 4 stray injected 0.5 anchors off-grid (m_gen at hardcoded t=[0,10], interval endpoints differ)

>>> INITIATING DR-AUDIT SUITE: scale_ceiling

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: scale_ceiling (0-75)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] scale_ceiling from context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(national)): declared=mountain, computed=rope
  [INDEX MISMATCH] scale_ceiling from context(agent_power(organized),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=rope
  [INDEX MISMATCH] scale_ceiling from context(agent_power(institutional),time_horizon(biographical),exit_options(arbitrage),spatial_scope(continental)): declared=mountain, computed=rope
  [INDEX MISMATCH] scale_ceiling from context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(regional)): declared=mountain, computed=rope
  [INDEX MISMATCH] scale_ceiling from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global)): declared=mountain, computed=rope
  [INDEX MISMATCH] scale_ceiling from context(agent_power(institutional),time_horizon(generational),exit_options(constrained),spatial_scope(national)): declared=mountain, computed=rope
  [INTENT] Result: stable (Confidence: high) [grid diet: authored 0/32, injected 4, imputed 28 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  scale_ceiling:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,75,0.08,0.12)

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  scale_ceiling -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  scale_ceiling (ε=0.12):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.12 × 1.36 × 0.80 = 0.130
    moderate@national: d=0.700 f(d)=1.11 χ = 0.12 × 1.11 × 1.00 = 0.133
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.12 × -0.04 × 1.00 = -0.005
    analytical@global: d=0.720 f(d)=1.14 χ = 0.12 × 1.14 × 1.20 = 0.164

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: scale_ceiling ===
  Shift (computed via dr_type/3):
    powerless=rope  moderate=rope  institutional=rope  analytical=rope
  Properties: [asymmetric,coordination,has_beneficiaries,natural]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=stable  suppression=stable  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     1.000 (pristine)




╔═══════════════════════════════════════════════════╗
║  VERDICT: GREEN                                    ║
║  12/12 subsystems checked — no tensions           ║
╚═══════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        coupling_invariant_rope
    Purity:           1.0 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Live index:       none (scope=0, power=0)
    Drift events:     1 — extraction_accumulation


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   1.0000
    Effective purity:   1.0000
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       rope (powerless)
    Confidence:       0.0100 (borderline)
    Rival Type:       rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         mountain->rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.6564]

--- MAXENT SHADOW CLASSIFICATION ---

  HARD DISAGREEMENT: Pipeline says mountain, MaxEnt says rope
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

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  No abductive triggers fired. All diagnostic paths agree.

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.00426484, Var_fd=0.00431588 (101.2%), Var_scope=0.00022870 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.130426 | 1.358606 | 0.8000 |
| moderate | 0.132779 | 1.106492 | 1.0000 |
| institutional | -0.005070 | -0.042252 | 1.0000 |
| analytical | 0.164392 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (12 subsystems):
    maxent, cohomology, abductive, signature, boltzmann, purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts: none

  Convergent Rejections: none

  Tensions: none

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 75
Structural Pattern: stable
Confidence:     high
Grid diet:      authored 0/32, injected 4, imputed 28 (OQ-93)

[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]
  ! ALERT [severe]: type_1_false_summit detected for scale_ceiling

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  Detected 1 constraint(s) falsely claiming "Mountain" status, across 4 observer-context instance(s):

  ┌─ CONSTRAINT: scale_ceiling
  │  Context: context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))
  │
  │  Suppression Requirement: 0.08
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.08 > 0.05 threshold)
  │  Base Extractiveness: 0.12
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: scale_ceiling
  │  Context: context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))
  │
  │  Suppression Requirement: 0.08
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.08 > 0.05 threshold)
  │  Base Extractiveness: 0.12
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: scale_ceiling
  │  Context: context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national))
  │
  │  Suppression Requirement: 0.08
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.08 > 0.05 threshold)
  │  Base Extractiveness: 0.12
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: scale_ceiling
  │  Context: context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))
  │
  │  Suppression Requirement: 0.08
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.08 > 0.05 threshold)
  │  Base Extractiveness: 0.12
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─


[STRUCTURAL SIGNATURE ANALYSIS]
  scale_ceiling: coupling_invariant_rope (confidence: high)
    → COUPLING-INVARIANT ROPE signature for scale_ceiling: Certified true coordination mechanism. Boltzmann compliance=compliant(0), scope invariance=invariant, excess extraction=0.000. Passes all structural purity tests — this is genuine coordination, not low-extraction construction.

Aggregate Magnitude (Kappa) at Tn: 0.41 [grid diet: authored 0/32, injected 4, imputed 28 — OQ-93]

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [coordination_cost_function_form] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether coordination cost scaling is information-theoretic necessity or contingent empirical pattern
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

  ┌─ [capital_mobility_reversibility] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether capital account openness is reversible policy or structural lock-in
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

  ┌─ [sectoral_coordination_stability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Long-term stability of sectoral coordination under capital mobility pressure
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

  ┌─ [false_summit_naturalization] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether scale ceiling is natural law or naturalized institutional arrangement benefiting mobile capital
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

====================================================
