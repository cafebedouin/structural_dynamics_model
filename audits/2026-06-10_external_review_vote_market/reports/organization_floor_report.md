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
[SCENARIO MANAGER] Loading: testsets/organization_floor.pl...
  [INJECTED] 8 structural-level 0.5 anchors (m_gen) at hardcoded t=[0,10] — fabricated, see OQ-93
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: organization_floor...
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 4 (m_gen) + imputed-from-priors 28 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 4 stray injected 0.5 anchors off-grid (m_gen at hardcoded t=[0,10], interval endpoints differ)

>>> INITIATING DR-AUDIT SUITE: organization_floor

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: organization_floor (0-75)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX MISMATCH] organization_floor from context(agent_power(powerless),time_horizon(immediate),exit_options(trapped),spatial_scope(local)): declared=mountain, computed=rope
  [INDEX MISMATCH] organization_floor from context(agent_power(moderate),time_horizon(biographical),exit_options(constrained),spatial_scope(regional)): declared=mountain, computed=rope
  [INDEX OK] organization_floor from context(agent_power(organized),time_horizon(generational),exit_options(mobile),spatial_scope(national)): declared=rope, computed=rope
  [INDEX OK] organization_floor from context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national)): declared=rope, computed=rope
  [INDEX MISMATCH] organization_floor from context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(universal)): declared=mountain, computed=rope
  [INTENT] Result: stable (Confidence: high) [grid diet: authored 0/32, injected 4, imputed 28 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  organization_floor:
    [watch] extraction_accumulation
        Evidence: evidence(extraction_delta,0,75,0.12,0.15)
    [watch] purity_drift
        Evidence: evidence(current_purity,1.0,decline_signals,[theater_rising])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  organization_floor -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  organization_floor (ε=0.15):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.15 × 1.36 × 0.80 = 0.163
    moderate@national: d=0.700 f(d)=1.11 χ = 0.15 × 1.11 × 1.00 = 0.166
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.15 × -0.04 × 1.00 = -0.006
    analytical@global: d=0.720 f(d)=1.14 χ = 0.15 × 1.14 × 1.20 = 0.205

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: organization_floor ===
  Shift (computed via dr_type/3):
    powerless=rope  moderate=rope  institutional=rope  analytical=rope
  Properties: [asymmetric,coordination,has_beneficiaries,natural]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=concentrated
  Drift:      extraction=stable  suppression=stable  theater=rising
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
    Drift events:     2 — extraction_accumulation, purity_drift

--- TEMPORAL TRAJECTORY ---

    theater_ratio: ceiling approach — rate 0.0015→-0.0013 over T=0–75, flattening near 0.20

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   1.0000
    Effective purity:   1.0000
    Propagation delta:  +0.0000

    Network neighbors (4):

    | Neighbor | Type | Edge | Strength | Purity |
    |----------|------|------|----------|--------|
    | ballot_access_requirements | unknown | explicit | 1.00 | N/A |
    | campaign_finance_thresholds | unknown | explicit | 1.00 | N/A |
    | lobbying_registration_rules | unknown | explicit | 1.00 | N/A |
    | union_recognition_procedures | unknown | explicit | 1.00 | N/A |

  No significant contamination — purity unchanged across 4 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Omega: omega_cut_safety_organization_floor
    Severity Score:    0.15
    Gap Class:         consensus
    Gap Pattern:       mountain_coordination_confusion
    Family ID:         F016

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
  Var_total=0.00666386, Var_fd=0.00674357 (101.2%), Var_scope=0.00035734 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.163033 | 1.358606 | 0.8000 |
| moderate | 0.165974 | 1.106492 | 1.0000 |
| institutional | -0.006338 | -0.042252 | 1.0000 |
| analytical | 0.205490 | 1.141609 | 1.2000 |


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
  ! ALERT [severe]: type_1_false_summit detected for organization_floor

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  Detected 1 constraint(s) falsely claiming "Mountain" status, across 4 observer-context instance(s):

  ┌─ CONSTRAINT: organization_floor
  │  Context: context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))
  │
  │  Suppression Requirement: 0.25
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.25 > 0.05 threshold)
  │  Base Extractiveness: 0.15
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: organization_floor
  │  Context: context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))
  │
  │  Suppression Requirement: 0.25
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.25 > 0.05 threshold)
  │  Base Extractiveness: 0.15
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: organization_floor
  │  Context: context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national))
  │
  │  Suppression Requirement: 0.25
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.25 > 0.05 threshold)
  │  Base Extractiveness: 0.15
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: organization_floor
  │  Context: context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))
  │
  │  Suppression Requirement: 0.25
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.25 > 0.05 threshold)
  │  Base Extractiveness: 0.15
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─


[STRUCTURAL SIGNATURE ANALYSIS]
  organization_floor: coupling_invariant_rope (confidence: high)
    → COUPLING-INVARIANT ROPE signature for organization_floor: Certified true coordination mechanism. Boltzmann compliance=compliant(0), scope invariance=invariant, excess extraction=0.000. Passes all structural purity tests — this is genuine coordination, not low-extraction construction.

Aggregate Magnitude (Kappa) at Tn: 0.44 [grid diet: authored 0/32, injected 4, imputed 28 — OQ-93]

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  organization_floor (mountain vs rope):
    ! MANDATROPHY GAP: delta_chi = 0.17 (moderate)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [floor_height_variability] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether organization floor height is universal or context-dependent
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

  ┌─ [digital_organizing_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether digital tools lower the organization floor or shift the bottleneck
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

  ┌─ [beneficiary_naturalization] CONCEPTUAL CLARIFICATION
  │  Constraint: unknown
  │  Gap: Whether the floor is a natural transaction cost or a constructed entry barrier
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

  ┌─ [preference_intensity_threshold] EMPIRICAL DATA COLLECTION
  │  Constraint: unknown
  │  Gap: Whether high-intensity preferences can bypass the organization floor
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

  ┌─ [omega_cut_safety_organization_floor] CONCEPTUAL CLARIFICATION
  │  Constraint: organization_floor
  │  Gap: Constraint organization_floor appears unchangeable (Mountain) to individuals but optional (Rope) to institutions...
  │
  │  HIGH RISK: Coordination Cut Safety
  │  Powerless see: MOUNTAIN (unchangeable, survival-critical)
  │  Institutions see: ROPE (optional, changeable)
  │
  │  RESOLUTION STRATEGY:
  │  1. SAFETY ASSESSMENT (DO NOT SKIP):
  │     - If institutions cut organization_floor, do individuals have alternatives?
  │     - Is this their only survival mechanism?
  │     - What scaffolding exists for transition?
  │  2. Test institutional perception:
  │     - Can institutions unilaterally change this?
  │     - Do they understand downstream impacts?
  │     - Is their "optional" view empirically accurate?
  │  3. Decision tree:
  │     IF truly unchangeable → Correct institutional view to MOUNTAIN
  │     IF changeable + safe alternatives → Correct powerless view to ROPE
  │     IF changeable + NO alternatives → ADD SCAFFOLD before any change
  │     IF uncertainty → HALT changes until resolved
  │  4. CRITICAL: Never proceed with changes until safety verified
  └─

====================================================
