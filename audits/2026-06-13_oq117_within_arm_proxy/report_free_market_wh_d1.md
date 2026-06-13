CORPUS CONTEXT: 30 constraints
  Types: 30 mountain
  Network stability: cascading | 0 omegas (0 critical)
  MaxEnt bands (corpus): 0 deep (0%) | 0 moderate (0%) | 30 borderline (100%)
  CS patterns: 3 classified | 2 diffuse_reconstruction, 1 marked_revision
  CS grounding mismatches: 0 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/free_market_naturalization_wh_d1.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: free_market_naturalization_wh_d1...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: free_market_naturalization_wh_d1

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: free_market_naturalization_wh_d1 (0-50)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 50 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 50 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 50 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 50 in free_market_naturalization_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] free_market_naturalization_wh_d1: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  free_market_naturalization_wh_d1:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,50,0.45,0.68)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.53)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  free_market_naturalization_wh_d1 -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  free_market_naturalization_wh_d1 (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: free_market_naturalization_wh_d1 ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,has_beneficiaries,natural]
  Voids:      [drifting_without_limit,extractive_immutable,no_exit_for_victims,self_sustaining_extraction,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.3))
  Purity:     0.354 (contaminated)




╔════════════════════════════════════════════════════════════════════════════╗
║  VERDICT: RED                                                              ║
║  BASE: YELLOW (12/12 subsystems — 1 tension(s) (abductive)) — CAPPED TO RED║
║  ! [moderate] signature_correction (signature_grade)                       ║
║  ! [severe] type_1_false_summit (claim_mismatch)                           ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)                    ║
╚════════════════════════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        false_natural_law
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=7)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

    Network neighbors (4):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | free_market_naturalization_fed_d2 | tangled_rope | shared_beneficiary | corpus-derived | low | 0.30 | 0.8803 |
    | free_market_naturalization_fed_d3 | tangled_rope | shared_victim | corpus-derived | low | 0.90 | 0.3542 |
    | free_market_naturalization_wh_d2 | tangled_rope | shared_beneficiary | corpus-derived | low | 0.30 | 0.3542 |
    | free_market_naturalization_wh_d3 | tangled_rope | shared_beneficiary | corpus-derived | low | 0.30 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  No significant contamination — purity unchanged across 4 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  [enriched_omega_data.json not available]

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    MaxEnt P(claimed): 0.0100 (borderline)
    Rival Type:       tangled_rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         mountain->tangled_rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.6029]

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: tangled_rope at P=0.9500 (pipeline says mountain)
  MaxEnt P(claimed): 0.0100 (borderline)
  Rival Type:    tangled_rope (P=0.9500)
  Margin:        -0.9400
  Entropy:       0.1557
  Distribution:  tangled_rope: 0.950, mountain: 0.010, rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9500)
  Entropy:       0.1557
  Distribution:  tangled_rope: 0.950, mountain: 0.010, rope: 0.010

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
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.13694903, Var_fd=0.13858779 (101.2%), Var_scope=0.00734369 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.739082 | 1.358606 | 0.8000 |
| moderate | 0.752415 | 1.106492 | 1.0000 |
| institutional | -0.028731 | -0.042252 | 1.0000 |
| analytical | 0.931553 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    maxent, cohomology, signature, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 50
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [severe]: type_1_false_summit detected for free_market_naturalization_wh_d1

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  Detected 1 constraint(s) falsely claiming "Mountain" status, across 4 observer-context instance(s):

  ┌─ CONSTRAINT: free_market_naturalization_wh_d1
  │  Context: context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))
  │
  │  Suppression Requirement: 0.71
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.71 > 0.05 threshold)
  │  Base Extractiveness: 0.68
  │  ✗ Shows extractive pattern (E > 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: SNARE
  │  → Rationale: High enforcement + high extraction = extractive trap
  └─

  ┌─ CONSTRAINT: free_market_naturalization_wh_d1
  │  Context: context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))
  │
  │  Suppression Requirement: 0.71
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.71 > 0.05 threshold)
  │  Base Extractiveness: 0.68
  │  ✗ Shows extractive pattern (E > 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: SNARE
  │  → Rationale: High enforcement + high extraction = extractive trap
  └─

  ┌─ CONSTRAINT: free_market_naturalization_wh_d1
  │  Context: context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national))
  │
  │  Suppression Requirement: 0.71
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.71 > 0.05 threshold)
  │  Base Extractiveness: 0.68
  │  ✗ Shows extractive pattern (E > 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: SNARE
  │  → Rationale: High enforcement + high extraction = extractive trap
  └─

  ┌─ CONSTRAINT: free_market_naturalization_wh_d1
  │  Context: context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))
  │
  │  Suppression Requirement: 0.71
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.71 > 0.05 threshold)
  │  Base Extractiveness: 0.68
  │  ✗ Shows extractive pattern (E > 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: SNARE
  │  → Rationale: High enforcement + high extraction = extractive trap
  └─


[STRUCTURAL SIGNATURE ANALYSIS]
  free_market_naturalization_wh_d1: false_natural_law (confidence: high)
    → FALSE NATURAL LAW signature for free_market_naturalization_wh_d1: Claims naturality (explicit_mountain_claim) but fails Boltzmann independence test. Coupling score=1.000 with 4 coupled dimension pairs. Excess extraction=0.53. This constraint is "physics-washed" — it appears natural but its coupling topology reveals structural construction.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  free_market_naturalization_wh_d1: R5 ZOMBIE CROSSCHECK: authored_zombie_uncorroborated

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [engineered_versus_lapsed_closure] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: free_market_naturalization_wh_d1
  │  Question: Did alternatives to market naturalization collapse because markets genuinely are natural law (lapsed-alternative naturalization), or because beneficiaries actively suppress non-market coordination systems (engineered closure)?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical analysis of policy battles over commons management, cooperative ownership, and public provisioning. If beneficiaries systematically funded opposition to alternatives and lobbied for their delegitimization, closure is engineered. If alternatives failed on their own coordination failures without beneficiary intervention, closure is lapsed.
  │
  │  IMPLICATIONS (authored):
  │  Engineered closure would reclassify from mountain to tangled_rope or snare. Lapsed closure would support the mountain claim. The temporal measurements showing rising suppression and theater suggest engineered closure, but the omega remains because the historical record is contested.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [coordination_extraction_separability] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: free_market_naturalization_wh_d1
  │  Question: Are the genuine coordination functions markets provide (information aggregation, specialization, trade) separable from the naturalization claim that forecloses alternatives?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative institutional analysis of economies that use market mechanisms for some domains while using non-market coordination for others. If mixed systems achieve coordination without naturalizing markets, the functions are separable.
  │
  │  IMPLICATIONS (authored):
  │  If separable, the naturalization claim is pure extraction riding on real coordination functions (tangled_rope). If inseparable, some measured extraction is the price of coordination itself (rope with high inherent cost).
  │
  │  Confidence without resolution: high
  └─

  ┌─ [beneficiary_dependence_strength] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: free_market_naturalization_wh_d1
  │  Question: How much of the beneficiaries' wealth and power depends specifically on market naturalization versus on market mechanisms themselves?
  │
  │  RESOLUTION METHOD (authored):
  │  Counterfactual analysis: if markets remained primary coordination mechanisms but the naturalization claim dissolved (making market design a live political question), how much would beneficiary positions erode? Strong dependence on naturalization specifically would indicate extraction; weak dependence would support coordination.
  │
  │  IMPLICATIONS (authored):
  │  Strong dependence would confirm beneficiaries extract from the naturalization claim itself, not just from market participation. Weak dependence would suggest the mountain claim is closer to accurate.
  │
  │  Confidence without resolution: low
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: financial_intermediaries (beneficiary: financial_intermediaries, n=3)
  Members: free_market_naturalization_fed_d2, free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_natural_law
    Constraints:       free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 2 constraints indicates active systemic instability, not constraint-local drift.

  Set: incumbent_capital_holders (beneficiary: incumbent_capital_holders, n=4)
  Members: free_market_naturalization_fed_d3, free_market_naturalization_wh_d1, free_market_naturalization_wh_d2, free_market_naturalization_wh_d3

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_natural_law
    Constraints:       free_market_naturalization_fed_d3, free_market_naturalization_wh_d1, free_market_naturalization_wh_d2, free_market_naturalization_wh_d3

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  free_market_naturalization_fed_d3, free_market_naturalization_wh_d1, free_market_naturalization_wh_d2, free_market_naturalization_wh_d3

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 4 constraints indicates active systemic instability, not constraint-local drift.

  Set: market_infrastructure_operators (beneficiary: market_infrastructure_operators, n=2)
  Members: free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_natural_law
    Constraints:       free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  free_market_naturalization_fed_d3, free_market_naturalization_wh_d1

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 2 constraints indicates active systemic instability, not constraint-local drift.
