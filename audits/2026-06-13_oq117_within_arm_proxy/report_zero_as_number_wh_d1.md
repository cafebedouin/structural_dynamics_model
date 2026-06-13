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
[SCENARIO MANAGER] Loading: testsets/zero_as_number_wh_d1.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: zero_as_number_wh_d1...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: zero_as_number_wh_d1

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: zero_as_number_wh_d1 (0-800)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 800 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 800 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 800 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 800 in zero_as_number_wh_d1 (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] zero_as_number_wh_d1: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  zero_as_number_wh_d1:
    [watch | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.9760000000000001,decline_signals,[excess_above_floor(0.06)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  zero_as_number_wh_d1 (ε=0.08):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.08 × 1.36 × 0.80 = 0.087
    moderate@national: d=0.700 f(d)=1.11 χ = 0.08 × 1.11 × 1.00 = 0.089
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.08 × -0.04 × 1.00 = -0.003
    analytical@global: d=0.720 f(d)=1.14 χ = 0.08 × 1.14 × 1.20 = 0.110

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: zero_as_number_wh_d1 ===
  Shift (computed via dr_type/3):
    powerless=rope  moderate=rope  institutional=rope  analytical=rope
  Properties: [asymmetric,coordination,has_beneficiaries,natural]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=concentrated
  Drift:      extraction=falling  suppression=falling  theater=falling
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.976 (pristine)




╔══════════════════════════════════════════════════════════════════════╗
║  VERDICT: RED                                                        ║
║  BASE: GREEN (12/12 subsystems checked — no tensions) — CAPPED TO RED║
║  ! [moderate] signature_correction (signature_grade)                 ║
║  ! [severe] type_1_false_summit (claim_mismatch)                     ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)              ║
╚══════════════════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     mountain
    Signature:        coupling_invariant_rope
    Purity:           0.976 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Live index:       none (scope=0, power=0)
    Drift events:     1 — purity_drift [watch]


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9760
    Effective purity:   0.9760
    Propagation delta:  +0.0000

    Network neighbors (3):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | zero_as_number_fed_d1 | rope | shared_beneficiary | corpus-derived | low | 0.30 | 0.9760 |
    | zero_as_number_fed_d3 | rope | shared_beneficiary | corpus-derived | salient | 0.60 | 0.9760 |
    | zero_as_number_wh_d2 | rope | shared_beneficiary | corpus-derived | salient | 0.60 | 0.9760 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  No significant contamination — purity unchanged across 3 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  [enriched_omega_data.json not available]

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       rope (powerless)
    MaxEnt P(claimed): 0.0100 (borderline)
    Rival Type:       rope (P=0.9500)
    Margin:           -0.9400
    Boundary:         mountain->rope
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.6029]

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: rope at P=0.9500 (pipeline says mountain)
  MaxEnt P(claimed): 0.0100 (borderline)
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
  Var_total=0.00189547, Var_fd=0.00191817 (101.2%), Var_scope=0.00010164 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.086951 | 1.358606 | 0.8000 |
| moderate | 0.088519 | 1.106492 | 1.0000 |
| institutional | -0.003380 | -0.042252 | 1.0000 |
| analytical | 0.109594 | 1.141609 | 1.2000 |


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

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: marked_revision

  Marked Revision: The kernel is precisely specified; authority is voluntary and grounded in expertise. Drift is formalized as a proposal-check-absorb cycle — acknowledgment is marked and legible rather than silently absorbed. Mathematics works this way; healthy long-term relationships and programming languages with formal deprecation procedures work this way. The pattern is stable when acknowledgment capacity matches environmental change rate. Failure mode: authority structure develops extraction stakes in kernel preservation that did not exist at founding.

  Structural signals: kernel_formalized, authority_expertise

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 800
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [severe]: type_1_false_summit detected for zero_as_number_wh_d1

[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]
  Detected 1 constraint(s) falsely claiming "Mountain" status, across 4 observer-context instance(s):

  ┌─ CONSTRAINT: zero_as_number_wh_d1
  │  Context: context(agent_power(analytical),time_horizon(civilizational),exit_options(analytical),spatial_scope(global))
  │
  │  Suppression Requirement: 0.12
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.12 > 0.05 threshold)
  │  Base Extractiveness: 0.08
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: zero_as_number_wh_d1
  │  Context: context(agent_power(institutional),time_horizon(generational),exit_options(arbitrage),spatial_scope(national))
  │
  │  Suppression Requirement: 0.12
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.12 > 0.05 threshold)
  │  Base Extractiveness: 0.08
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: zero_as_number_wh_d1
  │  Context: context(agent_power(moderate),time_horizon(biographical),exit_options(mobile),spatial_scope(national))
  │
  │  Suppression Requirement: 0.12
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.12 > 0.05 threshold)
  │  Base Extractiveness: 0.08
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─

  ┌─ CONSTRAINT: zero_as_number_wh_d1
  │  Context: context(agent_power(powerless),time_horizon(biographical),exit_options(trapped),spatial_scope(local))
  │
  │  Suppression Requirement: 0.12
  │  Mountain Ceiling (threshold): 0.05
  │
  │  FAILURE ANALYSIS:
  │  ✗ Requires active enforcement (suppression 0.12 > 0.05 threshold)
  │  Base Extractiveness: 0.08
  │  ✓ Non-extractive pattern (E ≤ 0.35)
  │  Resistance to Change: MISSING (using default 0.0)
  │
  │  FORENSIC VERDICT:
  │  → Should be classified as: ROPE
  │  → Rationale: Requires enforcement but not extractive = changeable rule
  └─


[STRUCTURAL SIGNATURE ANALYSIS]
  zero_as_number_wh_d1: coupling_invariant_rope (confidence: medium)
    → COUPLING-INVARIANT ROPE signature for zero_as_number_wh_d1: Certified true coordination mechanism. Boltzmann compliance=compliant(0), scope invariance=invariant, excess extraction=0.060. Passes all structural purity tests — this is genuine coordination, not low-extraction construction.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  zero_as_number_wh_d1: R5 ZOMBIE CROSSCHECK: authored_zombie_uncorroborated

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [discovery_vs_construction] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: zero_as_number_wh_d1
  │  Question: Is zero as a number a discovered structural feature of mathematical reality, or a constructed convention that Western mathematics adopted for operational convenience?
  │
  │  RESOLUTION METHOD (authored):
  │  Philosophical analysis of mathematical realism vs. conventionalism, cross-cultural comparison of number systems, and examination of whether zero's properties are derivable from more fundamental axioms or are stipulated.
  │
  │  IMPLICATIONS (authored):
  │  If zero is discovered (realist reading), the constraint is a genuine mountain and the extraction measured is the cost of recognizing a pre-existing structure. If zero is constructed (conventionalist reading), the constraint is a coordination mechanism (rope or scaffold) that became entrenched, and the beneficiaries are traditions that gained authority from the convention's adoption.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [geometric_tradition_displacement] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: zero_as_number_wh_d1
  │  Question: Was the geometric tradition's conceptual framework genuinely inadequate for mathematical progress, or was it displaced by a competing tradition that benefited from zero's adoption?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical counterfactual analysis: could Greek geometric methods have been extended to handle the problems zero solves (place-value notation, algebraic closure) without admitting zero as a number? Examination of alternative mathematical traditions (e.g., Chinese rod calculus) that achieved similar results with different foundational concepts.
  │
  │  IMPLICATIONS (authored):
  │  If the geometric framework was genuinely inadequate, the extraction is the necessary cost of progress (mountain). If it was displaced by a competing tradition, the extraction is the cost of a coordination shift that benefited one tradition at another's expense (rope or tangled rope).
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [beneficiary_identification_ambiguity] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: zero_as_number_wh_d1
  │  Question: Are the named beneficiaries (algebraic formalism, computational mathematics) real actors that gained from zero's admission, or are they reifications of mathematical practices that do not collect rents?
  │
  │  RESOLUTION METHOD (authored):
  │  Examination of whether mathematical traditions can be beneficiaries in the same sense as human actors or institutions. If traditions are not actors, the constraint has no beneficiaries and the FSM trigger is a category error.
  │
  │  IMPLICATIONS (authored):
  │  If traditions are not actors, the constraint is a genuine mountain with no beneficiaries, and the FSM trigger should not apply. If traditions are actors (in the sense that they coordinate resources, exclude alternatives, and shape careers), the FSM analysis is appropriate and the constraint may be a false summit.
  │
  │  Confidence without resolution: medium
  └─

====================================================
