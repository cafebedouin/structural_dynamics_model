HOW TO READ THIS REPORT
  Purpose: surface the SEATS a constraint is read from — not to issue a verdict.
  The engine classifies the SAME constraint from multiple observer positions
  (powerless / moderate / institutional / analytical) and contrasts those readings
  with what the story AUTHORS in commentary. Divergence — between seats, or between
  a seat and the authored claim — IS the finding; a per-seat type is that seat's
  structural reading, not a ranking.
  A RED verdict (e.g. dataset_recycling) flags the AUTHORED victim/beneficiary
  DIRECTION (OQ-187), not a seat-free moral judgment. χ = ε × f(d) × σ(S) is computed
  per seat; d is a function of the observer POSITION (a config lookup), not authored
  per story — identical d across constraints for the same position is by design.

CORPUS CONTEXT: 128 constraints
  Types: 18 mountain, 28 rope, 55 tangled_rope, 12 snare, 1 piton, 5 scaffold
  Network stability: cascading | 60 omegas (59 critical)
  MaxEnt bands (corpus): 24 deep (20%) | 2 moderate (2%) | 93 borderline (78%)
  CS patterns: 93 classified | 3 anchored_fixity_with_accretion, 15 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 26 interpretive_accretion, 8 marked_revision | 56 verdicts fired
  CS grounding mismatches: 74 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/epsilon_substrate_dependency.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: epsilon_substrate_dependency...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: epsilon_substrate_dependency

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: epsilon_substrate_dependency (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in epsilon_substrate_dependency (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] epsilon_substrate_dependency: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  epsilon_substrate_dependency:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.48,0.68)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.6000000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  epsilon_substrate_dependency -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  epsilon_substrate_dependency (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: epsilon_substrate_dependency ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.29))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      no_route
      institutional: author_engine_divergence
      analytical:    no_route
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=8)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_epsilon_substrate_dependency
    Severity Score:    0.716
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F016

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), snare (moderate), rope (institutional)
    MaxEnt P(claimed): 0.9944 (deep)
    Rival Type:       tangled_rope (P=0.0056)
    Margin:           +0.9888
    Boundary:         snare->tangled_rope
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → tangled_rope; institutional → rope.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9944 (deep)
  Rival Type:    tangled_rope (P=0.0056)
  Margin:        +0.9888
  Entropy:       0.0193
  Distribution:  snare: 0.994, tangled_rope: 0.006

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9966)
  Entropy:       0.0126
  Distribution:  snare: 0.997, tangled_rope: 0.003

  Classical/Indexed TV Distance: 0.0022 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): FLAGGED (ε=0.68, grid distance 0.22):
    - unstable_off_grid: final type flips under ε±r though ε is >r from every ε-threshold (a χ-gate crossing, not an ε-threshold one)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | classical_oracle_failure | 0.78 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.13773031, Var_fd=0.10553269 (76.6%), Var_scope=0.00471409 (3.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.353600 | 0.650000 | 0.8000 |
| moderate | 0.752415 | 1.106492 | 1.0000 |
| institutional | -0.028731 | -0.042252 | 1.0000 |
| analytical | 0.931553 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (3):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.78,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Both Hubs): H^1 >= 5 means both Hub 1 (power-scaled extraction) and Hub 2 (effective immutability) contribute to classification fracture. Three or more distinct types appear across observers.

  **6 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: marked_revision

  Marked Revision: The kernel is precisely specified; authority is voluntary and grounded in expertise. Drift is formalized as a proposal-check-absorb cycle — acknowledgment is marked and legible rather than silently absorbed. Mathematics works this way; healthy long-term relationships and programming languages with formal deprecation procedures work this way. The pattern is stable when acknowledgment capacity matches environmental change rate. Failure mode: authority structure develops extraction stakes in kernel preservation that did not exist at founding.

  Structural signals: kernel_formalized, authority_expertise

  ⚠ false_marked_revision: Signals conflict with Marked Revision claim: suppression, theater, or enforcement patterns suggest the revision mechanism is not functioning or is not voluntary.

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor (if authored acknowledgment taken at face value): stable_pattern

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for epsilon_substrate_dependency

[STRUCTURAL SIGNATURE ANALYSIS]
  epsilon_substrate_dependency: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for epsilon_substrate_dependency: Enforcement present (suppression=0.72, resistance=0.55) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  epsilon_substrate_dependency: R5 Q6 CROSSCHECK: live_claim_vs_snare_present daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 5 resolution scenario(s):

  ┌─ [epsilon_generation_variance] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: epsilon_substrate_dependency
  │  Question: What is the actual variance in epsilon values across independent story generators for the same constraint, and does it exceed the variance the framework's declaration discipline can tolerate?
  │
  │  RESOLUTION METHOD (authored):
  │  Systematic A/B generation study with multiple generators (human and AI) authoring epsilon for identical constraint scenarios, measuring inter-generator variance and comparing to intra-generator variance and to the epsilon differences that flip classification types.
  │
  │  IMPLICATIONS (authored):
  │  High variance that exceeds type-flip thresholds would establish epsilon substrate dependency as producing unreliable classifications; low variance would support the architects' claim that declaration discipline constrains judgment sufficiently. Variance concentrated in contested-kernel cases would support the necessity claim; variance uniform across all constraint types would indicate systematic ungroundedness.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [world_anchor_feasibility] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: epsilon_substrate_dependency
  │  Question: Could epsilon be computed from world-anchored observables (power asymmetry, exit-option distribution, enforcement cost) without losing the capacity to model reading-dependent classification?
  │
  │  RESOLUTION METHOD (authored):
  │  Prototype implementation of world-anchor epsilon computation with explicit reading-dependent modifiers; test whether it can reproduce the classification divergence the current judgment-authored approach produces for known kernel families (BGS, capital punishment, constitutional authority).
  │
  │  IMPLICATIONS (authored):
  │  If feasible, world-anchor computation would eliminate the substrate dependency while preserving reading-relativity, exposing the current architecture as extractive rather than necessary. If infeasible, it would vindicate the architects' necessity claim and reclassify the constraint as tangled rope (genuine coordination with unavoidable extraction).
  │
  │  Confidence without resolution: low
  └─

  ┌─ [declaration_discipline_enforcement] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: epsilon_substrate_dependency
  │  Question: Does §6.4 declaration discipline (claim and metrics independent, no tuning) actually prevent epsilon gaming, or does it create a theater of rigor while leaving epsilon effectively unconstrained?
  │
  │  RESOLUTION METHOD (authored):
  │  Audit of generated constraint stories for correlation between claimed_type and authored epsilon: if epsilon systematically tracks claimed_type despite the independence requirement, declaration discipline is failing. Cross-check against operator ruling history for cases where epsilon was rejected as gamed.
  │
  │  IMPLICATIONS (authored):
  │  If discipline is effective, the theater ratio should be lower and the constraint reclassifies toward tangled rope (real enforcement cost). If discipline is theater, the theater ratio is accurate and the constraint remains snare (extraction masked as rigor).
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [committer_axis_necessity] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: epsilon_substrate_dependency
  │  Question: Is the committer axis (reading-dependent classification) a necessary feature for modeling contested kernels, or could the framework achieve the same analytical coverage through observer-axis diversity alone?
  │
  │  RESOLUTION METHOD (authored):
  │  Theoretical analysis of whether observer-axis power/exit variation can produce the same classification divergence that committer-axis reading variation produces; test on known kernel families whether observer-only modeling captures the structural differences readings claim to see.
  │
  │  IMPLICATIONS (authored):
  │  If observer-axis suffices, the committer axis and its epsilon substrate dependency are unnecessary complexity, and the constraint reclassifies as pure extraction. If committer-axis is necessary, it supports the architects' coordination framing and shifts classification toward tangled rope.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_epsilon_substrate_dependency] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: epsilon_substrate_dependency
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint epsilon_substrate_dependency computes as extractive (snare) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================
