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
[SCENARIO MANAGER] Loading: testsets/vocabulary_collision_reading.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: vocabulary_collision_reading...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: vocabulary_collision_reading

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: vocabulary_collision_reading (0-10)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 10 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 10 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 10 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 10 in vocabulary_collision_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] vocabulary_collision_reading: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  1
  Critical: 0 | Warning: 0 | Watch: 1

  vocabulary_collision_reading:
    [watch | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.9480000000000001,decline_signals,[excess_above_floor(0.13)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  vocabulary_collision_reading (ε=0.18):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.18 × 1.39 × 0.80 = 0.201
    moderate@national: d=0.650 f(d)=1.01 χ = 0.18 × 1.01 × 1.00 = 0.182
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.18 × -0.04 × 1.00 = -0.008
    analytical@global: d=0.720 f(d)=1.14 χ = 0.18 × 1.14 × 1.20 = 0.247

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: vocabulary_collision_reading ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=scaffold  institutional=scaffold  analytical=scaffold
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=stable  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.948 (pristine)




╔═════════════════════════════════════════════════════════════════════════╗
║  VERDICT: RED                                                           ║
║  12/12 subsystems — 4 tension(s) (maxent, abductive, dirac, context_gap)║
║  ! [informational] signature_correction (signature_grade)               ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)                 ║
╚═════════════════════════════════════════════════════════════════════════╝
  ⓘ RED extraction = a statement about AUTHORED directionality d (victim/beneficiary/ε), not a seat-free moral verdict; the engine does not adjudicate the contested direction (trap vs cost-of-exit). Standing note — OQ-187.

--- KERNEL: seat_gauge_orientation_kernel ---
  3 readings | 2 diverging pairs | 0 axiom conflicts
  Reading disagreement: type_a_drift [within_kernel; obstruction=untyped, drift_unacknowledged]
  Reading robustness: 0 agree (real-typed) / 156 diverge / 0 undetermined  (of 156 contexts)
  H1 across readings: reading-specific [ontological_commitment_reading=3, vocabulary_collision_reading=0, measurement_architecture_reading=0]
    Jaccard(ontological_commitment_reading, vocabulary_collision_reading) = 0.000  [agree 0, diverge 156]
    Jaccard(ontological_commitment_reading, measurement_architecture_reading) = 0.000  [agree 0, diverge 156]
    Jaccard(vocabulary_collision_reading, measurement_architecture_reading) = 1.000  [agree 156, diverge 0]
    diverges: measurement_architecture_reading=scaffold / ontological_commitment_reading=snare / vocabulary_collision_reading=scaffold (94 contexts)
    diverges: measurement_architecture_reading=scaffold / ontological_commitment_reading=tangled_rope / vocabulary_collision_reading=scaffold (23 contexts)
    diverges: measurement_architecture_reading=scaffold / ontological_commitment_reading=rope / vocabulary_collision_reading=scaffold (21 contexts)
    diverges: measurement_architecture_reading=scaffold / ontological_commitment_reading=naturalized / vocabulary_collision_reading=scaffold (18 contexts)
  ontological_commitment_reading [terminal(if_authored_ack)=husk, unacknowledged]
 *vocabulary_collision_reading [terminal(if_authored_ack)=stable_pattern]
  measurement_architecture_reading [terminal(if_authored_ack)=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        false_ci_rope  (canonical stance · twin-agreement 0.722)
    Purity:           0.948 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Live index:       none (scope=0, power=0)
    Drift events:     2 — purity_drift [watch], network_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9480
    Effective purity:   0.6539
    Propagation delta:  -0.2941

    Network neighbors (2):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | measurement_architecture_reading | scaffold | explicit | authored | salient | 1.00 | 0.6488 |
    | ontological_commitment_reading | snare | explicit | authored | salient | 1.00 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  Purity degraded from 0.9480 to 0.6539 by contamination from 2 neighbor(s), primarily ontological_commitment_reading (explicit, authored, purity 0.3542).

--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   no gap (seats examined, comparable, agree)

  No gap-omega for this constraint (see gap operability above — an undetermined/no_gap constraint mints no gap-omega, by design).

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       scaffold (powerless)
    MaxEnt P(claimed): 0.0733 (borderline)
    Rival Type:       piton (P=0.6733)
    Margin:           -0.6000
    Boundary:         rope->piton
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.3935]

--- MAXENT SHADOW CLASSIFICATION ---

  MAXENT FAVORS RIVAL: piton at P=0.6733 (pipeline says rope)
  MaxEnt P(claimed): 0.0733 (borderline)
  Rival Type:    piton (P=0.6733)
  Margin:        -0.6000
  Entropy:       0.4497
  Distribution:  piton: 0.673, scaffold: 0.253, rope: 0.073

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      piton (P=0.6733)
  Entropy:       0.4497
  Distribution:  piton: 0.673, scaffold: 0.253, rope: 0.073

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — kernel 'seat_gauge_orientation_kernel' has no confirmed governing params yet
  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.18, grid distance 0.07)

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | metric_structural_divergence | 0.73 | high_entropy_preserved_orbit | genuine | MaxEnt sees ambiguity (high entropy) but Dirac orbit is unambiguous — metric uncertainty without structural uncertainty. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.00940282, Var_fd=0.00970826 (103.2%), Var_scope=0.00049638 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.200584 | 1.392945 | 0.8000 |
| moderate | 0.181551 | 1.008614 | 1.0000 |
| institutional | -0.007605 | -0.042252 | 1.0000 |
| analytical | 0.246588 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    cohomology, signature, boltzmann, purity, fingerprint_voids, drift, gauge_orbit

  Expected Conflicts (1):
    fcr_gate: fcr_gate_deferral
      FCR gate deferred override due to metric perspectival variance

  Convergent Rejections: none

  Tensions (4):
    maxent: hard(piton,scaffold)
    abductive: abductive_tension([trigger(metric_structural_divergence,0.73,high_entropy_preserved_orbit,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])
    dirac: dirac_mismatch(first_class,scaffold)
    context_gap: restricted_mismatch(rope,scaffold)

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: marked_revision

  Marked Revision: The kernel is precisely specified; authority is voluntary and grounded in expertise. Drift is formalized as a proposal-check-absorb cycle — acknowledgment is marked and legible rather than silently absorbed. Mathematics works this way; healthy long-term relationships and programming languages with formal deprecation procedures work this way. The pattern is stable when acknowledgment capacity matches environmental change rate. Failure mode: authority structure develops extraction stakes in kernel preservation that did not exist at founding.

  Structural signals: kernel_formalized, authority_expertise

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor (if authored acknowledgment taken at face value): stable_pattern

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 10
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  vocabulary_collision_reading: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for vocabulary_collision_reading: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.13)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: framework_users
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  vocabulary_collision_reading: R5 Q6 CROSSCHECK: q6_unclassified daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

  ┌─ [vocabulary_vs_ontology] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: vocabulary_collision_reading
  │  Question: Is the v8 change purely terminological (renaming to eliminate collision) or does it reflect a deeper shift in what the framework takes to be the primary analytical object?
  │
  │  RESOLUTION METHOD (authored):
  │  Examine whether engine behavior changed between v7 and v8: if the formalism is identical and only labels changed, the shift is terminological; if classification logic or measurement procedures changed, the shift is ontological.
  │
  │  IMPLICATIONS (authored):
  │  If purely terminological, this reading is correct and the constraint is a low-extraction coordination mechanism. If ontological, the ontological_commitment_reading is correct and the constraint reflects a substantive reframing of the framework's commitments.
  │
  │  Confidence without resolution: high
  └─

  ┌─ [transition_cost_distribution] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: vocabulary_collision_reading
  │  Question: Does the one-time coordination cost of adopting v8 terminology fall symmetrically on all framework users, or does it disproportionately burden certain user classes?
  │
  │  RESOLUTION METHOD (authored):
  │  Survey framework users by experience level and application domain to measure adoption friction; if advanced users or specific domains face higher transition costs, the coordination is asymmetric.
  │
  │  IMPLICATIONS (authored):
  │  If transition costs are symmetric, the low extractiveness score is accurate. If costs are asymmetric, effective extraction may be higher for burdened user classes.
  │
  │  Confidence without resolution: medium
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: network_db1a3b31 (network adjacency, n=3)
  Members: measurement_architecture_reading, ontological_commitment_reading, vocabulary_collision_reading

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_ci_rope
    Constraints:       measurement_architecture_reading, vocabulary_collision_reading
