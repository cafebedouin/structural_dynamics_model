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
[SCENARIO MANAGER] Loading: testsets/seat_gauge_orientation_kernel_flat_control.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: seat_gauge_orientation_kernel_flat_control...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: seat_gauge_orientation_kernel_flat_control

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: seat_gauge_orientation_kernel_flat_control (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in seat_gauge_orientation_kernel_flat_control (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] seat_gauge_orientation_kernel_flat_control: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 0 | Warning: 3 | Watch: 0

  seat_gauge_orientation_kernel_flat_control:
    [warning | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.35,0.42)
    [warning | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3941666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.39999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  seat_gauge_orientation_kernel_flat_control -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  seat_gauge_orientation_kernel_flat_control (ε=0.42):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.42 × 1.39 × 0.80 = 0.468
    moderate@national: d=0.650 f(d)=1.01 χ = 0.42 × 1.01 × 1.00 = 0.424
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.42 × -0.04 × 1.00 = -0.018
    analytical@global: d=0.720 f(d)=1.14 χ = 0.42 × 1.14 × 1.20 = 0.575

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: seat_gauge_orientation_kernel_flat_control ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=scaffold  institutional=scaffold  analytical=unknown
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=low  suppression=moderate
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,moderate,global-national,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.25))
  Purity:     0.394 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, boltzmann)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     engine_abstained
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    engine_abstained
    Signature:        false_ci_rope  (canonical stance · twin-agreement 0.722)
    Purity:           0.394167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=5)
    Drift events:     4 — extraction_accumulation [warning], coupling_drift [warning], purity_drift [warning], network_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3942
    Effective purity:   0.3912
    Propagation delta:  -0.0030

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | ontological_commitment_reading | snare | shared_beneficiary | corpus-derived | low | 0.30 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  Purity degraded from 0.3942 to 0.3912, but the contamination is carried entirely by low-salience corpus-derived edges (1 neighbor(s), each a single shared agent / no authored link). No connection here is asserted by this case's source material.

--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold, unknown]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   no gap (seats examined, comparable, agree)

  No gap-omega for this constraint (see gap operability above — an undetermined/no_gap constraint mints no gap-omega, by design).

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       unknown (powerless), scaffold (moderate)
    MaxEnt P(claimed): 0.8452 (deep)
    Rival Type:       scaffold (P=0.1457)
    Margin:           +0.6995
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.3935]

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.8452 (deep)
  Rival Type:    scaffold (P=0.1457)
  Margin:        +0.6995
  Entropy:       0.2598
  Distribution:  rope: 0.845, scaffold: 0.146, tangled_rope: 0.009

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.8399)
  Entropy:       0.2737
  Distribution:  rope: 0.840, scaffold: 0.145, tangled_rope: 0.015

  Classical/Indexed TV Distance: 0.0062 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): FLAGGED (ε=0.42, grid distance 0.03):
    - unstable_off_grid: final type flips under ε±r though ε is >r from every ε-threshold (a χ-gate crossing, not an ε-threshold one)

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | convergent_structural_stress | 0.90 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.05119324, Var_fd=0.05285610 (103.2%), Var_scope=0.00270254 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.468030 | 1.392945 | 0.8000 |
| moderate | 0.423618 | 1.008614 | 1.0000 |
| institutional | -0.017746 | -0.042252 | 1.0000 |
| analytical | 0.575371 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    cohomology, purity, dirac, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (3):
    maxent: residual_type_artifact
      Residual types not in MaxEnt 6-type model; disagreement is mechanistic
    signature: fcr_deferred_signature_mismatch
      FCR override target mismatch; gate deferred due to perspectival variance
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (2):
    abductive: abductive_tension([trigger(convergent_structural_stress,0.9,multi_signal_convergence,genuine)])
    boltzmann: non_compliant(1.0,0.25,unknown)

--- THEOREM INSTANTIATION ---

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **2 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for seat_gauge_orientation_kernel_flat_control

[STRUCTURAL SIGNATURE ANALYSIS]
  seat_gauge_orientation_kernel_flat_control: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for seat_gauge_orientation_kernel_flat_control: Appears to be rope (explicit_rope_claim) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(1.0,0.25),scope_variant([scaffold,unknown]),excess_above_floor(0.39999999999999997),nonsensical_coupling(0.3333333333333333)]. Coupling score=1.0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: formal_system_architects
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  seat_gauge_orientation_kernel_flat_control: R5 Q6 CROSSCHECK: q6_signature_unknown daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [ontological_vs_terminological] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: seat_gauge_orientation_kernel_flat_control
  │  Question: Are seat, gauge, and orientation three ontologically distinct roles in measurement architecture, or are they a terminological choice that could be collapsed into fewer primitives without loss of expressive power?
  │
  │  RESOLUTION METHOD (authored):
  │  Formal proof that a two-role system cannot express the same audit distinctions, or a worked two-role system that achieves equivalent disambiguation. The minimalist claim is falsifiable: if no two-role system can trace framing disputes separately from empirical disputes, the three-role structure is ontologically necessary.
  │
  │  IMPLICATIONS (authored):
  │  If ontologically distinct, the architecture is genuine coordination solving an irreducible measurement problem. If terminologically redundant, the structure is extractive overhead—practitioners pay a learning cost for a distinction that could be achieved more simply.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [asymmetry_necessity] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: seat_gauge_orientation_kernel_flat_control
  │  Question: Is the asymmetry between seat/gauge (symmetric measurement inputs) and orientation (interpretive frame) structurally necessary for audit traceability, or could a fully symmetric three-role system achieve the same disambiguation?
  │
  │  RESOLUTION METHOD (authored):
  │  Operational test: build a symmetric three-role system and attempt to trace framing disputes separately from empirical disputes. If the symmetric system cannot cleanly separate the two dispute types, the asymmetry is necessary.
  │
  │  IMPLICATIONS (authored):
  │  If the asymmetry is necessary, it is a structural feature of measurement architecture, not a design choice. If a symmetric system works equally well, the asymmetry is extractive—it concentrates interpretive authority with orientation-specifiers without functional justification.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [learning_cost_vs_disambiguation_benefit] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: seat_gauge_orientation_kernel_flat_control
  │  Question: Does the disambiguation power the three-role structure provides justify the learning cost it imposes on practitioners, or is the cost disproportionate to the benefit?
  │
  │  RESOLUTION METHOD (authored):
  │  Empirical study of practitioner error rates and dispute resolution times in systems with and without explicit role separation. If explicit separation reduces errors and accelerates resolution, the cost is justified; if outcomes are equivalent, the cost is pure overhead.
  │
  │  IMPLICATIONS (authored):
  │  If the benefit justifies the cost, the structure is efficient coordination. If the cost exceeds the benefit, the structure is extractive—it imposes unnecessary complexity that benefits architects and theorists more than practitioners.
  │
  │  Confidence without resolution: low
  └─

====================================================
