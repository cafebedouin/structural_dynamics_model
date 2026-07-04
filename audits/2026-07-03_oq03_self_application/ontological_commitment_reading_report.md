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
[SCENARIO MANAGER] Loading: testsets/ontological_commitment_reading.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: ontological_commitment_reading...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: ontological_commitment_reading

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: ontological_commitment_reading (0-25)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 25 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 25 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 25 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 25 in ontological_commitment_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] ontological_commitment_reading: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  ontological_commitment_reading:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,25,0.45,0.68)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.66)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  ontological_commitment_reading -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  ontological_commitment_reading (ε=0.68):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.68 × 1.36 × 0.80 = 0.739
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: ontological_commitment_reading ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.25))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝

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
 *ontological_commitment_reading [terminal(if_authored_ack)=husk, unacknowledged]
  vocabulary_collision_reading [terminal(if_authored_ack)=stable_pattern]
  measurement_architecture_reading [terminal(if_authored_ack)=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=7)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]
    Tangled psi:      0.9990 (snare_leaning)
    Coalition:        institutional_dissent



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

    Network neighbors (4):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | intervention_target_selection | snare | shared_victim | corpus-derived | low | 0.30 | 0.3542 |
    | measurement_architecture_reading | scaffold | explicit | authored | salient | 1.00 | 0.6488 |
    | seat_gauge_orientation_kernel_flat_control | unknown | shared_beneficiary | corpus-derived | low | 0.30 | 0.3912 |
    | vocabulary_collision_reading | scaffold | explicit | authored | salient | 1.00 | 0.6539 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  No significant contamination — purity unchanged across 4 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_ontological_commitment_reading
    Severity Score:    0.716
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F028

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional)
    MaxEnt P(claimed): 0.0056 (borderline)
    Rival Type:       snare (P=0.9944)
    Margin:           -0.9888
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.9944 (pipeline says tangled_rope)
  MaxEnt P(claimed): 0.0056 (borderline)
  Rival Type:    snare (P=0.9944)
  Margin:        -0.9888
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

  stability not assessed — kernel 'seat_gauge_orientation_kernel' has no confirmed governing params yet
  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.68, grid distance 0.22)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

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
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **6 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: marked_revision

  Marked Revision: The kernel is precisely specified; authority is voluntary and grounded in expertise. Drift is formalized as a proposal-check-absorb cycle — acknowledgment is marked and legible rather than silently absorbed. Mathematics works this way; healthy long-term relationships and programming languages with formal deprecation procedures work this way. The pattern is stable when acknowledgment capacity matches environmental change rate. Failure mode: authority structure develops extraction stakes in kernel preservation that did not exist at founding.

  Structural signals: kernel_formalized, authority_expertise

  ⚠ false_marked_revision: Signals conflict with Marked Revision claim: suppression, theater, or enforcement patterns suggest the revision mechanism is not functioning or is not voluntary.

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor (if authored acknowledgment taken at face value): husk
  Drift state: unacknowledged

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 25
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for ontological_commitment_reading

[STRUCTURAL SIGNATURE ANALYSIS]
  ontological_commitment_reading: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for ontological_commitment_reading: Enforcement present (suppression=0.72, resistance=0.61) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  ontological_commitment_reading: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [irreducibility_empirical_vs_stipulative] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: ontological_commitment_reading
  │  Question: Is the irreducibility of seat-gauge-orientation an empirical discovery about measurement structure, or a stipulative definition that serves to organize a research program?
  │
  │  RESOLUTION METHOD (authored):
  │  Cross-domain measurement practice: if successful measurement in some domains routinely collapses the roles without loss of rigor, irreducibility is stipulative rather than discovered. If all rigorous measurement preserves the distinctions, it is empirical.
  │
  │  IMPLICATIONS (authored):
  │  If stipulative, the ontological reading is extractive boundary maintenance rather than conceptual necessity. If empirical, the enforcement is justified by the structure of measurement itself.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [seat_primacy_vs_coequality] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: ontological_commitment_reading
  │  Question: Does seat hold metaphysical priority over gauge and orientation, or are the three roles co-equal with seat's apparent priority an artifact of formal epistemology's observer-centric tradition?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative analysis of measurement architectures from gauge-first sciences (experimental physics, where instrument choice precedes observer role) and orientation-first interpretive traditions (hermeneutics, where stance precedes both). If those traditions produce equally rigorous measurement without seat-primacy, priority is tradition-specific.
  │
  │  IMPLICATIONS (authored):
  │  If seat-primacy is tradition-specific, the ontological reading extracts from practitioners in gauge-first or orientation-first domains by forcing them into an unnatural architecture. If seat is genuinely prior, the extraction is the cost of conceptual rigor.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [collapsibility_conditions] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: ontological_commitment_reading
  │  Question: Are there domain-specific conditions under which collapsing seat-gauge-orientation roles is legitimate, or is non-collapsibility a universal constraint on rigorous measurement?
  │
  │  RESOLUTION METHOD (authored):
  │  Systematic review of measurement practices in domains where role-collapsing is routine (e.g., automated measurement systems where seat and gauge are fused, or participatory action research where seat and orientation are deliberately merged). If those practices produce valid measurements, collapsibility is domain-dependent.
  │
  │  IMPLICATIONS (authored):
  │  If collapsibility is sometimes legitimate, the ontological reading's universal non-collapsibility claim is over-general and its enforcement extracts from domains where collapsing is appropriate. If non-collapsibility is universal, the enforcement prevents conceptual confusion.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [omega_extraction_blindness_ontological_commitment_reading] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: ontological_commitment_reading
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint ontological_commitment_reading computes as extractive (snare) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: network_db1a3b31 (network adjacency, n=3)
  Members: measurement_architecture_reading, ontological_commitment_reading, vocabulary_collision_reading

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_ci_rope
    Constraints:       measurement_architecture_reading, vocabulary_collision_reading
