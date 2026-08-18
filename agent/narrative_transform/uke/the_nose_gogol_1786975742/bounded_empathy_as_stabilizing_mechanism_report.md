HOW TO READ THIS REPORT
  Purpose: surface the SEATS a constraint is read from — not to issue a verdict.
  The engine classifies the SAME constraint from multiple observer positions
  (powerless / moderate / institutional / analytical) and contrasts those readings
  with what the story AUTHORS in commentary. Divergence — between seats, or between
  a seat and the authored claim — IS the finding; a per-seat type is that seat's
  structural reading, not a ranking.
  A RED verdict (e.g. dataset_recycling) flags the AUTHORED victim/beneficiary
  DIRECTION (OQ-187), not a seat-free moral judgment. χ = ε × f(d) × σ(S) is computed
  per seat; d (directionality) is DERIVED per seat — precedence: authored override →
  beneficiary/victim structure + exit_options → canonical power fallback. Only the
  fallback is a pure position→config lookup; when a story authors victims/beneficiaries
  (the common case) d comes from that authored structure, so d for the SAME position
  label can differ across constraints (institutional d ranged 0.12–0.72 across the
  drone set) — cross-constraint 'same seat' d-comparison is NOT apples-to-apples.
  ‡ on a seat's type (typically institutional) = the verdict flips under a single authored
  stakeholder-role change: the seat's authored role d and its nearest alternative role
  constant sit on opposite sides of the f(d) sign root (agenda_setter 0.12 ↔ beneficiary
  0.25 straddle d*≈0.164), so that seat's rope/not-rope reading is role-authored, not
  situation-measured. Standing note — OQ-188.

CORPUS CONTEXT: 276 constraints
  Types: 21 mountain, 50 rope, 139 tangled_rope, 23 snare, 4 piton, 13 scaffold
  Network severity: 51/58 drifting are severe (88%) [severe = effective purity < 0.70] | 138 omegas (131 critical)
  Purity coverage: 220/276 scorable, 56 unscored = 43 gate-fail (sentinel) + 13 no-data (no coordination_type)
  MaxEnt bands (corpus): 77 deep (31%) | 22 moderate (9%) | 151 borderline (60%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         2 borderline mountains
         1 contaminated mountains  -> cover-story candidates
         6 borderline ropes
        14 contaminated ropes  -> cover-story candidates
        29 borderline tangled_ropes
        14 borderline snares
         2 sound pitons
         2 pristine scaffolds
         2 sound scaffolds
         6 contaminated scaffolds
    Expected band by type  (REPORTING CONVENTION, not ground truth; logical_fingerprint:purity_zone vocabulary -- the SPEC bander, the one OQ-62 left named; 'contaminated' = [0.30,0.50)):
      mountain,rope -> pristine|sound ; tangled_rope,snare -> contaminated ; scaffold,piton -> mixed
    Full matrix  (per-row unscored split kept as gate-fail/no-data -- OQ-236 provenance):
        type           pristine sound border contam degrad | scored  gf  nd total
        mountain             14     1      2      1      0 |     18   3   0    21
        rope                 11     9      6     14      0 |     40   2   8    50
        tangled_rope          0     0     29     97      0 |    126   9   4   139
        snare                 0     0     14      7      0 |     21   1   1    23
        piton                 0     2      2      0      0 |      4   0   0     4
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  26   0    26
        -----------------------------------------------------------------------
        totals               27    14     54    125      0 |    220  43  13   276
  SEVERITY x TYPE  (drifting subset, n_drifting=58; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              1       1     2      0 |      2        4
        piton                 1       0     1      0 |      1        2
        rope                  8       5     3      0 |     13       16
        scaffold              1       1     1      0 |      2        3
        snare                 0       3     0      0 |      3        3
        tangled_rope         16      14     0      0 |     30       30
  CS patterns: 180 classified | 8 anchored_fixity_with_accretion, 45 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 34 interpretive_accretion, 10 marked_revision | 78 verdicts fired
  CS grounding mismatches: 146 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/bounded_empathy_as_stabilizing_mechanism.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: bounded_empathy_as_stabilizing_mechanism...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: bounded_empathy_as_stabilizing_mechanism

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: bounded_empathy_as_stabilizing_mechanism (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in bounded_empathy_as_stabilizing_mechanism (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] bounded_empathy_as_stabilizing_mechanism: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  bounded_empathy_as_stabilizing_mechanism:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.44,0.61)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.4623333333333334,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.49)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  bounded_empathy_as_stabilizing_mechanism -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  bounded_empathy_as_stabilizing_mechanism (ε=0.61):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.61 × 1.36 × 0.80 = 0.663
    moderate@national: d=0.700 f(d)=1.11 χ = 0.61 × 1.11 × 1.00 = 0.675
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.61 × -0.04 × 1.00 = -0.026
    analytical@global: d=0.720 f(d)=1.14 χ = 0.61 × 1.14 × 1.20 = 0.836

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: bounded_empathy_as_stabilizing_mechanism ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=high
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.462 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 1 tension(s) (abductive)              ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     tangled_rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     no_route
      moderate:      no_route
      institutional: author_engine_divergence
      analytical:    no_route
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.462333 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=2, power=7)
    Drift events:     4 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning], network_drift [warning]
    Tangled psi:      0.9791 (snare_leaning)
    Coalition:        institutional_dissent

--- TEMPORAL TRAJECTORY ---

    base_extractiveness: ceiling approach — rate 0.0125→0.0025 over T=0–24, flattening near 0.61


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4623
    Effective purity:   0.4191
    Propagation delta:  -0.0433

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | categorical_nonexistence_as_soft_denial | snare | shared_victim | corpus-derived | low | distinct | 1.00 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  Purity degraded from 0.4623 to 0.4191, but the contamination is carried entirely by low-salience corpus-derived edges (1 neighbor(s), each a single shared agent / no authored link). No connection here is asserted by this case's source material.

--- NETWORK POSITION (OQ-193) ---

  NOT ASSESSED — giant_component_analysis.raw.json not produced.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, tangled_rope]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_bounded_empathy_as_stabilizing_mechanism
    Severity Score:    0.599
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F028

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.9522 (deep)
    Rival Type:       snare (P=0.0468)
    Margin:           +0.9054
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see tangled_rope.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9522 (deep)
  Rival Type:    snare (P=0.0468)
  Margin:        +0.9054
  Entropy:       0.1098
  Distribution:  tangled_rope: 0.952, snare: 0.047, piton: 0.001

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      tangled_rope (P=0.9486)
  Entropy:       0.1160
  Distribution:  tangled_rope: 0.949, snare: 0.050, piton: 0.001

  Classical/Indexed TV Distance: 0.0036 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.61, grid distance 0.15)

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.11020503, Var_fd=0.11152361 (101.2%), Var_scope=0.00590957 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.663000 | 1.358606 | 0.8000 |
| moderate | 0.674960 | 1.106492 | 1.0000 |
| institutional | -0.025774 | -0.042252 | 1.0000 |
| analytical | 0.835658 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    maxent, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate

  Expected Conflicts (4):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])

--- THEOREM INSTANTIATION ---

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **5 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for bounded_empathy_as_stabilizing_mechanism

[STRUCTURAL SIGNATURE ANALYSIS]
  bounded_empathy_as_stabilizing_mechanism: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for bounded_empathy_as_stabilizing_mechanism: Enforcement present (suppression=0.58, resistance=0.47) with high extraction (0.61). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  bounded_empathy_as_stabilizing_mechanism: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [empathy_boundary_origin] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: bounded_empathy_as_stabilizing_mechanism
  │  Question: Is the boundary on adjudicator empathy an inherent requirement of consistent adjudication at scale, or a constructed limit that could be redrawn to admit more categories without sacrificing consistency?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative study of adjudicative bodies that have expanded categorical schemes over time versus those that have held them fixed — track whether consistency and caseload manageability degrade when categories expand.
  │
  │  IMPLICATIONS (authored):
  │  If the boundary is inherent, the tangled_rope classification is durable and reform must target category expansion directly. If constructed, the bounded-empathy mechanism is closer to a snare wearing coordination's clothing, and the coordination story is largely cover.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [adjudicator_identity_lock_reversibility] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: bounded_empathy_as_stabilizing_mechanism
  │  Question: If the adjudicator's professional identity were reconstituted around advocating for category expansion rather than holding boundaries, would the bounded-empathy mechanism collapse or merely relocate?
  │
  │  RESOLUTION METHOD (authored):
  │  Track outcomes in jurisdictions or institutions where adjudicators have been given explicit mandate and channel to recommend categorical expansion, observing whether the felt-sorrow-as-substitute pattern persists.
  │
  │  IMPLICATIONS (authored):
  │  If the pattern persists even when adjudicators are freed to advocate, the mechanism is deeper than identity-lock and points to institutional continuity as the true fixed point; if it dissolves, identity-lock was the load-bearing structure.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_bounded_empathy_as_stabilizing_mechanism] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: bounded_empathy_as_stabilizing_mechanism
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint bounded_empathy_as_stabilizing_mechanism computes as extractive (tangled_rope) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================
