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

CORPUS CONTEXT: 279 constraints
  Types: 21 mountain, 50 rope, 141 tangled_rope, 24 snare, 4 piton, 13 scaffold
  Network severity: 52/59 drifting are severe (88%) [severe = effective purity < 0.70] | 141 omegas (134 critical)
  Purity coverage: 223/279 scorable, 56 unscored = 43 gate-fail (sentinel) + 13 no-data (no coordination_type)
  MaxEnt bands (corpus): 78 deep (31%) | 22 moderate (9%) | 153 borderline (60%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         2 borderline mountains
         1 contaminated mountains  -> cover-story candidates
         6 borderline ropes
        14 contaminated ropes  -> cover-story candidates
        29 borderline tangled_ropes
        15 borderline snares
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
        tangled_rope          0     0     29     99      0 |    128   9   4   141
        snare                 0     0     15      7      0 |     22   1   1    24
        piton                 0     2      2      0      0 |      4   0   0     4
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  26   0    26
        -----------------------------------------------------------------------
        totals               27    14     55    127      0 |    223  43  13   279
  SEVERITY x TYPE  (drifting subset, n_drifting=59; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              1       1     2      0 |      2        4
        piton                 1       0     1      0 |      1        2
        rope                  8       5     3      0 |     13       16
        scaffold              1       1     1      0 |      2        3
        snare                 1       3     0      0 |      4        4
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
[SCENARIO MANAGER] Loading: testsets/diagnostic_taxonomy_blind_spot.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: diagnostic_taxonomy_blind_spot...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: diagnostic_taxonomy_blind_spot

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: diagnostic_taxonomy_blind_spot (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in diagnostic_taxonomy_blind_spot (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] diagnostic_taxonomy_blind_spot: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  diagnostic_taxonomy_blind_spot:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.55,0.81)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.7300000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  diagnostic_taxonomy_blind_spot -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  diagnostic_taxonomy_blind_spot (ε=0.81):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.81 × 1.36 × 0.80 = 0.880
    moderate@national: d=0.700 f(d)=1.11 χ = 0.81 × 1.11 × 1.00 = 0.896
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.81 × -0.04 × 1.00 = -0.034
    analytical@global: d=0.720 f(d)=1.14 χ = 0.81 × 1.14 × 1.20 = 1.110

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: diagnostic_taxonomy_blind_spot ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,self_sustaining_extraction,unaccountable_extraction,unenforced_suppression]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.575 (borderline)




╔═══════════════════════════════════════════════════════════════╗
║  VERDICT: RED                                                 ║
║  12/12 subsystems — 3 tension(s) (abductive, signature, drift)║
║  ! [informational] perspectival_incoherence (perspectival)    ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)       ║
╚═══════════════════════════════════════════════════════════════╝
  ⓘ RED extraction = a statement about AUTHORED directionality d (victim/beneficiary/ε), not a seat-free moral verdict; the engine does not adjudicate the contested direction (trap vs cost-of-exit). Standing note — OQ-187.




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     snare
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     no_route
      moderate:      no_route
      institutional: author_engine_divergence
      analytical:    no_route
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.575 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Live index:       power (scope=0, power=6)
    Drift events:     4 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning], network_drift [critical]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5750
    Effective purity:   0.5032
    Propagation delta:  -0.0718

    Network neighbors (2):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | victim_self_attribution_foreclosure | snare | shared_beneficiary | corpus-derived | low | distinct | 0.30 | 0.3542 |
    | virtue_performance_as_exculpation | snare | shared_beneficiary | corpus-derived | low | distinct | 1.00 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  Purity degraded from 0.5750 to 0.5032, but the contamination is carried entirely by low-salience corpus-derived edges (2 neighbor(s), each a single shared agent / no authored link). No connection here is asserted by this case's source material.

--- NETWORK POSITION (OQ-193) ---

  Corpus giant component (pooled):  12 of 253 nodes, 137 components
  Corpus giant component (cross_kernel):  9 of 253 nodes, 201 components
  Sibling edges stripped: 234   same-kernel edges surviving: 0

  This constraint — pooled:   in_giant=False, component_size=3, degree=2
  This constraint — stratum:  in_giant=False, component_size=3, degree=2

  Interpretation: peripheral — not in the giant component in either stratum.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_diagnostic_taxonomy_blind_spot
    Severity Score:    0.781
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F011

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional‡)
    MaxEnt P(claimed): 1.0000 (deep)
    Rival Type:       tangled_rope (P=0.0000)
    Margin:           +1.0000
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 1.0000 (deep)
  Rival Type:    tangled_rope (P=0.0000)
  Margin:        +1.0000
  Entropy:       0.0001
  Distribution:  snare: 1.000, tangled_rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=1.0000)
  Entropy:       0.0001
  Distribution:  snare: 1.000, tangled_rope: 0.000

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.81, grid distance 0.35)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.19431727, Var_fd=0.19664240 (101.2%), Var_scope=0.01041997 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.880377 | 1.358606 | 0.8000 |
| moderate | 0.896259 | 1.106492 | 1.0000 |
| institutional | -0.034224 | -0.042252 | 1.0000 |
| analytical | 1.109644 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (6 subsystems):
    maxent, purity, dirac, fingerprint_voids, context_gap, fcr_gate

  Expected Conflicts (3):
    cohomology: cohomological_fracture_divergence
      Descent failure expected for constructed/perspectival type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    gauge_orbit: perspectival_orbit_variance
      Multi-type orbit IS the perspectival fracture

  Convergent Rejections: none

  Tensions (3):
    abductive: abductive_tension([trigger(maxent_shadow_divergence,0.85,shadow_override_tension,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(classical_oracle_failure,0.72,confident_oracle_with_obstruction,genuine)])
    signature: override_mismatch(constructed_high_extraction,snare)
    drift: critical_drift([drift(extraction_accumulation,evidence(extraction_delta,0,24,0.55,0.81),critical),drift(coupling_drift,evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing),critical),drift(purity_drift,evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.7300000000000001)]),warning),drift(network_drift,evidence(drifting_neighbors,[contagion(victim_self_attribution_foreclosure,0.03312499999999999,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.64)]),contagion(virtue_performance_as_exculpation,0.11041666666666664,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.64)])],effective_purity,0.5032291666666666,intrinsic_purity,0.575),critical)])

--- THEOREM INSTANTIATION ---

  T1 (Cover Story): At least one observer sees this constraint as benign (rope/tangled_rope) while another sees it as extractive (snare). The constraint functions as a cover story — its apparent type depends on observer position.

  T2 (Discrete Blocs): H^1 >= 3 means observer classifications cluster into discrete blocs that cannot be smoothly deformed into each other. The constraint lives in a topologically non-trivial region of the classification sheaf.

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T4 (Oracle Gap): A classical oracle (single-position MaxEnt) is confident, but cross-position comparison (H^1 > 0) reveals structure invisible from any single vantage point. Looking carefully from one position misses what comparing across positions reveals.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  T6 (Hub Correspondence — Hub 1): H^1 = 3 maps to Hub 1 (power-scaled extraction). A single observer's chi-value diverges from the other three, producing a 3+1 classification split.

  **6 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for diagnostic_taxonomy_blind_spot

[STRUCTURAL SIGNATURE ANALYSIS]
  diagnostic_taxonomy_blind_spot: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for diagnostic_taxonomy_blind_spot: Enforcement present (suppression=0.72, resistance=0.28) with high extraction (0.81). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  diagnostic_taxonomy_blind_spot: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [categorical_absence_vs_active_concealment] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: diagnostic_taxonomy_blind_spot
  │  Question: Is the missing caregiver-as-vector category a genuine, unintentional gap in each framework's conceptual vocabulary, or has Foma (or the surrounding social structure) actively cultivated conditions that keep the category from forming or being invoked?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical comparison: does the same gap appear in diagnostic frameworks with no domestic-abuse-suppressing social incentive at all (control case), or is the gap specifically correlated with cases where a caregiver benefits from non-detection?
  │
  │  IMPLICATIONS (authored):
  │  If the gap is a universal, incentive-independent feature of these diagnostic vocabularies, the classification leans toward a piton-adjacent structural failure that happens to be exploitable. If the gap is asymmetrically maintained or exploited by beneficiaries like Foma, the snare classification is strongly reinforced.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [escalation_threshold_location] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: diagnostic_taxonomy_blind_spot
  │  Question: How much confirming evidence would each framework need to accumulate before some observer, even without a formal caregiver-as-vector category, independently reasons their way to suspicion through informal inference?
  │
  │  RESOLUTION METHOD (authored):
  │  Track historical cases where informal suspicion eventually broke through despite categorical absence — what triggered the break, and how much unescalated evidence preceded it?
  │
  │  IMPLICATIONS (authored):
  │  A low threshold suggests the blind spot is fragile and likely to be interrupted eventually; a high or absent threshold suggests the blind spot is durable and self-reinforcing, strengthening the snare classification and lowering any expectation of self-correction.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_diagnostic_taxonomy_blind_spot] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: diagnostic_taxonomy_blind_spot
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint diagnostic_taxonomy_blind_spot computes as extractive (snare) at lower-power seats but functional (naturalized) at higher-power seats — extraction masked by perspective.
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: foma_silovich (beneficiary: foma_silovich, n=3)
  Members: diagnostic_taxonomy_blind_spot, victim_self_attribution_foreclosure, virtue_performance_as_exculpation

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  constructed_high_extraction
    Constraints:       diagnostic_taxonomy_blind_spot, victim_self_attribution_foreclosure, virtue_performance_as_exculpation

  [convergent_institutional]
    Institutional type: rope
    Analytical type:    snare
    Constraints:        diagnostic_taxonomy_blind_spot, victim_self_attribution_foreclosure, virtue_performance_as_exculpation
    ⚠ all members' institutional seats are role-authored knife-edge (OQ-188) — uniform institutional type is a config artifact channel

  [convergent_drift]
    Drift type:   extraction_accumulation
    Severity:     critical
    Constraints:  diagnostic_taxonomy_blind_spot, victim_self_attribution_foreclosure, virtue_performance_as_exculpation

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Shared structural signature and shared institutional observer type across this set are consistent with a shared configuration mechanism (OQ-188: all members' institutional seats are role-authored knife-edge) — not by itself evidence of coordination; do not treat the set as a coordinated system on this evidence alone.
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (extraction_accumulation) across 3 constraints indicates active systemic instability, not constraint-local drift.
