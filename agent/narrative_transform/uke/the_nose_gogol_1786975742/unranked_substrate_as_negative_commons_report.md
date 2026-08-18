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
[SCENARIO MANAGER] Loading: testsets/unranked_substrate_as_negative_commons.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: unranked_substrate_as_negative_commons...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: unranked_substrate_as_negative_commons

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: unranked_substrate_as_negative_commons (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in unranked_substrate_as_negative_commons (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] unranked_substrate_as_negative_commons: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  unranked_substrate_as_negative_commons:
    [watch | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.1,0.12)
    [watch | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.9720000000000001,decline_signals,[excess_above_floor(0.06999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  unranked_substrate_as_negative_commons -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  unranked_substrate_as_negative_commons (ε=0.12):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.12 × 1.39 × 0.80 = 0.134
    moderate@national: d=0.650 f(d)=1.01 χ = 0.12 × 1.01 × 1.00 = 0.121
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.12 × -0.04 × 1.00 = -0.005
    analytical@global: d=0.720 f(d)=1.14 χ = 0.12 × 1.14 × 1.20 = 0.164

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: unranked_substrate_as_negative_commons ===
  Shift (computed via dr_type/3):
    powerless=scaffold  moderate=scaffold  institutional=scaffold  analytical=scaffold
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=unknown  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.972 (pristine)




╔══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                         ║
║  12/12 subsystems — 2 tension(s) (abductive, dirac)      ║
║  ! [informational] signature_correction (signature_grade)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)  ║
╚══════════════════════════════════════════════════════════╝




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        false_ci_rope  (canonical stance · twin-agreement 0.722)
    Purity:           0.972 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Live index:       none (scope=0, power=0)
    Drift events:     3 — extraction_accumulation [watch], purity_drift [watch], network_drift [watch]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9720
    Effective purity:   0.7020
    Propagation delta:  -0.2700

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | categorical_nonexistence_as_soft_denial | snare | explicit | authored | salient | n/a | 1.00 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  Purity degraded from 0.9720 to 0.7020 by contamination from 1 neighbor(s), primarily categorical_nonexistence_as_soft_denial (explicit, authored, purity 0.3542).

--- NETWORK POSITION (OQ-193) ---

  NOT ASSESSED — giant_component_analysis.raw.json not produced.

--- ORBIT CONTEXT ---

  Orbit Signature:    [scaffold]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   no gap (seats examined, comparable, agree)

  No gap-omega for this constraint (see gap operability above — an undetermined/no_gap constraint mints no gap-omega, by design).

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       scaffold (powerless, institutional‡)
    MaxEnt P(claimed): 0.7488 (moderate)
    Rival Type:       scaffold (P=0.2493)
    Margin:           +0.4996
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.5044]

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.7488 (moderate)
  Rival Type:    scaffold (P=0.2493)
  Margin:        +0.4996
  Entropy:       0.3208
  Distribution:  rope: 0.749, scaffold: 0.249, piton: 0.002

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.7488)
  Entropy:       0.3208
  Distribution:  rope: 0.749, scaffold: 0.249, piton: 0.002

  Classical/Indexed TV Distance: 0.0000 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): FLAGGED (ε=0.12, grid distance 0.02):
    - near_threshold: ε within r of a threshold AND the final type flips under ε±r — the landed-near-a-boundary artifact; inspect before anchoring

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.00417904, Var_fd=0.00431478 (103.2%), Var_scope=0.00022062 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.133723 | 1.392945 | 0.8000 |
| moderate | 0.121034 | 1.008614 | 1.0000 |
| institutional | -0.005070 | -0.042252 | 1.0000 |
| analytical | 0.164392 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    cohomology, signature, boltzmann, purity, fingerprint_voids, drift, gauge_orbit

  Expected Conflicts (1):
    fcr_gate: fcr_gate_deferral
      FCR gate deferred override due to metric perspectival variance

  Convergent Rejections (1):
    -> rope (suggested by: maxent, context_gap)
       2 subsystems suggest rope

  Tensions (2):
    abductive: abductive_tension([trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])
    dirac: dirac_mismatch(first_class,scaffold)

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  unranked_substrate_as_negative_commons: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for unranked_substrate_as_negative_commons: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.06999999999999999)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: fused_widower
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  unranked_substrate_as_negative_commons: R5 Q6 CROSSCHECK: q6_unclassified daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 2 resolution scenario(s):

  ┌─ [solidarity_without_information_exchange] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: unranked_substrate_as_negative_commons
  │  Question: Does co-location without information exchange constitute genuine coordination (rope), or is it merely the absence of a system rather than a positive coordination good at all?
  │
  │  RESOLUTION METHOD (authored):
  │  Longitudinal interviews with bedrock users on whether the co-presence changes their subjective experience of exclusion, compared to matched excluded individuals with no equivalent unranked space available.
  │
  │  IMPLICATIONS (authored):
  │  If co-presence measurably reduces psychological cost of exclusion without any information exchange, the rope classification is well-grounded as a minimal but real coordination function. If it produces no measurable effect, the 'coordination function' may be a retrospective narrative imposed on pure absence, and the constraint might better be modeled as a non-constraint (a null category) rather than a rope.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [formalization_destroys_the_good] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: unranked_substrate_as_negative_commons
  │  Question: If any agency attempted to formalize, resource, or administratively recognize the bedrock, would the coordination function survive?
  │
  │  RESOLUTION METHOD (authored):
  │  Compare outcomes in jurisdictions where similar informal unranked spaces have been formalized (turned into shelters, drop-in centers, or monitored zones) versus those left unadministered.
  │
  │  IMPLICATIONS (authored):
  │  If formalization destroys the function (because being weighed by even a benevolent system reintroduces the ranking logic the space exists outside of), then any well-intentioned policy intervention to 'help' bedrock users would eliminate the very good it targets — an important caution for downstream policy readings of this constraint.
  │
  │  Confidence without resolution: medium
  └─

====================================================
