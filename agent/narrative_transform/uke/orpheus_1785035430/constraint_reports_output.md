
============================================================
CONSTRAINT REPORT: verification_prohibition_as_self_defeating_trial
============================================================
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

CORPUS CONTEXT: 217 constraints
  Types: 18 mountain, 40 rope, 107 tangled_rope, 16 snare, 3 piton, 13 scaffold
  Network severity: 36/42 drifting are severe (86%) [severe = effective purity < 0.70] | 109 omegas (106 critical)
  Purity coverage: 168/217 scorable, 49 unscored = 37 gate-fail (sentinel) + 12 no-data (no coordination_type)
  MaxEnt bands (corpus): 56 deep (28%) | 16 moderate (8%) | 125 borderline (63%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         1 contaminated mountains  -> cover-story candidates
         5 borderline ropes
        12 contaminated ropes  -> cover-story candidates
        22 borderline tangled_ropes
         8 borderline snares
         2 sound pitons
         2 pristine scaffolds
         2 sound scaffolds
         6 contaminated scaffolds
    Expected band by type  (REPORTING CONVENTION, not ground truth; logical_fingerprint:purity_zone vocabulary -- the SPEC bander, the one OQ-62 left named; 'contaminated' = [0.30,0.50)):
      mountain,rope -> pristine|sound ; tangled_rope,snare -> contaminated ; scaffold,piton -> mixed
    Full matrix  (per-row unscored split kept as gate-fail/no-data -- OQ-236 provenance):
        type           pristine sound border contam degrad | scored  gf  nd total
        mountain             13     1      0      1      0 |     15   3   0    18
        rope                  8     6      5     12      0 |     31   2   7    40
        tangled_rope          0     0     22     72      0 |     94   9   4   107
        snare                 0     0      8      6      0 |     14   1   1    16
        piton                 0     2      1      0      0 |      3   0   0     3
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  20   0    20
        -----------------------------------------------------------------------
        totals               23    11     37     97      0 |    168  37  12   217
  SEVERITY x TYPE  (drifting subset, n_drifting=42; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              0       1     2      0 |      1        3
        piton                 1       0     1      0 |      1        2
        rope                  6       4     2      0 |     10       12
        scaffold              1       1     1      0 |      2        3
        snare                 0       1     0      0 |      1        1
        tangled_rope         10      11     0      0 |     21       21
  CS patterns: 146 classified | 7 anchored_fixity_with_accretion, 34 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 33 interpretive_accretion, 9 marked_revision | 72 verdicts fired
  CS grounding mismatches: 114 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/verification_prohibition_as_self_defeating_trial.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: verification_prohibition_as_self_defeating_trial...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: verification_prohibition_as_self_defeating_trial

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: verification_prohibition_as_self_defeating_trial (0-40)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 40 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 40 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 40 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 40 in verification_prohibition_as_self_defeating_trial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] verification_prohibition_as_self_defeating_trial: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  verification_prohibition_as_self_defeating_trial:
    [warning | confidence: low] metric_substitution
        Evidence: evidence(theater_delta,0,40,0.32,0.58)
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,40,0.6,0.81)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.575,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.7300000000000001)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  verification_prohibition_as_self_defeating_trial -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  verification_prohibition_as_self_defeating_trial (ε=0.81):
    powerless@local: d=0.920 f(d)=1.37 χ = 0.81 × 1.37 × 0.80 = 0.890
    moderate@national: d=0.700 f(d)=1.11 χ = 0.81 × 1.11 × 1.00 = 0.896
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.81 × -0.04 × 1.00 = -0.034
    analytical@global: d=0.720 f(d)=1.14 χ = 0.81 × 1.14 × 1.20 = 1.110

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: verification_prohibition_as_self_defeating_trial ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=concentrated
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.575 (borderline)




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
      powerless:     no_route
      moderate:      no_route
      institutional: author_engine_divergence
      analytical:    no_route
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.575 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Live index:       power (scope=0, power=6)
    Drift events:     5 — metric_substitution [warning], extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning], network_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.5750
    Effective purity:   0.5198
    Propagation delta:  -0.0552

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | procedural_fairness_as_severity_laundering | snare | shared_victim | corpus-derived | low | distinct | 1.00 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  Purity degraded from 0.5750 to 0.5198, but the contamination is carried entirely by low-salience corpus-derived edges (1 neighbor(s), each a single shared agent / no authored link). No connection here is asserted by this case's source material.

--- NETWORK POSITION (OQ-193) ---

  Corpus giant component (pooled):  12 of 197 nodes, 110 components
  Corpus giant component (cross_kernel):  9 of 197 nodes, 156 components
  Sibling edges stripped: 158   same-kernel edges surviving: 0

  This constraint — pooled:   in_giant=False, component_size=2, degree=1
  This constraint — stratum:  in_giant=False, component_size=2, degree=1

  Interpretation: peripheral — not in the giant component in either stratum.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_verification_prohibition_as_self_defeating_trial
    Severity Score:    0.781
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F093

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.9994 (deep)
    Rival Type:       tangled_rope (P=0.0006)
    Margin:           +0.9989
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9994 (deep)
  Rival Type:    tangled_rope (P=0.0006)
  Margin:        +0.9989
  Entropy:       0.0026
  Distribution:  snare: 0.999, tangled_rope: 0.001, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9995)
  Entropy:       0.0026
  Distribution:  snare: 0.999, tangled_rope: 0.001, piton: 0.000

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
  Var_total=0.19513693, Var_fd=0.19893877 (101.9%), Var_scope=0.01050666 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.889966 | 1.373404 | 0.8000 |
| moderate | 0.896259 | 1.106492 | 1.0000 |
| institutional | -0.034224 | -0.042252 | 1.0000 |
| analytical | 1.109644 | 1.141609 | 1.2000 |


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


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 40
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for verification_prohibition_as_self_defeating_trial

[STRUCTURAL SIGNATURE ANALYSIS]
  verification_prohibition_as_self_defeating_trial: constructed_high_extraction (confidence: high)
    → CONSTRUCTED HIGH-EXTRACTION signature for verification_prohibition_as_self_defeating_trial: Enforcement present (suppression=0.72, resistance=0.55) with high extraction (0.81). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  verification_prohibition_as_self_defeating_trial: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [genuine_tradeoff_vs_manufactured_doubt] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: verification_prohibition_as_self_defeating_trial
  │  Question: Is there any documented instance of the verification-prohibition trial being paired with a genuine, low-cost alternative evidence channel that petitioners were permitted to use without voiding the trial?
  │
  │  RESOLUTION METHOD (authored):
  │  Systematic comparative-folklore survey across independent cultural traditions cataloguing whether any variant of the trial structure ever offers a sanctioned alternative confirmation method, and whether such variants show measurably lower forbidden-act failure rates.
  │
  │  IMPLICATIONS (authored):
  │  If genuine alternative-channel variants exist and show lower failure rates, the base structure without such channels is more clearly pure extraction (supporting snare); if no variant ever offers an alternative, the near-universality of the pure form suggests the doubt-manufacture is structurally intrinsic to the trial's cultural function, strengthening the snare classification further.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [petitioner_agency_vs_designed_trap] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: verification_prohibition_as_self_defeating_trial
  │  Question: Should the trial be read as testing petitioner virtue (a real, if harsh, coordination function) or as a designed trap where failure is structurally overdetermined regardless of petitioner conduct?
  │
  │  RESOLUTION METHOD (authored):
  │  Formal analysis of the failure-rate distribution: if failures cluster near points of maximal induced uncertainty (e.g., just before an expected but withheld confirmation) rather than uniformly across the trial's duration, this supports the designed-trap reading over the virtue-test reading.
  │
  │  IMPLICATIONS (authored):
  │  A designed-trap finding would push the classification more firmly toward snare with high confidence; a genuine virtue-test finding (uniform failure distribution driven by petitioner variation in resolve) would push toward tangled_rope, since a real coordination function would then coexist with the harsh cost.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [cross_cultural_naturalness_claim] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: verification_prohibition_as_self_defeating_trial
  │  Question: Does the structure's recurrence across unrelated cultures indicate it emerges naturally from universal features of trust and uncertainty (a mountain-like claim some custodians make), or does it indicate convergent institutional design serving similar custodian-class interests across societies?
  │
  │  RESOLUTION METHOD (authored):
  │  Compare societies with strong centralized ritual-custodian classes against societies with weak or absent such classes, controlling for trial-narrative prevalence and structure.
  │
  │  IMPLICATIONS (authored):
  │  If the pattern correlates strongly with custodian-class strength rather than appearing independent of institutional structure, this undermines any natural-law framing and supports the snare classification; if it appears independent of custodian strength, a rope or mountain reading becomes more defensible.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_verification_prohibition_as_self_defeating_trial] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: verification_prohibition_as_self_defeating_trial
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint verification_prohibition_as_self_defeating_trial computes as extractive (snare) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================


============================================================
CONSTRAINT REPORT: procedural_fairness_as_severity_laundering
============================================================
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

CORPUS CONTEXT: 217 constraints
  Types: 18 mountain, 40 rope, 107 tangled_rope, 16 snare, 3 piton, 13 scaffold
  Network severity: 36/42 drifting are severe (86%) [severe = effective purity < 0.70] | 109 omegas (106 critical)
  Purity coverage: 168/217 scorable, 49 unscored = 37 gate-fail (sentinel) + 12 no-data (no coordination_type)
  MaxEnt bands (corpus): 56 deep (28%) | 16 moderate (8%) | 125 borderline (63%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         1 contaminated mountains  -> cover-story candidates
         5 borderline ropes
        12 contaminated ropes  -> cover-story candidates
        22 borderline tangled_ropes
         8 borderline snares
         2 sound pitons
         2 pristine scaffolds
         2 sound scaffolds
         6 contaminated scaffolds
    Expected band by type  (REPORTING CONVENTION, not ground truth; logical_fingerprint:purity_zone vocabulary -- the SPEC bander, the one OQ-62 left named; 'contaminated' = [0.30,0.50)):
      mountain,rope -> pristine|sound ; tangled_rope,snare -> contaminated ; scaffold,piton -> mixed
    Full matrix  (per-row unscored split kept as gate-fail/no-data -- OQ-236 provenance):
        type           pristine sound border contam degrad | scored  gf  nd total
        mountain             13     1      0      1      0 |     15   3   0    18
        rope                  8     6      5     12      0 |     31   2   7    40
        tangled_rope          0     0     22     72      0 |     94   9   4   107
        snare                 0     0      8      6      0 |     14   1   1    16
        piton                 0     2      1      0      0 |      3   0   0     3
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  20   0    20
        -----------------------------------------------------------------------
        totals               23    11     37     97      0 |    168  37  12   217
  SEVERITY x TYPE  (drifting subset, n_drifting=42; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              0       1     2      0 |      1        3
        piton                 1       0     1      0 |      1        2
        rope                  6       4     2      0 |     10       12
        scaffold              1       1     1      0 |      2        3
        snare                 0       1     0      0 |      1        1
        tangled_rope         10      11     0      0 |     21       21
  CS patterns: 146 classified | 7 anchored_fixity_with_accretion, 34 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 33 interpretive_accretion, 9 marked_revision | 72 verdicts fired
  CS grounding mismatches: 114 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/procedural_fairness_as_severity_laundering.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: procedural_fairness_as_severity_laundering...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: procedural_fairness_as_severity_laundering

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: procedural_fairness_as_severity_laundering (0-100)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 100 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 100 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 100 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 100 in procedural_fairness_as_severity_laundering (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] procedural_fairness_as_severity_laundering: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  procedural_fairness_as_severity_laundering:
    [warning | confidence: low] metric_substitution
        Evidence: evidence(theater_delta,0,100,0.22,0.58)
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,100,0.38,0.71)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.61)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  procedural_fairness_as_severity_laundering -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  procedural_fairness_as_severity_laundering (ε=0.71):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.71 × 1.36 × 0.80 = 0.772
    moderate@national: d=0.700 f(d)=1.11 χ = 0.71 × 1.11 × 1.00 = 0.786
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.71 × -0.04 × 1.00 = -0.030
    analytical@global: d=0.720 f(d)=1.14 χ = 0.71 × 1.14 × 1.20 = 0.973

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: procedural_fairness_as_severity_laundering ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=concentrated
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=1.000, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(1.0,0.33))
  Purity:     0.354 (contaminated)




╔═══════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                          ║
║  12/12 subsystems — 2 tension(s) (abductive, signature)   ║
║  ! [informational] perspectival_incoherence (perspectival)║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)   ║
╚═══════════════════════════════════════════════════════════╝




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
    Drift events:     4 — metric_substitution [warning], extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]
    Tangled psi:      0.9988 (snare_leaning)
    Coalition:        institutional_dissent



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | verification_prohibition_as_self_defeating_trial | snare | shared_victim | corpus-derived | low | distinct | 1.00 | 0.5198 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  No significant contamination — purity unchanged across 1 neighbor(s).

--- NETWORK POSITION (OQ-193) ---

  Corpus giant component (pooled):  12 of 197 nodes, 110 components
  Corpus giant component (cross_kernel):  9 of 197 nodes, 156 components
  Sibling edges stripped: 158   same-kernel edges surviving: 0

  This constraint — pooled:   in_giant=False, component_size=2, degree=1
  This constraint — stratum:  in_giant=False, component_size=2, degree=1

  Interpretation: peripheral — not in the giant component in either stratum.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_procedural_fairness_as_severity_laundering
    Severity Score:    0.701
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F072

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.1368 (borderline)
    Rival Type:       snare (P=0.8632)
    Margin:           -0.7264
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.8632 (pipeline says tangled_rope)
  MaxEnt P(claimed): 0.1368 (borderline)
  Rival Type:    snare (P=0.8632)
  Margin:        -0.7264
  Entropy:       0.2231
  Distribution:  snare: 0.863, tangled_rope: 0.137, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.8744)
  Entropy:       0.2113
  Distribution:  snare: 0.874, tangled_rope: 0.126, piton: 0.000

  Classical/Indexed TV Distance: 0.0112 (moderate)
  Moderate divergence — observer-dependence shifts probabilistic weights without changing the top classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.71, grid distance 0.25)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.14929938, Var_fd=0.15108586 (101.2%), Var_scope=0.00800595 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.771688 | 1.358606 | 0.8000 |
| moderate | 0.785609 | 1.106492 | 1.0000 |
| institutional | -0.029999 | -0.042252 | 1.0000 |
| analytical | 0.972651 | 1.141609 | 1.2000 |


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


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 100
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for procedural_fairness_as_severity_laundering

[STRUCTURAL SIGNATURE ANALYSIS]
  procedural_fairness_as_severity_laundering: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for procedural_fairness_as_severity_laundering: Enforcement present (suppression=0.62, resistance=0.44) with high extraction (0.71). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  procedural_fairness_as_severity_laundering: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [consistency_vs_proportionality_conflation] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: procedural_fairness_as_severity_laundering
  │  Question: Is procedural consistency (same terms for everyone) genuinely sufficient for 'fairness,' or does the office's definition simply exclude proportionality by construction, laundering severity as a side effect of a definition chosen for other reasons?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative analysis against folk-legal systems that have separated the two questions (consistency review and periodic proportionality review as distinct processes) to see whether proportionality review can be added without destroying the bias-resistance the consistency rule protects.
  │
  │  IMPLICATIONS (authored):
  │  If proportionality review is separable from consistency without reintroducing arbitrary bias, the current fusion is revealed as a choice that specifically protects the office's unreviewable authority, strengthening the tangled_rope reading toward snare. If the functions are genuinely inseparable, part of the measured extraction is the unavoidable cost of bias-resistance.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [generational_reconsideration_zero_frequency] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: procedural_fairness_as_severity_laundering
  │  Question: Is the zero-frequency of term reconsideration across generations a deliberate suppression mechanism, or an emergent property of a system with no institutional mechanism for revision at all (absence of a revision clause rather than active blocking of one)?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical/ethnographic examination of whether revision was ever proposed and rejected (active suppression) versus never proposed at all (structural absence of a revision forum).
  │
  │  IMPLICATIONS (authored):
  │  Active rejection of proposed revisions would strengthen the snare/tangled_rope reading and raise measured suppression; a pure absence of a revision mechanism (never proposed, no gatekeeping event) would suggest the constraint is closer to inertial atrophy (piton-adjacent) than active extraction, and the office's benefit would appear more incidental than deliberate.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [downstream_coupling_with_verification_prohibition] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: procedural_fairness_as_severity_laundering
  │  Question: How much of this constraint's measured extraction is inherited from the upstream verification_prohibition_as_self_defeating_trial constraint (the trial's own rules foreclosing exculpatory evidence) versus generated independently by the fairness-as-consistency definition alone?
  │
  │  RESOLUTION METHOD (authored):
  │  Counterfactual analysis: would a version of this ritual system with normal (non-self-defeating) verification rules, but the same fairness-as-consistency definition, still show comparable extraction growth?
  │
  │  IMPLICATIONS (authored):
  │  If extraction growth persists even without the upstream verification-prohibition feeding it, the severity-laundering mechanism is independently extractive and should be weighted more heavily on its own account rather than treated as a downstream artifact of the sibling snare.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [omega_extraction_blindness_procedural_fairness_as_severity_laundering] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: procedural_fairness_as_severity_laundering
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint procedural_fairness_as_severity_laundering computes as extractive (snare) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================


============================================================
CONSTRAINT REPORT: ritual_transmission_as_double_edged_inheritance
============================================================
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

CORPUS CONTEXT: 217 constraints
  Types: 18 mountain, 40 rope, 107 tangled_rope, 16 snare, 3 piton, 13 scaffold
  Network severity: 36/42 drifting are severe (86%) [severe = effective purity < 0.70] | 109 omegas (106 critical)
  Purity coverage: 168/217 scorable, 49 unscored = 37 gate-fail (sentinel) + 12 no-data (no coordination_type)
  MaxEnt bands (corpus): 56 deep (28%) | 16 moderate (8%) | 125 borderline (63%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         1 contaminated mountains  -> cover-story candidates
         5 borderline ropes
        12 contaminated ropes  -> cover-story candidates
        22 borderline tangled_ropes
         8 borderline snares
         2 sound pitons
         2 pristine scaffolds
         2 sound scaffolds
         6 contaminated scaffolds
    Expected band by type  (REPORTING CONVENTION, not ground truth; logical_fingerprint:purity_zone vocabulary -- the SPEC bander, the one OQ-62 left named; 'contaminated' = [0.30,0.50)):
      mountain,rope -> pristine|sound ; tangled_rope,snare -> contaminated ; scaffold,piton -> mixed
    Full matrix  (per-row unscored split kept as gate-fail/no-data -- OQ-236 provenance):
        type           pristine sound border contam degrad | scored  gf  nd total
        mountain             13     1      0      1      0 |     15   3   0    18
        rope                  8     6      5     12      0 |     31   2   7    40
        tangled_rope          0     0     22     72      0 |     94   9   4   107
        snare                 0     0      8      6      0 |     14   1   1    16
        piton                 0     2      1      0      0 |      3   0   0     3
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  20   0    20
        -----------------------------------------------------------------------
        totals               23    11     37     97      0 |    168  37  12   217
  SEVERITY x TYPE  (drifting subset, n_drifting=42; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              0       1     2      0 |      1        3
        piton                 1       0     1      0 |      1        2
        rope                  6       4     2      0 |     10       12
        scaffold              1       1     1      0 |      2        3
        snare                 0       1     0      0 |      1        1
        tangled_rope         10      11     0      0 |     21       21
  CS patterns: 146 classified | 7 anchored_fixity_with_accretion, 34 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 33 interpretive_accretion, 9 marked_revision | 72 verdicts fired
  CS grounding mismatches: 114 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/ritual_transmission_as_double_edged_inheritance.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: ritual_transmission_as_double_edged_inheritance...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: ritual_transmission_as_double_edged_inheritance

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: ritual_transmission_as_double_edged_inheritance (0-20)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 20 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 20 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 20 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 20 in ritual_transmission_as_double_edged_inheritance (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] ritual_transmission_as_double_edged_inheritance: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 0 | Warning: 3 | Watch: 0

  ritual_transmission_as_double_edged_inheritance:
    [warning | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,20,0.05,0.42)
    [warning | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.75,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.639,decline_signals,[extraction_rising,coupling_above_threshold(0.75),theater_rising,excess_above_floor(0.33999999999999997)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  ritual_transmission_as_double_edged_inheritance -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  ritual_transmission_as_double_edged_inheritance (ε=0.42):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.42 × 1.36 × 0.80 = 0.456
    moderate@national: d=0.700 f(d)=1.11 χ = 0.42 × 1.11 × 1.00 = 0.465
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.42 × -0.04 × 1.00 = -0.018
    analytical@global: d=0.720 f(d)=1.14 χ = 0.42 × 1.14 × 1.20 = 0.575

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: ritual_transmission_as_double_edged_inheritance ===
  Shift (computed via dr_type/3):
    powerless=unknown  moderate=unknown  institutional=rope  analytical=unknown
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      [no_exit_for_victims,self_sustaining_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=unknown  theater=rising
  Zone:       extraction=low  suppression=moderate
  Coupling:   strongly_coupled (score=0.750, pairs=[], boltzmann=non_compliant(0.75,0.29))
  Purity:     0.639 (borderline)




  [Verdict unavailable — run full pipeline to include]




═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     engine_abstained
      moderate:      engine_abstained
      institutional: no_route
      analytical:    engine_abstained
    Signature:        false_ci_rope  (canonical stance · twin-agreement 0.722)
    Purity:           0.639 (borderline)
    Coupling:         strongly_coupled (score: 0.75)
    Boltzmann:        non_compliant
    Live index:       power (scope=0, power=6)
    Drift events:     3 — extraction_accumulation [warning], coupling_drift [warning], purity_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.6390
    Effective purity:   0.6390
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- NETWORK POSITION (OQ-193) ---

  Corpus giant component (pooled):  12 of 197 nodes, 110 components
  Corpus giant component (cross_kernel):  9 of 197 nodes, 156 components
  Sibling edges stripped: 158   same-kernel edges surviving: 0

  This constraint — pooled:   in_giant=False, component_size=1, degree=0
  This constraint — stratum:  in_giant=False, component_size=1, degree=0

  Interpretation: peripheral — not in the giant component in either stratum.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, unknown]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   no gap (seats examined, comparable, agree)

  No gap-omega for this constraint (see gap operability above — an undetermined/no_gap constraint mints no gap-omega, by design).

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       unknown (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.0285 (borderline)
    Rival Type:       piton (P=0.9614)
    Margin:           -0.9329
    Boundary:         rope->piton
    Sheaf status:     undetermined — N/A (OQ-51) — the gluing regime cannot be computed: fewer than two real (non-unknown) seats — no pair to glue

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: piton at P=0.9614 (pipeline says rope)
  MaxEnt P(claimed): 0.0285 (borderline)
  Rival Type:    piton (P=0.9614)
  Margin:        -0.9329
  Entropy:       0.1050
  Distribution:  piton: 0.961, rope: 0.028, tangled_rope: 0.010

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      piton (P=0.9604)
  Entropy:       0.1076
  Distribution:  piton: 0.960, rope: 0.028, tangled_rope: 0.011

  Classical/Indexed TV Distance: 0.0010 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.42, grid distance 0.03)

--- ABDUCTIVE FLAGS ---

  **1 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.05224449, Var_fd=0.05286956 (101.2%), Var_scope=0.00280153 (5.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.456492 | 1.358606 | 0.8000 |
| moderate | 0.464727 | 1.106492 | 1.0000 |
| institutional | -0.017746 | -0.042252 | 1.0000 |
| analytical | 0.575371 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  [diagnostic_verdict not computed for this constraint]

--- THEOREM INSTANTIATION ---

  T3 (Spectral Dominance): The institutional observer's classification diverges from the majority of other observers. The power-scaled extraction metric (chi) produces a qualitatively different result at the institutional index — the spectrum is dominated by a single observer position.

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **2 of 6 theorems active.**


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 20
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for ritual_transmission_as_double_edged_inheritance

[STRUCTURAL SIGNATURE ANALYSIS]
  ritual_transmission_as_double_edged_inheritance: false_ci_rope (confidence: medium)
    → FALSE CI_ROPE signature for ritual_transmission_as_double_edged_inheritance: Appears to be rope (explicit_rope_claim) but fails 2 Boltzmann structural test(s): [boltzmann_non_compliant(0.75,0.29),excess_above_floor(0.33999999999999997)]. Coupling score=0.75. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: grieving_child_recipient
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  ritual_transmission_as_double_edged_inheritance: R5 Q6 CROSSCHECK: q6_signature_unknown daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [exactness_as_natural_or_constructed_requirement] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: ritual_transmission_as_double_edged_inheritance
  │  Question: Is the ritual's requirement of exact name and exact hour a natural feature of how grief-comfort rituals must work (specificity aids psychological steadying), or is it a constructed feature deliberately compatible with the Warden's jurisdictional access requirements?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative study of other grief-comfort rituals in the same folk tradition: if exactness-of-form is common across rituals with no jurisdictional function, the requirement is likely intrinsic to the comfort mechanism, not designed for later coercive compatibility.
  │
  │  IMPLICATIONS (authored):
  │  If the exactness is intrinsic to comfort, the dual-use is coincidental and the teacher bears no design responsibility for the later coercive function. If the exactness was always shaped by or borrowed from jurisdictional forms, the teacher's ritual was never purely comfort — it carried the coercive key from its own origin.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [teacher_downstream_liability_ambiguity] AUTHORED RESOLUTION PROTOCOL (preference)
  │  Constraint: ritual_transmission_as_double_edged_inheritance
  │  Question: Does the teacher's complete inability to modify or recall the ritual after transmission mean she bears no responsibility for its later coercive use, or does knowingly teaching an irreversible, exact-form ritual to a child carry an anticipatory responsibility for foreseeable misuse?
  │
  │  RESOLUTION METHOD (authored):
  │  Examine whether the folk tradition itself has any doctrine of transmitter responsibility for downstream ritual use, or whether irreversibility is understood community-wide as fully discharging the teacher's responsibility at the moment of teaching.
  │
  │  IMPLICATIONS (authored):
  │  If the tradition holds transmitters blameless once transmission completes, the teacher is correctly classified as a victim of the later use. If the tradition holds transmitters partially responsible for foreseeable misuse, the teacher's seat may carry some d toward the target end even without active current involvement.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [warden_jurisdiction_natural_or_constructed] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: ritual_transmission_as_double_edged_inheritance
  │  Question: Is the Warden's jurisdiction itself a genuine, independently-arising coordination structure that happens to overlap with the ritual's form, or is the jurisdiction constructed/maintained partly through the accumulation of exactly these repurposed folk-rituals as access keys?
  │
  │  RESOLUTION METHOD (authored):
  │  Trace whether the Warden's jurisdiction predates the ritual's dual-use discovery, and whether the jurisdiction has other independent access mechanisms not derived from folk grief-rituals.
  │
  │  IMPLICATIONS (authored):
  │  If the jurisdiction is independently founded and merely absorbs whatever keys arrive, the Warden's operators are incidental beneficiaries with lower culpability. If the jurisdiction's authority is substantially constituted by accumulated repurposed rituals, the Warden's operators are active co-architects of the extraction and the tangled-rope reading strengthens at their seat specifically.
  │
  │  Confidence without resolution: low
  └─

====================================================

