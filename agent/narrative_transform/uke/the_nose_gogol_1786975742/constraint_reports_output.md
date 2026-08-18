
============================================================
CONSTRAINT REPORT: categorical_nonexistence_as_soft_denial
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
[SCENARIO MANAGER] Loading: testsets/categorical_nonexistence_as_soft_denial.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: categorical_nonexistence_as_soft_denial...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: categorical_nonexistence_as_soft_denial

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: categorical_nonexistence_as_soft_denial (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in categorical_nonexistence_as_soft_denial (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] categorical_nonexistence_as_soft_denial: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  4
  Critical: 2 | Warning: 2 | Watch: 0

  categorical_nonexistence_as_soft_denial:
    [warning | confidence: low] metric_substitution
        Evidence: evidence(theater_delta,0,24,0.3,0.58)
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.42,0.68)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.56)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  categorical_nonexistence_as_soft_denial -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  categorical_nonexistence_as_soft_denial (ε=0.68):
    powerless→organized@local: d=0.500 f(d)=0.65 χ = 0.68 × 0.65 × 0.80 = 0.354
    moderate@national: d=0.700 f(d)=1.11 χ = 0.68 × 1.11 × 1.00 = 0.752
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.68 × -0.04 × 1.00 = -0.029
    analytical@global: d=0.720 f(d)=1.14 χ = 0.68 × 1.14 × 1.20 = 0.932

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: categorical_nonexistence_as_soft_denial ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=distributed
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
      powerless:     no_route
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=8)
    Drift events:     4 — metric_substitution [warning], extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]
    Tangled psi:      0.9990 (snare_leaning)
    Coalition:        split_field



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

    Network neighbors (2):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | bounded_empathy_as_stabilizing_mechanism | tangled_rope | shared_victim | corpus-derived | low | distinct | 1.00 | 0.4191 |
    | unranked_substrate_as_negative_commons | scaffold | explicit | authored | salient | n/a | 1.00 | 0.7020 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  No significant contamination — purity unchanged across 2 neighbor(s).

--- NETWORK POSITION (OQ-193) ---

  NOT ASSESSED — giant_component_analysis.raw.json not produced.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare, tangled_rope]
  Orbit Span:         3
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_categorical_nonexistence_as_soft_denial
    Severity Score:    0.737
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F031

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless), snare (moderate), rope (institutional‡)
    MaxEnt P(claimed): 0.0003 (borderline)
    Rival Type:       snare (P=0.9997)
    Margin:           -0.9993
    Boundary:         tangled_rope->snare
    H^1 band:         5 — Both hubs contribute — 3 types across 4 observers: moderate, analytical → snare; powerless → tangled_rope; institutional → rope.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.9997 (pipeline says tangled_rope)
  MaxEnt P(claimed): 0.0003 (borderline)
  Rival Type:    snare (P=0.9997)
  Margin:        -0.9993
  Entropy:       0.0017
  Distribution:  snare: 1.000, tangled_rope: 0.000, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9997)
  Entropy:       0.0016
  Distribution:  snare: 1.000, tangled_rope: 0.000, piton: 0.000

  Classical/Indexed TV Distance: 0.0000 (near_zero)
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


====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for categorical_nonexistence_as_soft_denial

[STRUCTURAL SIGNATURE ANALYSIS]
  categorical_nonexistence_as_soft_denial: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for categorical_nonexistence_as_soft_denial: Enforcement present (suppression=0.79, resistance=0.44) with high extraction (0.68). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  categorical_nonexistence_as_soft_denial: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [schema_incompleteness_negligence_vs_design] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: categorical_nonexistence_as_soft_denial
  │  Question: Is the categorical schema's incompleteness an unavoidable byproduct of any finite classification system, or has the table had ample opportunity and demonstrated capacity to expand the schema and simply declined to do so?
  │
  │  RESOLUTION METHOD (authored):
  │  Audit of how frequently and how quickly the table has historically added new categories in response to documented unclassifiable claims, compared to the rate at which such claims arise.
  │
  │  IMPLICATIONS (authored):
  │  If the table has consistently failed to expand the schema despite clear, repeated demonstration of need, the constraint drifts toward snare (deliberate extraction via engineered incompleteness). If schema expansion genuinely lags behind the unpredictable diversity of real claims despite good-faith effort, the tangled_rope characterization holds more cleanly.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [appeal_pathway_existence_ambiguity] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: categorical_nonexistence_as_soft_denial
  │  Question: Does a genuine, if obscure, pathway exist for petitioners to compel schema expansion (e.g., via legislative petition or judicial mandamus), or is the 'petition for a new category' option itself illusory in practice?
  │
  │  RESOLUTION METHOD (authored):
  │  Case-outcome tracking: how many petitions for new categories have been filed, how many succeeded, and over what time horizon, compared to the volume of unclassifiable claims.
  │
  │  IMPLICATIONS (authored):
  │  A functioning, if slow, pathway would mitigate the suppression score and support keeping this as coordination-with-friction; a pathway that is nominally available but never successfully used would support treating the suppression as effectively total.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [the_table_agenda_setter_or_captured_bureaucracy] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: categorical_nonexistence_as_soft_denial
  │  Question: Is 'the_table_as_institution' a unified beneficiary intentionally preserving its appearance of completeness, or is it better modeled as a diffuse bureaucratic apparatus with no single actor who benefits, making this closer to a piton than a tangled_rope?
  │
  │  RESOLUTION METHOD (authored):
  │  Trace whether any identifiable office or role captures budget, prestige, or reduced liability specifically from the low visible denial rate the schema produces.
  │
  │  IMPLICATIONS (authored):
  │  If a concentrated beneficiary exists (e.g., leadership whose performance metrics are the closure rate), tangled_rope is correct. If the benefit is truly diffuse and no one profits, this would be better modeled as a piton with the table as agenda_setter absorbing only diffuse institutional inertia.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [omega_extraction_blindness_categorical_nonexistence_as_soft_denial] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: categorical_nonexistence_as_soft_denial
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint categorical_nonexistence_as_soft_denial computes as extractive (snare) at lower-power seats but functional (rope) at higher-power seats — extraction masked by perspective.
  └─

====================================================


============================================================
CONSTRAINT REPORT: bounded_empathy_as_stabilizing_mechanism
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


============================================================
CONSTRAINT REPORT: unranked_substrate_as_negative_commons
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

