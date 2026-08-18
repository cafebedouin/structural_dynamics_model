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
[SCENARIO MANAGER] Loading: testsets/victim_self_attribution_foreclosure.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: victim_self_attribution_foreclosure...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: victim_self_attribution_foreclosure

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: victim_self_attribution_foreclosure (0-20)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 20 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 20 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 20 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 20 in victim_self_attribution_foreclosure (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] victim_self_attribution_foreclosure: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  victim_self_attribution_foreclosure:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,20,0.48,0.72)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.64)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  victim_self_attribution_foreclosure -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  victim_self_attribution_foreclosure (ε=0.72):
    powerless@local: d=0.880 f(d)=1.34 χ = 0.72 × 1.34 × 0.80 = 0.773
    moderate@national: d=0.700 f(d)=1.11 χ = 0.72 × 1.11 × 1.00 = 0.797
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.72 × -0.04 × 1.00 = -0.030
    analytical@global: d=0.720 f(d)=1.14 χ = 0.72 × 1.14 × 1.20 = 0.986

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: victim_self_attribution_foreclosure ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=concentrated  victims=concentrated
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

    Network neighbors (2):

    | Neighbor | Type | Edge | Provenance | Salience | Independence | Strength | Purity |
    |----------|------|------|------------|----------|--------------|----------|--------|
    | diagnostic_taxonomy_blind_spot | snare | shared_beneficiary | corpus-derived | low | distinct | 0.30 | 0.5032 |
    | virtue_performance_as_exculpation | snare | shared_beneficiary | corpus-derived | low | common-cause | 0.30 | 0.3542 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding. Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1 beneficiary AND ≥1 victim at near-identical ε (|Δε| ≤ 0.02) — consistent with co-authored slices of one underlying fact, so convergence across such edges is re-description, not independent corroboration (OQ-186); 'n/a' = out of domain (authored/inferred edge, or neighbor not comparable), never a verified 'distinct'.

  No significant contamination — purity unchanged across 2 neighbor(s).

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

  Omega: omega_extraction_blindness_victim_self_attribution_foreclosure
    Severity Score:    0.724
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F011

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.0167 (borderline)
    Rival Type:       snare (P=0.9832)
    Margin:           -0.9665
    Boundary:         tangled_rope->snare
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.9832 (pipeline says tangled_rope)
  MaxEnt P(claimed): 0.0167 (borderline)
  Rival Type:    snare (P=0.9832)
  Margin:        -0.9665
  Entropy:       0.0477
  Distribution:  snare: 0.983, tangled_rope: 0.017, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9837)
  Entropy:       0.0466
  Distribution:  snare: 0.984, tangled_rope: 0.016, piton: 0.000

  Classical/Indexed TV Distance: 0.0005 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — no kernel linkage (cs_kernel_id absent from testset)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

  ε-stability (data-side, r=0.02): stable — final type unchanged under ε±0.02 (ε=0.72, grid distance 0.26)

--- ABDUCTIVE FLAGS ---

  **3 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | maxent_shadow_divergence | 0.85 | shadow_override_tension | genuine | MaxEnt strongly favors a type different from signature override target — override may mask metric-preferred classification. |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |
  | classical_oracle_failure | 0.72 | confident_oracle_with_obstruction | genuine | MaxEnt is confident but H^1>0: looking carefully from one position misses what comparing across positions reveals (Theorem 4). |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.15285035, Var_fd=0.15341609 (100.4%), Var_scope=0.00815769 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.773137 | 1.342252 | 0.8000 |
| moderate | 0.796674 | 1.106492 | 1.0000 |
| institutional | -0.030421 | -0.042252 | 1.0000 |
| analytical | 0.986350 | 1.141609 | 1.2000 |


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
Timeline:       0 to 20
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for victim_self_attribution_foreclosure

[STRUCTURAL SIGNATURE ANALYSIS]
  victim_self_attribution_foreclosure: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for victim_self_attribution_foreclosure: Enforcement present (suppression=0.68, resistance=0.15) with high extraction (0.72). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  victim_self_attribution_foreclosure: R5 Q6 CROSSCHECK: contested_open daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [vocabulary_causal_priority] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: victim_self_attribution_foreclosure
  │  Question: Is the self-attributive vocabulary genuinely causally prior to and independent of the household mechanism (an inherited frame that happens to foreclose reporting), or was it partially shaped or reinforced by Foma Silovich's conduct within the marriage in ways that make it less independent than it appears?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparison of the vocabulary's content and intensity before and after the marriage began, via correspondence, diary entries, or testimony from family members who knew her premaritally.
  │
  │  IMPLICATIONS (authored):
  │  If genuinely prior and independent, the tangled-rope reading (real coordination function predating and outside this specific extraction) is well-supported. If substantially shaped by the marriage itself, the constraint drifts toward snare — the coordination story becomes cover constructed contemporaneously with the extraction rather than an inherited structure the extraction merely exploits.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [suppression_mechanism_ambiguity] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: victim_self_attribution_foreclosure
  │  Question: Is the measured suppression structural (she has literally no one to report to, no institutional channel that would hear her) or internalized (channels exist but she cannot use them because her own vocabulary forecloses the thought)?
  │
  │  RESOLUTION METHOD (authored):
  │  Post-hoc examination of whether any of the female relatives, if directly and explicitly asked whether the household could be implicated, would have been willing to hear and act on such a report — distinguishing an absent channel from an unusable one.
  │
  │  IMPLICATIONS (authored):
  │  If purely internalized, the effective suppression is higher than the structural measure suggests, because removing external barriers (were any identified) would not open the report — she would carry the foreclosure with her regardless of who was listening. If partly structural, external intervention (an outside party directly raising the possibility) could have material effect even without dismantling her vocabulary.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [physician_confessor_intent] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: victim_self_attribution_foreclosure
  │  Question: Do the physician and confessor knowingly reinforce a diagnostic frame they suspect is inaccurate because it is institutionally convenient, or do they hold the constitutional-weakness and moral-fault framings as genuine professional and pastoral truth?
  │
  │  RESOLUTION METHOD (authored):
  │  Records of the physician's private case notes or the confessor's private correspondence, if any survive, compared against their public/professional pronouncements to the family.
  │
  │  IMPLICATIONS (authored):
  │  If knowing reinforcement, they shift from incidental administrators of a shared cultural vocabulary toward active co-beneficiaries, strengthening the case for treating them as concentrated beneficiaries alongside Foma Silovich. If genuine belief, they remain agenda-setters without extraction, consistent with the current tangled-rope reading that names foma_silovich alone as the concentrated beneficiary.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_victim_self_attribution_foreclosure] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: victim_self_attribution_foreclosure
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint victim_self_attribution_foreclosure computes as extractive (snare) at lower-power seats but functional (naturalized) at higher-power seats — extraction masked by perspective.
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
