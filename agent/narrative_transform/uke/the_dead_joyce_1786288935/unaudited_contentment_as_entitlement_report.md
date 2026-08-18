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

CORPUS CONTEXT: 235 constraints
  Types: 18 mountain, 45 rope, 116 tangled_rope, 18 snare, 3 piton, 13 scaffold
  Network severity: 39/45 drifting are severe (87%) [severe = effective purity < 0.70] | 117 omegas (114 critical)
  Purity coverage: 183/235 scorable, 52 unscored = 39 gate-fail (sentinel) + 13 no-data (no coordination_type)
  MaxEnt bands (corpus): 65 deep (31%) | 14 moderate (7%) | 134 borderline (63%)
  PURITY x TYPE  (INTRINSIC purity; descriptive; single leg -- OQ-236, not cross-leg comparable)
    Off-diagonal residual -- where purity says what type does not:
         1 contaminated mountains  -> cover-story candidates
         5 borderline ropes
        13 contaminated ropes  -> cover-story candidates
        24 borderline tangled_ropes
         9 borderline snares
         2 sound pitons
         2 pristine scaffolds
         2 sound scaffolds
         6 contaminated scaffolds
    Expected band by type  (REPORTING CONVENTION, not ground truth; logical_fingerprint:purity_zone vocabulary -- the SPEC bander, the one OQ-62 left named; 'contaminated' = [0.30,0.50)):
      mountain,rope -> pristine|sound ; tangled_rope,snare -> contaminated ; scaffold,piton -> mixed
    Full matrix  (per-row unscored split kept as gate-fail/no-data -- OQ-236 provenance):
        type           pristine sound border contam degrad | scored  gf  nd total
        mountain             13     1      0      1      0 |     15   3   0    18
        rope                  9     8      5     13      0 |     35   2   8    45
        tangled_rope          0     0     24     79      0 |    103   9   4   116
        snare                 0     0      9      7      0 |     16   1   1    18
        piton                 0     2      1      0      0 |      3   0   0     3
        scaffold              2     2      1      6      0 |     11   2   0    13
        unknown               0     0      0      0      0 |      0  22   0    22
        -----------------------------------------------------------------------
        totals               24    13     40    106      0 |    183  39  13   235
  SEVERITY x TYPE  (drifting subset, n_drifting=45; engine severity cut, Q1 backstop):
        type           critical warning watch  undet | severe drifting
        mountain              0       1     2      0 |      1        3
        piton                 1       0     1      0 |      1        2
        rope                  7       4     2      0 |     11       13
        scaffold              1       1     1      0 |      2        3
        snare                 0       1     0      0 |      1        1
        tangled_rope         11      12     0      0 |     23       23
  CS patterns: 158 classified | 8 anchored_fixity_with_accretion, 36 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 34 interpretive_accretion, 10 marked_revision | 75 verdicts fired
  CS grounding mismatches: 125 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/unaudited_contentment_as_entitlement.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: unaudited_contentment_as_entitlement...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is opt-in by story focus (authored-or-absent; injection/imputation retired) — OQ-93 RESOLVED 2026-06-11, see ISSUES.md
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: unaudited_contentment_as_entitlement

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: unaudited_contentment_as_entitlement (0-24)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 24 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 24 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 24 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 24 in unaudited_contentment_as_entitlement (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] unaudited_contentment_as_entitlement: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  unaudited_contentment_as_entitlement:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,24,0.42,0.71)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,1.0,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.3541666666666667,decline_signals,[extraction_rising,coupling_above_threshold(1.0),theater_rising,excess_above_floor(0.63)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  unaudited_contentment_as_entitlement -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  unaudited_contentment_as_entitlement (ε=0.71):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.71 × 1.36 × 0.80 = 0.772
    moderate@national: d=0.700 f(d)=1.11 χ = 0.71 × 1.11 × 1.00 = 0.786
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.71 × -0.04 × 1.00 = -0.030
    analytical@global: d=0.720 f(d)=1.14 χ = 0.71 × 1.14 × 1.20 = 0.973

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: unaudited_contentment_as_entitlement ===
  Shift (computed via dr_type/3):
    powerless=snare  moderate=snare  institutional=rope  analytical=snare
  Properties: [asymmetric,coordination,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,self_sustaining_extraction,unaccountable_extraction,unenforced_suppression]
  Actors:     beneficiaries=concentrated  victims=concentrated
  Drift:      extraction=rising  suppression=unknown  theater=rising
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
      powerless:     no_route
      moderate:      no_route
      institutional: author_engine_divergence
      analytical:    no_route
    Signature:        constructed_high_extraction  (canonical stance · twin-agreement 0.722)
    Purity:           0.354167 (contaminated)
    Coupling:         strongly_coupled (score: 1.0)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=7)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning]



--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.3542
    Effective purity:   0.3542
    Propagation delta:  +0.0000

  No contamination network — purity is intrinsic.

--- NETWORK POSITION (OQ-193) ---

  Corpus giant component (pooled):  12 of 213 nodes, 118 components
  Corpus giant component (cross_kernel):  9 of 213 nodes, 171 components
  Sibling edges stripped: 194   same-kernel edges surviving: 0

  This constraint — pooled:   in_giant=False, component_size=1, degree=0
  This constraint — stratum:  in_giant=False, component_size=1, degree=0

  Interpretation: peripheral — not in the giant component in either stratum.

--- ORBIT CONTEXT ---

  Orbit Signature:    [rope, snare]
  Orbit Span:         2
  Gauge Status:       Gauge-Variant

--- ENRICHED OMEGA CONTEXT ---

  Gap operability:   gap detected

  Omega: omega_extraction_blindness_unaudited_contentment_as_entitlement
    Severity Score:    0.701
    Gap Class:         coordination_washing
    Gap Pattern:       snare_masked_as_rope
    Family ID:         F094

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       snare (powerless), rope (institutional‡)
    MaxEnt P(claimed): 0.9941 (deep)
    Rival Type:       tangled_rope (P=0.0055)
    Margin:           +0.9885
    Boundary:         snare->tangled_rope
    H^1 band:         3 — Hub 1 (power-scaled extraction) drives a 3+1 split: institutional sees rope while powerless, moderate, analytical see snare.
    Sheaf status:     manifest_presheaf — no global section — observers disagree (H^1>0)

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9941 (deep)
  Rival Type:    tangled_rope (P=0.0055)
  Margin:        +0.9885
  Entropy:       0.0211
  Distribution:  snare: 0.994, tangled_rope: 0.006, piton: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9945)
  Entropy:       0.0200
  Distribution:  snare: 0.994, tangled_rope: 0.005, piton: 0.000

  Classical/Indexed TV Distance: 0.0004 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

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
Timeline:       0 to 24
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Leveled coercion grid: not authored (story not level-resolved-coercion focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [informational]: perspectival_incoherence detected for unaudited_contentment_as_entitlement

[STRUCTURAL SIGNATURE ANALYSIS]
  unaudited_contentment_as_entitlement: constructed_high_extraction (confidence: medium)
    → CONSTRUCTED HIGH-EXTRACTION signature for unaudited_contentment_as_entitlement: Enforcement present (suppression=0.62, resistance=0.34) with high extraction (0.71). This is an extraction mechanism that metrics failed to classify as snare.

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  unaudited_contentment_as_entitlement: R5 Q6 CROSSCHECK: dead_claim_vs_snare_present daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [suppression_mechanism_ambiguity] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: unaudited_contentment_as_entitlement
  │  Question: Is Klára's silence-as-suppression structural (fear of Anton's defensiveness, economic/social dependency on the marriage) or internalized (she has come to believe her own contentment ought to be inferable this way, or that voicing cost would be illegitimate)?
  │
  │  RESOLUTION METHOD (authored):
  │  Post-accident trajectory: if Klára's suppression persists even after Anton's partial recognition removes the immediate risk of defensiveness, that indicates internalization; if it lifts once the external risk is addressed, it was primarily structural.
  │
  │  IMPLICATIONS (authored):
  │  If substantially internalized, the effective suppression is higher than the structural indicators alone suggest, and the accident of visibility will produce only temporary, incomplete correction rather than durable renegotiation — consistent with the story's premise of partial recognition without full correction.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [inference_vs_inquiry_threshold] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: unaudited_contentment_as_entitlement
  │  Question: Is there a version of Anton's behavior — some threshold of attentiveness short of a full explicit audit — that would count as genuine inquiry rather than mere inference from absence of complaint, and did he ever cross it before the accident?
  │
  │  RESOLUTION METHOD (authored):
  │  A detailed behavioral history of whether Anton ever directly asked Klára an open-ended question about her experience of the arrangement, versus only reading her behavior and drawing conclusions.
  │
  │  IMPLICATIONS (authored):
  │  If he never once directly inquired, the snare classification is strongly supported — the exemption from investigation is total and structural. If he inquired occasionally but she deflected, the picture shifts toward a more genuinely coordinated but still asymmetric arrangement, closer to tangled_rope.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [accident_correction_completeness] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: unaudited_contentment_as_entitlement
  │  Question: Does the accident of visibility produce a durable renegotiation of the ledger, or only a temporary, incident-specific concession that leaves the underlying inferential mechanism (silence = proof of adequacy) intact for future use?
  │
  │  RESOLUTION METHOD (authored):
  │  Observe whether Anton institutes any standing practice of inquiry after the accident, versus reverting to inference-from-silence once the immediate visible crisis passes.
  │
  │  IMPLICATIONS (authored):
  │  A reversion would confirm the constraint's persistence mechanism is robust to isolated disruptions — the snare survives partial exposure. A durable change would indicate the accident functioned as a genuine correction, shifting the classification toward scaffold (a transitional crisis resolving into renegotiated terms).
  │
  │  Confidence without resolution: low
  └─

  ┌─ [omega_extraction_blindness_unaudited_contentment_as_entitlement] NO SCENARIO TEMPLATE [OPEN]
  │  Constraint: unaudited_contentment_as_entitlement
  │  Unmatched type/gap combination: (conceptual, extraction_blindness) — no authored protocol,
  │  no typed template. Graduation: author a 5-arity omega_variable
  │  protocol in the testset, or add a typed template clause.
  │  Gap: Constraint unaudited_contentment_as_entitlement computes as extractive (snare) at lower-power seats but functional (naturalized) at higher-power seats — extraction masked by perspective.
  └─

====================================================
