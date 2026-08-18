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
