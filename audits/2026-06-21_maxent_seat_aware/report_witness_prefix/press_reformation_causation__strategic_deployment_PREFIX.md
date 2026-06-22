CORPUS CONTEXT: 92 constraints
  Types: 15 mountain, 19 rope, 38 tangled_rope, 9 snare, 5 scaffold
  Network stability: cascading | 40 omegas (40 critical)
  MaxEnt bands (corpus): 16 deep (19%) | 3 moderate (3%) | 67 borderline (78%)
  CS patterns: 75 classified | 3 anchored_fixity_with_accretion, 10 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 26 interpretive_accretion, 4 marked_revision | 31 verdicts fired
  CS grounding mismatches: 56 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/press_reformation_causation__strategic_deployment.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: press_reformation_causation__strategic_deployment...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 0/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 32 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93

>>> INITIATING DR-AUDIT SUITE: press_reformation_causation__strategic_deployment

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: press_reformation_causation__strategic_deployment (0-50)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] press_reformation_causation__strategic_deployment: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: increasing_coercion (Confidence: high) [grid diet: authored 32/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  3
  Critical: 2 | Warning: 1 | Watch: 0

  press_reformation_causation__strategic_deployment:
    [critical | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,50,0.35,0.62)
    [critical | confidence: low] coupling_drift
        Evidence: evidence(coupling_score,0.875,threshold,0.25,extraction_trend,increasing)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.41566666666666674,decline_signals,[extraction_rising,coupling_above_threshold(0.875),theater_rising,excess_above_floor(0.44)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  press_reformation_causation__strategic_deployment -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  press_reformation_causation__strategic_deployment (ε=0.62):
    powerless@local: d=0.900 f(d)=1.36 χ = 0.62 × 1.36 × 0.80 = 0.674
    moderate@national: d=0.700 f(d)=1.11 χ = 0.62 × 1.11 × 1.00 = 0.686
    institutional@national: d=0.880 f(d)=1.34 χ = 0.62 × 1.34 × 1.00 = 0.832
    analytical@global: d=0.720 f(d)=1.14 χ = 0.62 × 1.14 × 1.20 = 0.849

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: press_reformation_causation__strategic_deployment ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [asymmetric,coordination,enforcement,has_beneficiaries]
  Voids:      [drifting_without_limit,no_exit_for_victims,unaccountable_extraction]
  Actors:     beneficiaries=distributed  victims=distributed
  Drift:      extraction=rising  suppression=rising  theater=rising
  Zone:       extraction=extreme  suppression=extreme
  Coupling:   strongly_coupled (score=0.875, pairs=[coupled(power_scope,moderate,local-national,1.0),coupled(power_scope,moderate,global-local,1.0),coupled(power_scope,analytical,local-national,1.0),coupled(power_scope,analytical,global-local,1.0)], boltzmann=non_compliant(0.875,0.3))
  Purity:     0.416 (contaminated)




╔════════════════════════════════════════════════════════════════════════════╗
║  VERDICT: RED                                                              ║
║  BASE: YELLOW (12/12 subsystems — 1 tension(s) (abductive)) — CAPPED TO RED║
║  ! [moderate] signature_correction (signature_grade)                       ║
║  ! [severe] type_3_snare_as_rope (claim_mismatch)                          ║
║  Grid: authored 32/32 (injected 0, imputed 0, absent 0)                    ║
╚════════════════════════════════════════════════════════════════════════════╝

--- KERNEL: press_reformation_causation ---
  2 readings | 1 diverging pairs | 0 axiom conflicts
  Reading disagreement: type_c_ambiguity [within_kernel; obstruction=licensed_plurality, coexist_edge]
 *press_reformation_causation__strategic_deployment [terminal=revival]
  press_reformation_causation__mutual_shaping [terminal=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        false_ci_rope
    Purity:           0.415667 (contaminated)
    Coupling:         strongly_coupled (score: 0.875)
    Boltzmann:        non_compliant
    Live index:       both (scope=4, power=3)
    Drift events:     3 — extraction_accumulation [critical], coupling_drift [critical], purity_drift [warning] [3/50 series points authored-as-projected — OQ-102]

--- TEMPORAL TRAJECTORY ---

    [CONDITIONAL: 3/50 authored-as-PROJECTED (guesses, not observations) — OQ-93/OQ-102]
    theater_ratio: ceiling approach — rate 0.0060→0.0000 over T=0–50, flattening near 0.28

--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.4157
    Effective purity:   0.4157
    Propagation delta:  +0.0000

    Network neighbors (1):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | press_reformation_causation__mutual_shaping | piton | explicit | authored | salient | 1.00 | 0.8896 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  No significant contamination — purity unchanged across 1 neighbor(s).

--- ORBIT CONTEXT ---

  Orbit Signature:    [tangled_rope]
  Orbit Span:         1
  Gauge Status:       Gauge-Invariant

--- ENRICHED OMEGA CONTEXT ---

  Not yet enriched — see live omega results in report sections below.
  (Run full pipeline to include in severity scoring and family grouping.)

═══ LEVEL 2: DIAGNOSTIC CONVERGENCE ═══


--- CLASSIFICATION CONVERGENCE ---

    Batch Type:       tangled_rope (powerless)
    MaxEnt P(claimed): 0.0000 (borderline)
    Rival Type:       snare (P=0.9842)
    Margin:           -0.9842
    Boundary:         rope->snare
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     fragile_presheaf — readings glue but high Arakelov complexity (H^1=0, height above corpus p75) [p75 this run: 0.1694]

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: snare at P=0.9842 (pipeline says rope)
  MaxEnt P(claimed): 0.0000 (borderline)
  Rival Type:    snare (P=0.9842)
  Margin:        -0.9842
  Entropy:       0.0453
  Distribution:  snare: 0.984, tangled_rope: 0.016

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      snare (P=0.9880)
  Entropy:       0.0363
  Distribution:  snare: 0.988, tangled_rope: 0.012

  Classical/Indexed TV Distance: 0.0038 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

--- PARAMETRIC PERSISTENCE ---

  [WARNING (OQ-29): persistence_results.json carries no corpus_hash — staleness unverifiable; re-run persistence_sweep.py to stamp it]

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — kernel 'press_reformation_causation' has no confirmed governing params yet
  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)

  Fisher ε-sensitivity (MaxEnt): STALE (OQ-29) — carries no corpus_hash;
  not rendered (would be pre-reset data); re-run python3 python/sweeps/epsilon_sensitivity.py

--- ABDUCTIVE FLAGS ---

  **4 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | convergent_structural_stress | 0.84 | multi_signal_convergence | genuine | 3+ stress indicators converge with a rare anomaly signal — metrically confident but structurally stressed. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |
  | snare_leaning_tangled | 0.75 | high_snare_psi | genuine | Classified tangled_rope but snare-lean ratio exceeds threshold — behaves more like snare than classification suggests. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: scope
  Var_total=0.00652182, Var_fd=0.00499696 (76.6%), Var_scope=0.01176850 (180.4%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.673869 | 1.358606 | 0.8000 |
| moderate | 0.686025 | 1.106492 | 1.0000 |
| institutional | 0.832196 | 1.342252 | 1.0000 |
| analytical | 0.849357 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (7 subsystems):
    cohomology, signature, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (4):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(convergent_structural_stress,0.84,multi_signal_convergence,genuine),trigger(snare_leaning_tangled,0.75,high_snare_psi,genuine),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: diffuse_reconstruction

  Diffuse Reconstruction: The kernel is under-specified or intentionally ambiguous. No centralized authority structure exists to adjudicate. Many parties produce mutually incompatible readings claiming the same source. The pattern persists indefinitely but lacks operational coherence — it often serves strategic purposes for parties who benefit from operational ambiguity. The failure condition is the persistent state rather than an event.

  Structural signals: kernel_distributed, authority_distributed

  ⚠ false_diffuse_reconstruction: Signals conflict with Diffuse Reconstruction claim: suppression or coordination type suggests a concentrated enforcer rather than truly distributed authority.

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor: revival

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 50
Structural Pattern: increasing_coercion
Pattern confidence (categorical): high
Grid diet:      authored 32/32, injected 0, imputed 0 (OQ-93)

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  ! ALERT [severe]: type_3_snare_as_rope detected for press_reformation_causation__strategic_deployment

[STRUCTURAL SIGNATURE ANALYSIS]
  press_reformation_causation__strategic_deployment: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for press_reformation_causation__strategic_deployment: Appears to be rope (explicit_rope_claim) but fails 4 Boltzmann structural test(s): [boltzmann_non_compliant(0.875,0.3),scope_variant([snare,tangled_rope]),excess_above_floor(0.44),nonsensical_coupling(0.3333333333333333)]. Coupling score=0.875. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: protestant_reformers
    → Institutional d=0.880

Aggregate Magnitude (Kappa) at Tn: 0.60 [level coverage 4/4: [structural,organizational,class,individual]] [grid diet: authored 32/32, injected 0, imputed 0 — OQ-93]

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  press_reformation_causation__strategic_deployment: R5 Q6 CROSSCHECK: live_claim_vs_tangled_present daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 4 resolution scenario(s):

  ┌─ [intentionality_threshold] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: press_reformation_causation__strategic_deployment
  │  Question: How much explicit strategic coordination between reformers and printers is required to establish the deployment as intentional rather than opportunistic adaptation?
  │
  │  RESOLUTION METHOD (authored):
  │  Archival analysis of reformer-printer correspondence, business records, contract language, and manuscript commissioning patterns. Identify decision points where either coalition chose printing investment over alternatives.
  │
  │  IMPLICATIONS (authored):
  │  If abundant coordination records exist (letters discussing strategy, negotiated edition sizes, planned text selection), the reading is strongly supported. If coordination is sparse or post-hoc rationalization, the mutual_shaping reading (feedback loops, unplanned synergies) gains plausibility.
  │
  │  Confidence without resolution: high
  └─

  ┌─ [neutral_tool_counterfactual] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: press_reformation_causation__strategic_deployment
  │  Question: Was the printing press truly 'neutral capacity awaiting purposeful use,' or did its material properties (speed, duplication, scalability) constrain what purposes it could serve?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative history: examine printing's early uses in non-reformation contexts (legal documents, commercial printing, devotional texts) and ask whether reformers chose to exploit properties the press already had, or whether the press created new possibilities reformers then pursued.
  │
  │  IMPLICATIONS (authored):
  │  If the press served many purposes equally well pre-reformation, the neutral-tool framing holds. If printing's properties inherently favored replication, scalability, and vernacular circulation (making reformation use more likely than random selection), the technological_determinism reading gains structural support.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [catholic_suppression_counterfactual] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: press_reformation_causation__strategic_deployment
  │  Question: Could catholic institutional authority have suppressed the Reformation without printing (through manuscript confiscation, clergy control, territorial suppression)? Or did printing make suppression structurally impossible?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical comparison with pre-print heresies (Lollards, Hussites, Waldensians) that were suppressed despite wide support. Trace how their suppression differed from reformation suppression, and whether the difference is printing availability or something else (geographic scope, political fragmentation, institutional readiness).
  │
  │  IMPLICATIONS (authored):
  │  If pre-print heresies could be suppressed through the same tools (confiscation, clergy enforcement), printing did not make suppression impossible — it made suppression more costly. This supports strategic_deployment (reformers exploited a costly-to-suppress technology). If printing alone made suppression impossible, technological_determinism gains support.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [kernel_reading_committer_frame] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: press_reformation_causation__strategic_deployment
  │  Question: Is the press_reformation_causation kernel accurately framed as three mutually exclusive readings (strategic_deployment, mutual_shaping, technological_determinism), or are there alternative framings that would decompose the kernel differently?
  │
  │  RESOLUTION METHOD (authored):
  │  Historiographic analysis of how the causal question is framed across different scholarly traditions (history of technology, religious history, social history). Identify whether the three-reading set exhausts the conceptually distinct mechanisms or whether other readings (e.g., institutional readiness as upstream driver, religious crisis as independent motivation) would change the kernel structure.
  │
  │  IMPLICATIONS (authored):
  │  If alternative framings identify genuinely distinct mechanisms, the kernel should be refined to include them as distinct readings. If the three-reading set is structurally complete, the reading distinctions are real.
  │
  │  Confidence without resolution: low
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: network_dcd986fb (network adjacency, n=2)
  Members: press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_ci_rope
    Constraints:       press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment
