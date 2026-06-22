CORPUS CONTEXT: 80 constraints
  Types: 13 mountain, 17 rope, 33 tangled_rope, 7 snare, 5 scaffold
  Network stability: cascading | 32 omegas (32 critical)
  MaxEnt bands (corpus): 15 deep (20%) | 2 moderate (3%) | 58 borderline (77%)
  CS patterns: 69 classified | 3 anchored_fixity_with_accretion, 8 diffuse_reconstruction, 1 epistemic_consensus, 2 implicit_practice, 26 interpretive_accretion, 4 marked_revision | 31 verdicts fired
  CS grounding mismatches: 51 grounding-metric conflicts

====================================================
   STRUCTURAL ANALYSIS STACK INITIALIZED             
====================================================
Control:    Scenario Manager Active
Usage:      run_scenario('file.pl', interval_id).
====================================================

[SCENARIO MANAGER] Clearing Knowledge Base...
[OK] Knowledge Base is empty.
[SCENARIO MANAGER] Loading: testsets/fictional_construct_reading.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: fictional_construct_reading...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: fictional_construct_reading

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: fictional_construct_reading (0-20)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 20 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 20 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 20 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 20 in fictional_construct_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] fictional_construct_reading: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 1 | Watch: 1

  fictional_construct_reading:
    [watch | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,20,0.25,0.28)
    [warning | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.7538333333333334,decline_signals,[coupling_above_threshold(0.375),excess_above_floor(0.23000000000000004)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  fictional_construct_reading -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  fictional_construct_reading (ε=0.28):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.28 × 1.39 × 0.80 = 0.312
    moderate@national: d=0.650 f(d)=1.01 χ = 0.28 × 1.01 × 1.00 = 0.282
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.28 × -0.04 × 1.00 = -0.012
    analytical@global: d=0.720 f(d)=1.14 χ = 0.28 × 1.14 × 1.20 = 0.384

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: fictional_construct_reading ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=stable  theater=stable
  Zone:       extraction=low  suppression=moderate
  Coupling:   weakly_coupled (score=0.375, pairs=[coupled(power_scope,powerless,global-local,1.0),coupled(power_scope,powerless,global-national,1.0)], boltzmann=non_compliant(0.375,0.25))
  Purity:     0.754 (sound)




╔════════════════════════════════════════════════════════╗
║  VERDICT: YELLOW                                       ║
║  12/12 subsystems — 1 tension(s) (abductive)           ║
║  ! [moderate] signature_correction (signature_grade)   ║
║  Grid: authored 0/32 (injected 0, imputed 0, absent 32)║
╚════════════════════════════════════════════════════════╝

--- KERNEL: polaris_document_status ---
  4 readings | 3 diverging pairs | 2 axiom conflicts
  authoritative_specification_reading [terminal=stable_pattern]
  pre_public_initiative_reading [terminal=stable_pattern]
 *fictional_construct_reading [terminal=stable_pattern]
  conceptual_framework_reading [terminal=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        false_ci_rope
    Purity:           0.753833 (sound)
    Coupling:         weakly_coupled (score: 0.375)
    Boltzmann:        non_compliant
    Live index:       both (scope=2, power=1)
    Drift events:     3 — extraction_accumulation [watch], purity_drift [warning], network_drift [critical]


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.7538
    Effective purity:   0.5737
    Propagation delta:  -0.1801

    Network neighbors (3):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | authoritative_specification_reading | snare | explicit | authored | salient | 1.00 | 0.3542 |
    | conceptual_framework_reading | tangled_rope | explicit | authored | salient | 1.00 | 0.6524 |
    | pre_public_initiative_reading | unknown | explicit | authored | salient | 1.00 | 0.3932 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  Purity degraded from 0.7538 to 0.5737 by contamination from 3 neighbor(s), primarily authoritative_specification_reading (explicit, authored, purity 0.3542).

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
    Rival Type:       scaffold (P=0.9317)
    Margin:           -0.9317
    Boundary:         rope->scaffold
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.2119]

--- MAXENT SHADOW CLASSIFICATION ---

  PIPELINE CLASSIFICATION REJECTED by MaxEnt: scaffold at P=0.9317 (pipeline says rope)
  MaxEnt P(claimed): 0.0000 (borderline)
  Rival Type:    scaffold (P=0.9317)
  Margin:        -0.9317
  Entropy:       0.1393
  Distribution:  scaffold: 0.932, tangled_rope: 0.068, rope: 0.000

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      scaffold (P=0.8689)
  Entropy:       0.2170
  Distribution:  scaffold: 0.869, tangled_rope: 0.131, rope: 0.000

  Classical/Indexed TV Distance: 0.0628 (moderate)
  Moderate divergence — observer-dependence shifts probabilistic weights without changing the top classification.

--- PARAMETRIC PERSISTENCE ---

  No persistence bars for this constraint across swept parameters.

--- PARAMETRIC STABILITY BAND ---

  stability not assessed — kernel 'polaris_document_status' has no confirmed governing params yet
  (witness required: coverage>0 AND fold_survival<1.0 in ≥1 context)

  Fisher ε-sensitivity (MaxEnt): not computed
  (run python3 python/sweeps/epsilon_sensitivity.py to compute)

--- ABDUCTIVE FLAGS ---

  **2 trigger(s) fired:**

  | Trigger Class | Confidence | Anomaly | Category | Interpretation |
  |---|---|---|---|---|
  | signature_override_artifact | 0.95 | hard_disagreement_with_override | artifact | Metric disagreement explained by a known signature override — architectural artifact, not a genuine anomaly. |
  | epistemic_trap | 0.78 | restricted_view_divergence | genuine | Powerless observer's restricted classification diverges from full-data view — trapped in gauge-fixed frame. |

### AXIOM 2: Chi/Epsilon Decomposition

**Dominant factor**: directionality
  Var_total=0.02275261, Var_fd=0.02349160 (103.2%), Var_scope=0.00120113 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.312020 | 1.392945 | 0.8000 |
| moderate | 0.282412 | 1.008614 | 1.0000 |
| institutional | -0.011831 | -0.042252 | 1.0000 |
| analytical | 0.383581 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    cohomology, signature, purity, fingerprint_voids, drift, context_gap, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    boltzmann: constructed_non_compliance
      Constructed types couple dimensions deliberately; non-compliance is confirmatory
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — violated): Classification does NOT factor through a single Boltzmann distribution. Observer positions are thermodynamically coupled — the constraint's type depends on which observers you condition on, not just their individual measurements.

  **1 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  No CS pattern detected — signals ambiguous or field combination unrecognized.
  (Declared signals: anomalous_field_combination)
--- COMMITMENT SYSTEM TEMPORAL STATUS ---
  Drift terminal attractor: stable_pattern

====================================================
   DR DETAILED ANALYSIS                             
====================================================
Timeline:       0 to 20
Structural Pattern: open(no_gradient_data)
Pattern confidence (categorical): low
Grid diet:      authored 0/32, injected 0, imputed 0 (OQ-93) [CONDITIONAL: grid authored 0/32]

[META-LOGICAL AUDIT: DECLARED-TYPE vs OWN-ASSIGNED-METRICS SELF-CONSISTENCY]
  No classification errors detected. System is Ontologically Coherent.

[STRUCTURAL SIGNATURE ANALYSIS]
  fictional_construct_reading: false_ci_rope (confidence: high)
    → FALSE CI_ROPE signature for fictional_construct_reading: Appears to be rope (explicit_rope_claim) but fails 3 Boltzmann structural test(s): [boltzmann_non_compliant(0.375,0.25),excess_above_floor(0.23000000000000004),nonsensical_coupling(0.16666666666666666)]. Coupling score=0.375. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: narrative_designers
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  fictional_construct_reading: R5 Q6 CROSSCHECK: live_claim_vs_tangled_present daylight(unstated)  (structural mismatch; orientation not witnessed at this tier)
  fictional_construct_reading: EXTRACTION READING: extractive constraint-level type with no authored victim; beneficiary-side seats = [narrative_designers,systems_pedagogy_practitioners,worldbuilding_communities]; cost-bearer named only in the authored situation/transfer narrative (commentary, non-classifying; OQ-85/OQ-86).

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [authorial_intent_vs_reception] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: fictional_construct_reading
  │  Question: Does the document's status as fictional construct depend on authorial intent, or can it be established by reception and use patterns alone?
  │
  │  RESOLUTION METHOD (authored):
  │  Examination of document provenance, author statements, and actual use cases. If authors explicitly disclaim implementation intent and users treat it as worldbuilding, the fictional status is corroborated. If authors claim implementation intent but users ignore it, reception overrides intent.
  │
  │  IMPLICATIONS (authored):
  │  If intent is dispositive, the reading's validity depends on recovering authorial claims. If reception is dispositive, the reading is validated by observed use patterns regardless of authorial intent. The former makes this a conceptual question about document ontology; the latter makes it an empirical question about community practice.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [pedagogical_vs_deceptive_framing] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: fictional_construct_reading
  │  Question: Is presenting fictional specifications in technical format pedagogically valuable (teaching systems thinking through realistic artifacts) or deceptive (blurring the boundary between speculation and engineering)?
  │
  │  RESOLUTION METHOD (authored):
  │  Pedagogical outcomes research: do students taught with fictional specifications develop better systems thinking skills, or do they develop confusion about epistemic standards? Comparison with alternative teaching methods (explicitly labeled toy problems, real-world case studies).
  │
  │  IMPLICATIONS (authored):
  │  If pedagogically valuable, the constraint's coordination function is genuine and the format choice is justified. If deceptive, the format itself becomes an extractive mechanism—imposing cognitive costs on readers who must determine whether claims are fictional or factual. This would raise extractiveness substantially and potentially reclassify the constraint.
  │
  │  Confidence without resolution: low
  └─

  ┌─ [fictional_construct_vs_failed_initiative] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: fictional_construct_reading
  │  Question: Is Polaris a fictional construct by design, or a failed real-world initiative retroactively reframed as fiction?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical investigation: timeline of document development, funding sources, organizational affiliations, deployment attempts. A fictional construct would show no implementation attempts; a failed initiative would show abandoned deployment efforts followed by narrative reframing.
  │
  │  IMPLICATIONS (authored):
  │  If fictional by design, this reading is accurate and extractiveness remains low. If a failed initiative reframed as fiction, the constraint's history includes a period of higher extraction (compliance demands, resource allocation) that has since collapsed, making this a piton rather than a rope—the coordination function has atrophied but the document persists as institutional artifact.
  │
  │  Confidence without resolution: medium
  └─

====================================================


═══ CROSS-CONSTRAINT CONVERGENCE ═══

  Set: network_04f7cc8a (network adjacency, n=4)
  Members: authoritative_specification_reading, conceptual_framework_reading, fictional_construct_reading, pre_public_initiative_reading

  --- CONVERGENCE PATTERNS ---

  [convergent_signature]
    Shared signature:  false_ci_rope
    Constraints:       conceptual_framework_reading, fictional_construct_reading, pre_public_initiative_reading

  [convergent_drift]
    Drift type:   network_drift
    Severity:     critical
    Constraints:  conceptual_framework_reading, fictional_construct_reading

  --- DEFENSIBILITY ASSESSMENT ---

  Constrained positions:
    - Type classifications for this set should be treated as temporally unstable pending drift resolution.

  Indefensible positions:
    Position: Current type classifications for all constraints in this set are stable
    Ruled out by: Convergent critical-severity drift (network_drift) across 2 constraints indicates active systemic instability, not constraint-local drift.
