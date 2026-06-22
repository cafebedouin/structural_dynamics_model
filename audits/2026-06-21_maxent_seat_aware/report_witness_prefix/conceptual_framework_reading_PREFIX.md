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
[SCENARIO MANAGER] Loading: testsets/conceptual_framework_reading.pl...
[SHIM] grid injection RETIRED (OQ-93 ruling (b), authored-or-absent) — absent grid points stay OPEN
[SCENARIO MANAGER] Performing Global Repair...

[REPAIR] Auditing vectors for: conceptual_framework_reading...
  [OPEN] grid imputation RETIRED (OQ-93 ruling (b), authored-or-absent): 32/32 grid points absent — expected-and-witnessed
  [PROVENANCE] grid 32 = authored 0 + injected-0.5 0 (m_gen) + imputed-from-priors 0 (repair_m_*)
  [PROVENANCE] leveled grid is unauthorable under the live generation schema — contract gap, see ISSUES.md OQ-93
  [WARN] 32 grid slots still absent after repair

>>> INITIATING DR-AUDIT SUITE: conceptual_framework_reading

--- [START] Data Verification ---
[OK] Ontology Schema matches.
Checking Interval: conceptual_framework_reading (0-20)
  [OPEN] 4/4 grid components absent for Level: structural at T: 0 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: structural at T: 20 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 0 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: organizational at T: 20 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 0 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: class at T: 20 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 0 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
  [OPEN] 4/4 grid components absent for Level: individual at T: 20 in conceptual_framework_reading (shim disabled — absence expected-and-witnessed, OQ-93/OQ-96)
--- [END] Data Verification Complete ---
[OK] Verification passed (grid completeness is guaranteed by Stage-1 repair — non-discriminating post-repair, OQ-93).

--- PER-INDEX VALIDATION ---
  [INDEX VACUOUS] conceptual_framework_reading: no authored classifications — ZERO per-index checks ran (not a clean pass)
  [INTENT] Result: OPEN (no_gradient_data) — no pattern verdict; leveled grid absent or below named-level coverage (OQ-93) [grid diet: authored 0/32, injected 0, imputed 0 — OQ-93]

--- LIFECYCLE DRIFT ANALYSIS ---

================================================================
  DRIFT EVENT REPORT
================================================================

  Constraints scanned: 1
  Total drift events:  2
  Critical: 0 | Warning: 0 | Watch: 2

  conceptual_framework_reading:
    [watch | confidence: low] extraction_accumulation
        Evidence: evidence(extraction_delta,0,20,0.1,0.12)
    [watch | confidence: low] purity_drift
        Evidence: evidence(current_purity,0.9720000000000001,decline_signals,[excess_above_floor(0.06999999999999999)])

--- Transition Path Analysis ---

--- Terminal State Predictions ---
  conceptual_framework_reading -> tangled_rope (confidence: low)

--- Network Drift Analysis ---
  Network stability: stable

================================================================

--- SCOPE EFFECT ANALYSIS ---
  Formula: χ = ε × f(d) × σ(S)
  conceptual_framework_reading (ε=0.12):
    powerless@local: d=0.950 f(d)=1.39 χ = 0.12 × 1.39 × 0.80 = 0.134
    moderate@national: d=0.650 f(d)=1.01 χ = 0.12 × 1.01 × 1.00 = 0.121
    institutional@national: d=0.120 f(d)=-0.04 χ = 0.12 × -0.04 × 1.00 = -0.005
    analytical@global: d=0.720 f(d)=1.14 χ = 0.12 × 1.14 × 1.20 = 0.164

--- WASSERSTEIN TRANSPORT ---
  Corpus perspectival fracture (W1): 0.0000

--- LOGICAL FINGERPRINT ---

=== Logical Fingerprint: conceptual_framework_reading ===
  Shift (computed via dr_type/3):
    powerless=tangled_rope  moderate=tangled_rope  institutional=tangled_rope  analytical=tangled_rope
  Properties: [coordination,has_beneficiaries]
  Voids:      []
  Actors:     beneficiaries=distributed  victims=none
  Drift:      extraction=stable  suppression=stable  theater=stable
  Zone:       extraction=negligible  suppression=low
  Coupling:   independent (score=0.000, pairs=[], boltzmann=compliant(0))
  Purity:     0.972 (pristine)




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
  fictional_construct_reading [terminal=stable_pattern]
 *conceptual_framework_reading [terminal=stable_pattern]


═══ LEVEL 1: SELF-CONSISTENCY ═══


--- CONSTRAINT IDENTITY ---

    Claimed Type:     rope
    Routing (per-seat author↔engine diff — OQ-128 sink; a review label, certifies nothing):
      powerless:     author_engine_divergence
      moderate:      author_engine_divergence
      institutional: author_engine_divergence
      analytical:    author_engine_divergence
    Signature:        false_ci_rope
    Purity:           0.972 (pristine)
    Coupling:         independent (score: 0)
    Boltzmann:        compliant
    Live index:       none (scope=0, power=0)
    Drift events:     3 — extraction_accumulation [watch], purity_drift [watch], network_drift [critical]


--- CONTAMINATION NETWORK ---

    Intrinsic purity:   0.9720
    Effective purity:   0.6524
    Propagation delta:  -0.3196

    Network neighbors (2):

    | Neighbor | Type | Edge | Provenance | Salience | Strength | Purity |
    |----------|------|------|------------|----------|----------|--------|
    | authoritative_specification_reading | snare | explicit | authored | salient | 1.00 | 0.3542 |
    | fictional_construct_reading | tangled_rope | explicit | authored | salient | 1.00 | 0.5737 |

    Provenance: 'authored' = an affects_constraint link declared in this case's testset (the source material asserts the connection); 'corpus-derived' = an edge the engine computed from corpus topology (two constraints naming the same beneficiary/victim), NOT asserted by this case. Salience floor: a corpus-derived agent edge counts as 'salient' only when the two constraints share ≥2 agents; a single shared agent (strength 0.30) is weak corpus scaffolding.

  Purity degraded from 0.9720 to 0.6524 by contamination from 2 neighbor(s), primarily authoritative_specification_reading (explicit, authored, purity 0.3542).

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
    MaxEnt P(claimed): 0.9185 (deep)
    Rival Type:       piton (P=0.0801)
    Margin:           +0.8384
    Boundary:         rope->piton
    H^1 band:         0 — All observers agree. Neither hub produces classification divergence.
    Sheaf status:     genuine_sheaf — local readings glue — global section exists (H^1=0, height below corpus p75) [p75 this run: 0.2119]

--- MAXENT SHADOW CLASSIFICATION ---

  Classification is stable (low entropy, types agree)
  MaxEnt P(claimed): 0.9185 (deep)
  Rival Type:    piton (P=0.0801)
  Margin:        +0.8384
  Entropy:       0.1618
  Distribution:  rope: 0.919, piton: 0.080, scaffold: 0.001

  Indexed MaxEnt (χ-scaled, analytical context):
  Top Type:      rope (P=0.9179)
  Entropy:       0.1643
  Distribution:  rope: 0.918, piton: 0.080, scaffold: 0.002

  Classical/Indexed TV Distance: 0.0007 (near_zero)
  Classical and indexed MaxEnt agree — observer-dependence does not alter probabilistic classification.

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
  Var_total=0.00417904, Var_fd=0.00431478 (103.2%), Var_scope=0.00022062 (5.3%)

| Perspective | chi | f(d) | scope_mod |
| :--- | ---: | ---: | ---: |
| powerless | 0.133723 | 1.392945 | 0.8000 |
| moderate | 0.121034 | 1.008614 | 1.0000 |
| institutional | -0.005070 | -0.042252 | 1.0000 |
| analytical | 0.164392 | 1.141609 | 1.2000 |


--- DIAGNOSTIC VERDICT ---

  Subsystems Checked: 12/12

  Agreements (8 subsystems):
    cohomology, signature, boltzmann, purity, fingerprint_voids, drift, fcr_gate, gauge_orbit

  Expected Conflicts (3):
    maxent: signature_override_artifact
      MaxEnt disagrees because signature override forces type
    dirac: pre_post_override_divergence
      Dirac class reflects metric-layer type, not override type
    context_gap: pre_post_override_divergence
      Restricted classifier sees pre-override metric-based type

  Convergent Rejections: none

  Tensions (1):
    abductive: abductive_tension([trigger(signature_override_artifact,0.95,hard_disagreement_with_override,artifact),trigger(epistemic_trap,0.78,restricted_view_divergence,genuine)])

--- THEOREM INSTANTIATION ---

  T5 (Functor Axiom — satisfied): Classification across index dimensions factors through a single Boltzmann distribution. The constraint's type assignments are thermodynamically consistent — no hidden coupling between observer positions.

  **1 of 6 theorems active.**

--- COMMITMENT SYSTEM PATTERN ---

  Pattern: marked_revision

  Marked Revision: The kernel is precisely specified; authority is voluntary and grounded in expertise. Drift is formalized as a proposal-check-absorb cycle — acknowledgment is marked and legible rather than silently absorbed. Mathematics works this way; healthy long-term relationships and programming languages with formal deprecation procedures work this way. The pattern is stable when acknowledgment capacity matches environmental change rate. Failure mode: authority structure develops extraction stakes in kernel preservation that did not exist at founding.

  Structural signals: kernel_formalized, authority_expertise

  See: docs/commitment_systems/commitment_systems_sketch_v5_2.md
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
  conceptual_framework_reading: false_ci_rope (confidence: low)
    → FALSE CI_ROPE signature for conceptual_framework_reading: Appears to be rope (explicit_rope_claim) but fails 1 Boltzmann structural test(s): [excess_above_floor(0.06999999999999999)]. Coupling score=0. This constraint is "coordination-washed" — it hides extraction behind low metrics, distributed enforcement, or behavioral defaults.
    → Institutional beneficiary: architectural_thinkers
    → Institutional d=0.120

Aggregate Magnitude (Kappa): DATA_INSUFFICIENT

[MANDATROPHY GAP ANALYSIS]
  (Full perspectival detail in Levels 1-2 above)
  conceptual_framework_reading: EXTRACTION READING: extractive constraint-level type with no authored victim; beneficiary-side seats = [architectural_thinkers,design_theorists,pattern_researchers]; cost-bearer named only in the authored situation/transfer narrative (commentary, non-classifying; OQ-85/OQ-86).

[OMEGA RESOLUTION SCENARIO GENERATION]
  Generated 3 resolution scenario(s):

  ┌─ [pattern_language_sufficiency] AUTHORED RESOLUTION PROTOCOL (conceptual)
  │  Constraint: conceptual_framework_reading
  │  Question: Can a governance pattern language be epistemically complete without organizational instantiation, or does lack of implementation evidence undermine its validity?
  │
  │  RESOLUTION METHOD (authored):
  │  Comparative analysis of other pattern languages (e.g., Alexander's architectural patterns, Gang of Four design patterns) that gained acceptance without requiring the pattern authors to build exemplar structures. If pattern validity tracks conceptual coherence rather than implementation proof, this reading is vindicated.
  │
  │  IMPLICATIONS (authored):
  │  If pattern languages require implementation evidence, this reading collapses into the pre_public_initiative_reading (Polaris must be building something) or fictional_construct_reading (it's just speculation). If patterns can be valid without instantiation, this reading is structurally stable.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [thought_experiment_organizational_boundary] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: conceptual_framework_reading
  │  Question: At what point does a thought experiment's elaboration cross into implicit organizational claim? Is there a level of specification detail that makes 'this is just a framework' untenable?
  │
  │  RESOLUTION METHOD (authored):
  │  Examination of Polaris document specificity: if documents specify operational procedures, resource flows, or authority structures in implementation-ready detail, the thought-experiment framing becomes strained. If they remain at the abstraction level of design principles, the framing holds.
  │
  │  IMPLICATIONS (authored):
  │  If Polaris documents are implementation-ready specifications, this reading's bracketing of instantiation is evasive rather than principled, and the authoritative_specification_reading or pre_public_initiative_reading becomes more coherent. If documents stay abstract, this reading is defensible.
  │
  │  Confidence without resolution: medium
  └─

  ┌─ [influence_without_authority] AUTHORED RESOLUTION PROTOCOL (empirical)
  │  Constraint: conceptual_framework_reading
  │  Question: Can a framework that claims no organizational authority still shape real institutional design, or does influence require some form of instantiation?
  │
  │  RESOLUTION METHOD (authored):
  │  Historical analysis of whether Polaris patterns appear in real governance structures built by others. If patterns diffuse into practice without Polaris instantiating, the framework's influence is demonstrated. If no uptake occurs, the reading's claim that conceptual value is independent of instantiation is weakened.
  │
  │  IMPLICATIONS (authored):
  │  If patterns diffuse, this reading is vindicated: thought experiments can coordinate thinking without organizational grounding. If no diffusion occurs, the reading's utility claim is undermined, though its epistemic framing could still be coherent.
  │
  │  Confidence without resolution: low
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
