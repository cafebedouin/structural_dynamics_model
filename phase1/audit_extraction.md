# Phase 1: Audit Extraction

Source: `/home/scott/bin/structural_dynamics_model/docs/trifurcation_mapping_audit.md`

---

## Module Classifications

The audit classifies by "subsystem" not always by individual file. The table below maps each subsystem to the files it names.

| module / subsystem | audit primary classification | evidence quoted from audit (with line #) |
|---|---|---|
| `dirac_classification.pl` (Gauge Orbits) | Type C — clean | Line 85: "The gauge orbit directly answers 'which constraints require position specification to have a determinate type?'" Line 86: "This is the operational content of indexical underspecification applied to constraint classification." |
| `grothendieck_cohomology.pl` (Cohomology H⁰/H¹) | Type C — clean | Line 119: "H¹ is a quantitative measure of indexical underspecification." Line 120: "A constraint with H¹ = 0 has a type-claim that doesn't require position specification." |
| `boltzmann_compliance.pl` (Boltzmann Compliance) | Type B — clean | Line 158: "Boltzmann compliance is a structural consistency test in the precise Type B sense: it checks whether the constraint's behavior is consistent with what the structural axioms require for its claimed type." |
| `purity_scoring.pl` (Purity Score) | Type B — clean (aggregate measure) | Line 185: "Purity score is a continuous measurement of structural inconsistency." Line 186: "A low purity score means the constraint's structural behavior contradicts the properties a well-formed coordination mechanism should have." |
| `signature_detection.pl` (FCR — False CI Rope) | Type B — clean | Line 211: "FCR catches the specific structural inconsistency where the classification label (rope) contradicts what the structural properties require (tangled_rope given coupling + extraction)." |
| `signature_detection.pl` (FSM — False Summit Mountain) | Type B — clean | Line 234: "The contradiction is immediate from the axioms: 'natural law without beneficiary' is a definitional Mountain property. A constraint that satisfies the metric conditions but has constraint_beneficiary facts asserted in the testset is axiomatically inconsistent with Mountain classification." |
| `signature_detection.pl` (FNL — False Natural Law) | Type B — clean | Line 257: "FNL is identical in structure to FSM but targets a different inconsistency: claimed origin (natural) vs. actual behavior (coupled)." |
| `signature_detection.pl` / `structural_signatures.pl` (Structural Signatures) | Type B — clean, but layered | Line 283: "Structural signatures detect the inconsistency between a constraint's structural origin and its metric-implied type." Line 285: "The sub-signatures extend this by also detecting internal structural inconsistency within the constructed category." |
| `drift_events.pl` / `transition_paths.pl` / `network_dynamics.pl` / `drift_report.pl` / `drl_lifecycle.pl` (Drift Analysis) | Type A — clean | Line 324: "Drift analysis is a direct operationalization of the 'unmarked state mutation' that the trifurcation identifies as the Type A mechanism." Line 341: "This is the only primary Type A subsystem in the apparatus." |
| `arakelov_height.pl` (Arakelov Heights) | Partial Type B | Line 375: "Arakelov height measures proximity to structural inconsistency... It's not a Type B detection in the full sense (no contradiction is yet manifest), but it predicts which constraints are most vulnerable to Type B failures under small parameter changes." Line 386: "Not Type A: no temporal process is involved; this is a static metric." |
| `python/epsilon_sensitivity.py` (Fisher Curvature) | Partial Type B | Line 409: "Fisher curvature refines the Arakelov height diagnostic for the consensus manifold. Both measure proximity to Type B failures at the continuous level." Line 416: "Fisher curvature is a Python-only diagnostic — not a Prolog subsystem." |
| `dirac_classification.pl` (Dirac Orbits — full module) | Cross-cutting B + C | Line 443: "`dirac_class` (first/second-class decomposition, separability) targets Type B: it characterizes the structural properties of the constraint." Line 444: "`gauge_fixed/3` targets Type C: it detects that an observer is in a position-fixed frame where they cannot see the full constraint structure." |
| `maxent_classifier.pl` + `measurement_layer.pl` (MaxEnt Shadow + Wasserstein W₁) | Cross-cutting C + partial B | Line 486: "W₁ measures the continuous analog of indexical underspecification... This is Type C at the distributional level." Line 489: "MaxEnt hard disagreements (deterministic says X, probabilistic says Y) are partial Type B." |
| `report_generator.pl` (Mandatrophy Gap) | Partial C, partial B | Line 519: "The gap measurement (delta_chi) is Type C: it quantifies how different the answer to 'what type is this?' is depending on which observer position you're at." Line 522: "Mandatrophy declaration is Type B: a constraint declared mandatrophy has a structural property... that is immediately inconsistent with any response strategy other than containment." |
| `python/evaluative_convergence.py` (Evaluative Convergence) | Cross-cutting A + B, partially outside | Line 555: "`convergent_drift`: Type A — detects the group-level version of frame drift." Line 557: "`convergent_institutional` + `cover_story_topology`: Type B — both detect structural inconsistency at the group level." Line 562: "`convergent_signature`: **Outside the trifurcation.**" |
| `drl_purity_network.pl` + `drl_fpn.pl` (Network Purity Propagation + FPN) | Outside | Line 594: "A rope whose effective_purity has degraded due to contamination from neighboring snares has not drifted (no temporal process), has no internal axiomatic inconsistency, and is not indexically underspecified. The failure is structural coupling — which the trifurcation doesn't name." |
| `drl_boltzmann_analysis.pl` (Nash Distance / Game Theory) | Partial B (embedded in Dirac) | Line 630: "Nash distance measures minimum single-observer reclassifications required to reach H¹=0. Maps to **partial B** (structural distance to consistency) and is embedded in Dirac orbits." |
| `python/cognitive_displacement_sweep.py` (Cognitive Displacement) | Partial C | Line 637: "Tests whether structural invariants survive intra-position calibration variation. Maps to **partial C** — probes the robustness of the indexical structure under observer calibration noise." |
| `logical_fingerprint.pl` (Logical Fingerprint) | Cross-cutting A + B + C | Line 641: "Cross-cutting: the zone dimension captures metric position (partial B), the shift dimension captures perspectival structure (partial C), the drift dimension captures temporal change (partial A). The fingerprint subsystem is itself a multi-type instrument." |

---

## Audit's Type A Coverage Claims

All quotes are from `/home/scott/bin/structural_dynamics_model/docs/trifurcation_mapping_audit.md`.

**Primary Type A "thin" finding:**

Line 35–36: "**Type A (frame drift):** One subsystem — drift analysis — has a clean Type A mapping. The trifurcation's richest category (five subcategories: vagueness drift, infinite subdivision, epistemic updates, identity criteria switching, decision framework oscillation) is represented by a single instrument."

Line 341 (Note on thinness): "This is the only primary Type A subsystem in the apparatus. The trifurcation's Type A was the richest category (Sorites, Zeno, Surprise Exam, Ship of Theseus, Newcomb — five subcategories). The apparatus has one instrument for this entire space. See Aggregate Analysis for interpretation."

Line 676–678 (summary table): "Type A: 1 clean, 1 partial (evaluative convergence convergent_drift)"

Line 697–698 (Coverage Check): "**Type A (frame drift):** Thinly covered — one clean subsystem (drift_events.pl). This is the most significant coverage gap relative to the trifurcation's emphasis."

**What the audit considered as Type A:**

Line 304–340: The audit inspects `drift_events.pl` in detail. It lists nine detectors (`detect_metric_substitution/1`, `detect_extraction_accumulation/1`, `detect_coordination_loss/1`, `detect_function_obsolescence/1`, `detect_sunset_violation/1`, `detect_extraction_dried_up/1`, `detect_is_piton/1`, `detect_coupling_drift/1`, `detect_purity_drift/1`), plus `drift_velocity/3` and `drift_acceleration/3`.

Line 305–306: "**Operational description:** `drift_events.pl` detects seven temporal change patterns in constraint properties..." (the audit lists 9 but describes them as mapping to Type A).

Line 330: "Drift analysis is a direct operationalization of the 'unmarked state mutation' that the trifurcation identifies as the Type A mechanism."

Line 332: "The apparatus version: a constraint whose structural properties have changed (extraction accumulating, coordination dissolving, theater rising) while the classification label remains unchanged."

**What the audit explicitly excluded from Type A:**

Line 385–387: "Not Type A: no temporal process is involved; this is a static metric." (about Arakelov Heights)

Line 639 (Cognitive Displacement): mapped to "partial C" not Type A.

Line 641–643 (Logical Fingerprint): the drift dimension is flagged as "partial A" within a cross-cutting instrument but the module is classified overall as cross-cutting A + B + C.

Line 554–565 (Evaluative Convergence): `convergent_drift` is flagged as "Type A" but the module overall is cross-cutting A + B + outside.

**The audit's explanation for why Type A is thin:**

Lines 700–710: "The trifurcation was developed for philosophical reasoning episodes — individual acts of analysis where the analyst's frame drifts over minutes or hours. The apparatus analyzes structural properties of social constraints, with temporal evidence embedded in constraint measurement histories (narrative_ontology:measurement/5 facts with timestamps). Type A in the apparatus requires temporal data in the testsets. The majority of testsets contain point-in-time structural data, not longitudinal measurement series. Where temporal data exists, drift_events.pl detects it. Where it doesn't, there's nothing for a Type A detector to work with. The coverage gap is partly a data availability constraint, not purely an architectural omission."

**What the audit did NOT examine at predicate granularity:**

The audit does not inspect predicates in `drl_composition.pl` (`constraint_history/3`, `dr_type_at/3`, `transformation_detected/5`, `transformation_type/6`, `canonical_transformation/6`, `predict_transformation/3`) at all. It mentions `drl_composition.pl` only obliquely via `drl_modal_logic.pl` facade mention (line 455: "via drl_modal_logic facade").

The audit does not examine `transition_paths.pl` as a distinct Type A module — it groups it under "Drift Analysis" (Subsystem 9) which is classified as the single clean Type A subsystem, and the narrative treats the lifecycle split (drift_events + transition_paths + network_dynamics + drift_report → drl_lifecycle) as a single subsystem.

The audit does not examine `coercion_projection.pl` predicates (`coercion_gradient/4`, `time_point_in_interval/2`, `coercion_vector/3`, `coercion_magnitude/3`) for Type A content.

The audit does not examine `pattern_analysis.pl` or `intent_engine.pl` for Type A content.

The audit does not examine `drl_counterfactual.pl` predicates (`dr_gradient_at/3`, `infer_structural_coupling/3`) for Type A content.

The audit does not examine `network_dynamics.pl` predicates (`network_drift_velocity/4`, `cascade_prediction/3`) individually — they are grouped under the "outside trifurcation" network purity category.

**Modules the audit classifies as primary NOT-Type-A that may contain Type A predicates:**

- `drl_composition.pl` — classified as part of the drl_modal_logic facade, whose subsystems are: composition (Type B via classify_from_metrics), counterfactual (not explicitly classified), Boltzmann analysis (partial B).
- `transition_paths.pl` — grouped under "Drift Analysis" (Type A) via the drl_lifecycle facade, but the audit does not distinguish it from drift_events.pl.
- `network_dynamics.pl` — classified as "Outside" (network contamination), but contains `network_drift_velocity/4` and `cascade_prediction/3` which are temporal/progression predicates.
- `coercion_projection.pl` — not mentioned in the audit at all.
- `pattern_analysis.pl` — not mentioned in the audit at all.
- `intent_engine.pl` — not mentioned in the audit at all.
