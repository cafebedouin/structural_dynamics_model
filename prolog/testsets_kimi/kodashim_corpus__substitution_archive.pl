% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Memorial Archive of Superseded Sacrifice
 *   domain: religious_studies/rabbinic_judaism
 *
 * SUMMARY:
 *   This constraint story models the Kodashim corpus (Mishnah and Talmud
 *   tractates concerning holy things) under the substitution_archive reading
 *   of the kodashim_corpus kernel. In this reading, the corpus does not
 *   preserve an active kernel awaiting restoration, nor does it constitute
 *   the spiritual performance of sacrifice. Rather, it is a memorial archive
 *   that documents a practice rabbinic Judaism has superseded, replacing it
 *   with prayer and Torah study. The archive claims continuity with the
 *   destroyed Temple cult while structurally denying restoration, thereby
 *   coordinating the diaspora community around text and extracting religious
 *   authority from those who would revive sacrificial practice.
 *
 * KEY AGENTS:
 *   - rabbinic_academies (agenda_setter/beneficiary): Institutional authorities that administer the archive, teach its supersession narrative, and concentrate legitimacy in textual study.
 *   - restorationist_practitioners (payer/victim): Individuals and movements seeking to restore sacrifice, structurally excluded by halakhic prohibition and normative substitution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.52).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.58).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.52).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Memorial Archive of Superseded Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '5ba3d499-d7b3-4b85-a63d-b15befaf8470').
narrative_ontology:cs_kernel_codification('5ba3d499-d7b3-4b85-a63d-b15befaf8470', fixed_text).
narrative_ontology:cs_authority_grounding('5ba3d499-d7b3-4b85-a63d-b15befaf8470', lineage).
narrative_ontology:cs_interpretation_layer_present('5ba3d499-d7b3-4b85-a63d-b15befaf8470').
narrative_ontology:cs_reading_relation('5ba3d499-d7b3-4b85-a63d-b15befaf8470', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('5ba3d499-d7b3-4b85-a63d-b15befaf8470', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_axiom('5ba3d499-d7b3-4b85-a63d-b15befaf8470', foundational, sacrifice_superseded_not_suspended).
narrative_ontology:cs_axiom_status(sacrifice_superseded_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('5ba3d499-d7b3-4b85-a63d-b15befaf8470', sacrifice_superseded_not_suspended, conventional).
narrative_ontology:cs_axiom('5ba3d499-d7b3-4b85-a63d-b15befaf8470', foundational, kodashim_functions_as_memorial_archive).
narrative_ontology:cs_axiom_status(kodashim_functions_as_memorial_archive, holdable).
narrative_ontology:cs_axiom_grounding('5ba3d499-d7b3-4b85-a63d-b15befaf8470', kodashim_functions_as_memorial_archive, conventional).
narrative_ontology:cs_reference_frame('5ba3d499-d7b3-4b85-a63d-b15befaf8470', temple_cult_active).
narrative_ontology:cs_drift_state('5ba3d499-d7b3-4b85-a63d-b15befaf8470', rabbinic_hegemony_established, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5ba3d499-d7b3-4b85-a63d-b15befaf8470', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_academies).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, restorationist_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach, codify, and transmit the interpretive tradition that frames the Kodashim corpus as the valid continuity of Israel's cultic life after the Temple's destruction. They concentrate religious authority, student populations, and communal resources in textual study rather than in priestly practice, and they judge halakhic questions about what was superseded.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_academies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_academies, beneficiary).

% Seek to practice or restore sacrificial worship in the present. They are structurally excluded by the halakhic prohibition against sacrifice outside the Temple and by the normative elevation of study as its replacement. Their religious identity is fused with the conviction that the sacrificial kernel remains live, making exit from the archive's framing psychologically and communically costly.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, restorationist_practitioners, payer,
    moderate, biographical, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_academies).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish communal cohesion and covenant continuity after the destruction of the Second Temple by providing a portable, text-centered framework for relating to sacrificial law without the physical infrastructure of the sanctuary.
% TRANSFER_FUNCTION: Moves religious authority, institutional resources, and normative legitimacy from the priestly sacrificial order to rabbinic academies and textual study; moves the status of sacrifice from active commandment to archived object of contemplation.
% ABSENT_VOICES: Restorationist practitioners and priestly descendants who would argue for the immediate resumption of sacrificial worship are absent from the normative halakhic conversation; they are present historically and liturgically but excluded from the academy's agenda-setting process.
% DISAPPEARANCE_RATIONALE: If the archive's claim of continuity-through-substitution vanished, the rabbinic monopoly on post-Temple legitimacy would fracture, priestly and restorationist movements would gain normative ground, and communal liturgy would face pressure to reconstitute itself around sacrificial anticipation rather than textual memorial.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the central site of Israelite worship, creating a crisis of covenant continuity and communal practice.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Roman antiquity and archaeologists corroborate the destruction event and the subsequent rise of synagogue-based Judaism. Restorationist movements corroborate that the problem persists unresolved; rabbinic academies assert the problem is solved through substitution.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the archive redirects substantial religious capital from priestly practice to rabbinic institutions without fully monetizing it. Suppression (0.58) reflects the active halakhic enforcement of Temple-only sacrifice and the communal exclusion of restorationists. Theater ratio (0.48) is elevated because the archive's claims of continuity grow increasingly performative as historical memory of actual sacrifice fades. Accessibility collapse (0.62) is high because, once the archive's frame is accepted, sacrificial alternatives collapse into the category of obsolete or forbidden. Resistance (0.42) comes from persistent restorationist and messianic movements. Measurements share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, the archive is a necessary and legitimate pivot preserving Israel's covenant after catastrophe; from the restorationist seat, the same structure is an exclusionary replacement that captures the kernel's legitimacy while denying its substance. The engine computes this divergence from beneficiary/victim declarations and divergent exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic academies are structural beneficiaries: they collect legitimacy, students, and institutional resources through the archive's centrality, placing them near the beneficiary end (low d). Restorationist practitioners are structural victims: they bear the cost of exclusion and the foreclosure of their preferred practice, with identity-locked exit placing them near the full-target end (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe destruction of the Templeâis two millennia dead, yet the archive persists and actively reshapes communal life. This triggers a mandatrophy mismatch flag (dead problem + world_rearranges). However, the constraint is not a piton because rabbinic academies actively and substantially benefit from its maintenance; it is a tangled rope in which genuine coordination (diaspora survival) is braided with asymmetric extraction (authority monopoly). The theater ratio is rising but has not crossed into dominant performance, confirming the tangled classification over piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_kodashim,
    'This constraint instantiates the substitution_archive reading of the kodashim_corpus kernel. How would classification shift if the study_as_exercise reading (study is the performance) or performance_only reading (awaiting messianic restoration) were adopted instead?',
    'Comparative analysis of the sibling constraint stories generated from the same kernel; examination of which rabbinic authorities explicitly deny restoration versus those who defer it.',
    'Adopting study_as_exercise would likely lower extractiveness by legitimizing study as genuine performance rather than replacement; adopting performance_only would reclassify the corpus as a scaffold or husk, potentially lowering theater_ratio but raising accessibility_collapse if restoration is suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_kodashim, conceptual, 'Sibling reading structural delta for kodashim kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of sacrificial restoration structural (halakhic prohibition, lack of Temple) or internalized (restorationists accept their own exclusion as legitimate deferral)?',
    'Post-messianic or post-Temple-reconstruction trajectory: if structural barriers were removed, would restorationist movements immediately resume practice, or would internalized norms delay resumption?',
    'If suppression is largely internalized, effective extraction is higher than structural measures suggest; the constraint would behave more like a snare even with moderate scalar suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of restorationist practice').

omega_variable(
    archive_continuity_empirical_basis,
    'Does the Kodashim corpus genuinely preserve sacrificial knowledge, or does its editorial framing reconstruct a cultic past that never operated exactly as described?',
    'Archaeological and textual comparison of Mishnah Kodashim against Second Temple historical records and Qumranic halakhic texts.',
    'If the archive is largely a rabbinic reconstruction rather than a preservation, its claim of continuity is performative theater, raising theater_ratio and supporting a more extractive classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_continuity_empirical_basis, empirical, 'Historical accuracy of the archive''s continuity claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_sa_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.12).
narrative_ontology:measurement(kodashim_sa_tr_t200, kodashim_corpus__substitution_archive, theater_ratio, 200, 0.2).
narrative_ontology:measurement(kodashim_sa_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.28).
narrative_ontology:measurement(kodashim_sa_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(kodashim_sa_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(kodashim_sa_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.48).

% Extraction over time
narrative_ontology:measurement(kodashim_sa_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(kodashim_sa_be_t200, kodashim_corpus__substitution_archive, base_extractiveness, 200, 0.28).
narrative_ontology:measurement(kodashim_sa_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(kodashim_sa_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(kodashim_sa_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.48).
narrative_ontology:measurement(kodashim_sa_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_sa_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(kodashim_sa_su_t200, kodashim_corpus__substitution_archive, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(kodashim_sa_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.45).
narrative_ontology:measurement(kodashim_sa_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(kodashim_sa_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(kodashim_sa_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three structurally distinct constraints because the natural-language label 'Kodashim' conflates divergent normative claims about the relationship between text and sacrificial practice. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
