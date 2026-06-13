% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Framework
 *   domain: educational/cognitive
 *
 * SUMMARY:
 *   The whole-language reading framework claims that reading acquisition is a
 *   natural process emerging from meaningful engagement with connected text;
 *   phonics skills develop through exposure and context rather than explicit
 *   instruction. This is ONE READING of the contested
 *   literacy_acquisition_kernel. Sibling readings—phonics, balanced_literacy,
 *   and structured_literacy—instantiate different structural theories of how
 *   decoding emerges and who benefits from different instructional
 *   approaches. The whole-language reading claims low extractiveness on
 *   teacher autonomy (professional judgment is preserved) but high
 *   extractiveness on students without print-rich home literacy support (the
 *   framework assumes background knowledge that not all students have).
 *   Beneficiaries include teachers whose professional identity is anchored in
 *   student-centered pedagogy and students from affluent, print-rich homes;
 *   victims include students lacking home literacy support and students with
 *   dyslexia or phonological processing difficulties.
 *
 * KEY AGENTS:
 *   - whole_language_teachers: agenda_setters implementing the framework; professional identity fused with the reading
 *   - students_without_home_literacy_support: powerless payers bearing the cost of inferring phonics from context alone
 *   - students_with_decoding_difficulties: trapped payers; neurological inefficiency of context-inference approach; motivation damage before late special-ed identification
 *   - education_researchers_phonics_tradition: excluded voices; reading-science evidence discounted as ideologically motivated
 *   - curriculum_publishers_and_adoption_states: institutional beneficiaries; revenue and prestige from adoption cycle
 *   - parents_affluent_literacy_aware: beneficiaries with exit options; supplement school instruction informally
 *   - policymakers_and_administrators: observers facing pressure from reading-science advocates and stagnant proficiency data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Framework").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/cognitive").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '8be608a0-08e9-4d17-bd4a-c0400a7fde4e').
narrative_ontology:cs_kernel_codification('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', distributed).
narrative_ontology:cs_authority_grounding('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', extraction).
narrative_ontology:cs_interpretation_layer_present('8be608a0-08e9-4d17-bd4a-c0400a7fde4e').
narrative_ontology:cs_reading_relation('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', foundational, reading_emerges_from_meaningful_context).
narrative_ontology:cs_axiom_status(reading_emerges_from_meaningful_context, holdable).
narrative_ontology:cs_axiom_grounding('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', reading_emerges_from_meaningful_context, empirically_contingent).
narrative_ontology:cs_axiom('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', foundational, intrinsic_motivation_preserved_by_meaning_first).
narrative_ontology:cs_axiom_status(intrinsic_motivation_preserved_by_meaning_first, holdable).
narrative_ontology:cs_axiom_grounding('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', intrinsic_motivation_preserved_by_meaning_first, deontological).
narrative_ontology:cs_reference_frame('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', natural_emergent_literacy).
narrative_ontology:cs_drift_state('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', contemporary_reading_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8be608a0-08e9-4d17-bd4a-c0400a7fde4e', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teachers_professional_autonomy).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, middle_class_students_with_print_rich_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_or_decoding_difficulties).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at endpoint) because the framework delivers genuine coordination benefits (unified philosophy, preserved teacher autonomy) to print-rich students and teachers while extracting from print-poor and neurodiverse students—not through coercion but through institutional assumption of background knowledge that is not universal. Suppression is high (0.72) because reading-science evidence is systematically excluded from curriculum design, student-centered pedagogy is valorized in teacher training, and there is no institutional pathway for students to access alternative instruction until years into school. Theater rises from 0.35 to 0.58 over the interval: as reading proficiency outcomes stagnate (t=15–25), more of the framework's energy goes to rhetorical defense ('motivation is being preserved,' 'students are constructing meaning') even as the decoding bottleneck persists for non-fluent readers. The measurement series are authored on one shared time grid so every metric value is comparable at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From a whole-language teacher's perspective, the constraint is a rope—it coordinates genuine meaning-making and preserves intrinsic motivation. From a student lacking home literacy support, it is a snare—they are trapped in an extractive arrangement where the hidden cost is years of slow reading and lowered achievement. From a cognitive-science researcher, it is a tangled_rope with high false-positive rate: coordination function is real but asymmetrically distributed (benefits print-rich, harms print-poor). The engine computes directionality from power, exit, and beneficiary/victim declarations and should surface this seat divergence clearly.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole-language teachers sit near the beneficiary end (d near 0.2–0.3): they set the agenda, their professional identity is validated, they have constrained exit (shifting frameworks is a professional-identity threat). Print-rich students and affluent parents sit near beneficiary (d near 0.1–0.2): they benefit from the coordination and have external support (home reading, tutoring). Print-poor and neurodiverse students sit near the target end (d near 0.85–0.95): they bear the cost of missing prerequisites, have no exit, no compensation, and no institutional recourse until years in. The suppression operates through absence of alternatives and structural dependence on school. Curriculum publishers sit near beneficiary (d near 0.15–0.25): they profit from adoption without bearing learning-outcome risk. Policymakers sit at symmetric (d near 0.5): they face pressure from multiple directions and have some authority but are constrained by teacher-union preferences and adoption inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rigid, demotivating phonics instruction) is contested as live or dead. Whole-language proponents say the problem is still live: over-mechanistic instruction damages motivation. Reading scientists and families of struggling readers say the problem has been overcorrected: the solution has inverted the error (now decoding is underemphasized, motivation is preserved at the cost of fluency). The constraint persists because teacher professional identity is fused with the reading (identity_locked exit for many teachers), because curriculum adoption is institutionalized, and because the harm to print-poor students is diffuse and delayed (motivation and confidence damage accumulates over years; special-ed identification comes late). Mandatrophy is present but not resolved: the original mandate (restore meaning to reading) has outlived its function (meaning is fine; the problem now is decoding), but the constraint persists through institutional inertia and identity fusion rather than through active benefit to the original problem-solvers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    home_literacy_prereq_assumption,
    'Is print-rich home literacy support a natural prerequisite to reading acquisition in whole-language instruction, or a contingent background condition that masks educational extraction for print-poor students?',
    'Longitudinal comparison of reading outcomes for students with and without home literacy support under whole-language vs. explicit-phonics instruction; analysis of which variables predict fluency development (home exposure vs. classroom instruction).',
    'If home literacy is natural prerequisite, whole-language instruction is not extractive—it is well-suited to students with that background. If it is a contingent condition, the constraint extracts from students lacking it by assuming what they do not have. This directly determines whether print-poor students are victims or simply not yet ready for this instructional model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(home_literacy_prereq_assumption, empirical, 'Whether whole-language instruction requires prior home literacy as a natural prerequisite or as a hidden assumption.').

omega_variable(
    phonological_processing_necessity,
    'Is explicit instruction in phoneme-grapheme correspondence necessary for students with phonological processing deficits or dyslexia, or can context-based inference work for them given sufficient exposure?',
    'Randomized controlled trials of whole-language vs. systematic phonics instruction for students with documented phonological deficits or dyslexia diagnosis; measurement of fluency, decoding accuracy, and spelling outcomes at matched exposure time.',
    'If explicit phonics is necessary for some neurotypes, whole-language instruction is extractive for those students—it withholds access to an instructional method known to be more efficient for them. If context-based inference works equally well given time, the constraint is non-extractive—it is just slower for some learners but eventually effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_processing_necessity, empirical, 'Whether neurological decoding difficulties require explicit systematic phonics instruction or can be overcome via context-based learning.').

omega_variable(
    motivation_trade_off_boundary,
    'At what point does the preservation of intrinsic motivation trade off against the risk of prolonged non-fluency and reading avoidance? Does explicit phonics instruction in fact damage motivation, or do students who decode efficiently experience greater motivation?',
    'Longitudinal measurement of reading motivation, self-efficacy, and engagement across instruction types and time; qualitative interviews with students experiencing reading difficulty under different frameworks.',
    'If explicit phonics damages motivation net-negatively, whole-language preserves a critical good. If explicit phonics supports motivation by enabling fluency and success, the suppression of phonics instruction harms the very motivation it is meant to protect. This bears directly on the theater_ratio: if suppression of phonics is performed under the banner of motivation preservation but actually decreases motivation for struggling readers, that is a theater signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(motivation_trade_off_boundary, empirical, 'The causal relationship between instructional explicitness and intrinsic reading motivation, especially for struggling readers.').

omega_variable(
    kernel_reading_contest,
    'Is the literacy_acquisition_kernel best read as whole-language (emergent from context) or through one of the sibling readings (phonics-first, balanced, or structured)?',
    'Large-scale comparative outcomes data on reading achievement, fluency, and equity (reading gap by SES, special-ed identification rates) under the different instructional frameworks; meta-analysis of reading-science evidence; policy natural experiments where jurisdictions switch between frameworks.',
    'If reading-science evidence and outcomes data strongly support one reading, the sibling readings may be foreclosed or relegated to special populations. If outcomes are mixed (some readings better for some populations), the readings coexist. The engine computes type per-seat from structural data; this omega documents the empirical contest among the readings themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of the literacy acquisition kernel is supported by evidence and outcomes.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of reading-science evidence and phonics instruction purely structural (institutional adoption patterns, curriculum cycles, publisher incentives) or partly internalized (teachers and teacher educators have fused their professional identity with the reading and resist alternatives)?',
    'Post-policy-shift trajectory: if a jurisdiction mandates balanced or phonics-first instruction, do teachers and ed-programs adapt their practice and training, or do they continue to prioritize whole-language principles covertly? Qualitative research on teacher identity and reading beliefs.',
    'If suppression is purely structural, a policy shift can change outcomes. If suppression is internalized (identity_locked), the constraint persists even after formal policy change—teachers teach whole-language under the label of ''balanced'' or ''phonics plus meaning.'' This affects the classification: an internalized-suppression constraint is more snare-like than a purely structural one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of reading-science evidence is structural or internalized in teacher professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(lite_tr_t35, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(lite_be_t35, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(lite_su_t35, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy_acquisition_kernel is contested across four structurally distinct claims (whole_language, phonics, balanced, structured). Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. This whole-language reading claims low extractiveness on teacher autonomy but high extractiveness on students without print-rich home support; sibling readings will have different structural profiles. All four readings are linked via network.affects_constraints because they compete for institutional adoption and because evidence supporting one reading influences the plausibility of the others. The kernel contest is not resolvable as 'one reading per observable'—it is a genuine institutional dispute about cognitive mechanism. Each reading is a separate constraint story; their coexistence or foreclosure is computed by the engine from reading_relations and the empirical evidence base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
