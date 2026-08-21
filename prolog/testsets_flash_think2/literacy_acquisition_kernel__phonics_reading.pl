% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition Mandate
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'phonics-first' reading of the literacy
 *   acquisition kernel, asserting that explicit, systematic instruction in
 *   phoneme-grapheme correspondence is a prerequisite for connected text
 *   exposure and comprehension. It is a core tenet of the 'Science of
 *   Reading' movement. While it aims to coordinate effective literacy
 *   instruction, it also extracts from teacher autonomy by mandating specific
 *   instructional methods and curriculum, and suppresses alternative
 *   pedagogical philosophies. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates a complex educational problem (literacy
 *   acquisition) but does so with significant, asymmetric extraction from
 *   teachers' professional judgment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Acquisition Mandate").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '16b0fba5-e81d-4dbb-ae36-289c05395061').
narrative_ontology:cs_kernel_codification('16b0fba5-e81d-4dbb-ae36-289c05395061', formalized).
narrative_ontology:cs_authority_grounding('16b0fba5-e81d-4dbb-ae36-289c05395061', expertise).
narrative_ontology:cs_interpretation_layer_present('16b0fba5-e81d-4dbb-ae36-289c05395061').
narrative_ontology:cs_reading_relation('16b0fba5-e81d-4dbb-ae36-289c05395061', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('16b0fba5-e81d-4dbb-ae36-289c05395061', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('16b0fba5-e81d-4dbb-ae36-289c05395061', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('16b0fba5-e81d-4dbb-ae36-289c05395061', foundational, decoding_is_primary_skill).
narrative_ontology:cs_axiom_status(decoding_is_primary_skill, holdable).
narrative_ontology:cs_axiom_grounding('16b0fba5-e81d-4dbb-ae36-289c05395061', decoding_is_primary_skill, empirically_contingent).
narrative_ontology:cs_axiom('16b0fba5-e81d-4dbb-ae36-289c05395061', foundational, explicit_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('16b0fba5-e81d-4dbb-ae36-289c05395061', explicit_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('16b0fba5-e81d-4dbb-ae36-289c05395061', cognitive_science_evidence_based_pedagogy).
narrative_ontology:cs_drift_state('16b0fba5-e81d-4dbb-ae36-289c05395061', contemporary_pedagogical_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('16b0fba5-e81d-4dbb-ae36-289c05395061', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_developers_phonics_programs).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, educational_researchers_cognitive_science).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, students_who_prefer_holistic_learning).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, science_of_reading_principles).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, simple_view_of_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teachers are often required to follow scripted phonics curricula, limiting their professional autonomy and judgment in adapting instruction to individual student needs or alternative pedagogical approaches. Exit means leaving the school or profession.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teachers_professional_judgment, payer,
    moderate, biographical, constrained, local).

% These students benefit significantly from explicit, systematic phonics instruction, which provides them with foundational decoding skills they might not acquire through less structured methods. They are trapped within the educational system.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, immediate, trapped, local).

% These entities develop and market phonics-based curricula and professional development programs, directly benefiting from the widespread adoption and enforcement of phonics-first mandates. They actively shape policy and resource allocation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_developers_phonics_programs, agenda_setter,
    organized, generational, arbitrage, national).

% Researchers in cognitive science and educational psychology provide the empirical evidence base for phonics-first approaches, influencing policy and pedagogical shifts. They benefit from the validation of their research paradigms.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, educational_researchers_cognitive_science, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, educational_researchers_cognitive_science, observer).

% Parents whose children struggle with reading often advocate for explicit phonics instruction, seeing it as a clear, actionable solution to their children's difficulties. Their options are limited by available school programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers, beneficiary,
    moderate, biographical, constrained, local).

% Administrators implement and enforce district- or state-level mandates for reading instruction, often balancing research recommendations with political pressures and teacher resistance. They are responsible for ensuring compliance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, school_administrators, agenda_setter,
    institutional, biographical, constrained, local).

% Some students may find highly structured, decontextualized phonics instruction disengaging or less effective for their learning style, potentially impacting their motivation and broader comprehension development. They are trapped within the prescribed method.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_who_prefer_holistic_learning, payer,
    powerless, immediate, trapped, local).

% Advocates for whole language or less explicit approaches are often marginalized in policy debates and curriculum adoption processes, despite their arguments for meaning-making and intrinsic motivation in reading. Their pedagogical philosophy is actively suppressed.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_developers_phonics_programs).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure all students acquire foundational decoding skills by providing a systematic, explicit instructional pathway, thereby reducing variability in early reading outcomes.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design from individual teachers to centralized, research-aligned programs, and transfers resources to curriculum developers. It also transfers cognitive load from implicit learning to explicit instruction for students.
% ABSENT_VOICES: Advocates for whole language or purely balanced literacy approaches are largely excluded from policy-making and curriculum adoption, as their core tenets are seen as antithetical to the phonics-first mandate. Students who thrive on holistic, meaning-based learning may also be unheard.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished overnight, educational systems would likely revert to more varied, less systematic approaches, potentially leading to a resurgence of 'reading wars' debates and inconsistent outcomes for students, particularly those with phonological challenges. Curriculum markets would also shift dramatically.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among students from disadvantaged backgrounds or those with dyslexia, due to inconsistent or inadequate foundational decoding instruction.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers, cognitive scientists, and parents of struggling readers widely attest that the problem of reading failure remains live, and that systematic phonics instruction is a critical component of the solution. This is corroborated by longitudinal studies and meta-analyses from outside the direct beneficiaries of phonics curriculum sales.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) due to the significant imposition on teacher autonomy and the market created for specific phonics curricula. Suppression is also high (0.75) because this approach actively marginalizes and often prohibits alternative reading instruction methods in many educational settings. Theater ratio is low (0.15) as the instruction is genuinely intended to achieve its stated goal of improving decoding skills, with little performative maintenance. Accessibility collapse is moderate (0.60) as while phonics is dominant, other approaches still exist in discourse, if not practice. Resistance is high (0.70) reflecting the ongoing 'reading wars' and pedagogical debates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students with weak phonological awareness and their parents, this constraint is a clear beneficiary, providing a necessary pathway to literacy. From the perspective of teachers, particularly those who value pedagogical flexibility, it is an extractive force that diminishes their professional judgment. Curriculum developers and cognitive science researchers largely see it as a vindication of evidence-based practice and a coordination mechanism. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness and their parents are primary beneficiaries (low d) as the constraint directly addresses their needs. Curriculum developers and educational researchers are also beneficiaries/agenda-setters (low d) as they profit from or validate the approach. Teachers' professional judgment and students who prefer holistic learning are victims/payers (high d) as their autonomy or preferred learning styles are suppressed. Whole language advocates are excluded, their alternatives actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    teacher_autonomy_vs_student_outcomes,
    'Is the observed extraction from teacher autonomy a necessary cost for improving student decoding outcomes, or could similar outcomes be achieved with greater teacher flexibility?',
    'Comparative studies of student outcomes in systems with high vs. low teacher autonomy in phonics instruction, controlling for curriculum quality and teacher training.',
    'If similar outcomes are achievable with more autonomy, the extraction is unnecessary and the constraint leans more towards a Snare. If not, the extraction is a necessary cost of coordination, supporting the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_student_outcomes, empirical, 'Whether teacher autonomy is a zero-sum trade-off with student decoding outcomes.').

omega_variable(
    long_term_comprehension_impact,
    'Does early, intensive phonics instruction, by potentially de-emphasizing meaning-making, negatively impact long-term reading comprehension and motivation for some students?',
    'Longitudinal studies tracking students from phonics-first vs. alternative programs into later grades, assessing comprehension, reading fluency, and reading enjoyment.',
    'If negative long-term impacts are significant, the constraint''s overall benefit to students is reduced, potentially increasing its effective extractiveness for a broader group of students.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_comprehension_impact, empirical, 'Potential trade-off between early decoding focus and later comprehension/motivation.').

omega_variable(
    pedagogical_framing_ambiguity,
    'Is the ''decoding precedes comprehension'' claim a descriptive statement about cognitive processes or a prescriptive pedagogical mandate?',
    'Analysis of how the claim is used in policy documents and teacher training materials: if primarily used to justify specific instructional sequences, it''s prescriptive. If used to explain observed cognitive phenomena, it''s descriptive.',
    'If primarily prescriptive, the constraint''s suppression of alternative pedagogical sequences is more direct and intentional. If descriptive, the constraint might be misapplied as a rigid mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_framing_ambiguity, conceptual, 'Ambiguity in the ''decoding precedes comprehension'' claim''s function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__phonics_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(lite_tr_t2025, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(lite_be_t2025, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(lite_su_t2025, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
