% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'phonics decoding primacy' reading of
 *   reading acquisition legitimacy, asserting that reading is fundamentally
 *   decoding and legitimate instruction explicitly teaches the alphabetic
 *   principle through systematic phonics. This reading emphasizes high
 *   structure, explicit sequencing, decodable texts, and the teacher as a
 *   direct instructor, with early identification of struggling readers via
 *   decoding assessments. It is one of several competing pedagogical
 *   approaches within the broader 'reading_acquisition_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.45).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, 'a43cde9e-881d-4acb-aa70-20d29bfeef05').
narrative_ontology:cs_kernel_codification('a43cde9e-881d-4acb-aa70-20d29bfeef05', formalized).
narrative_ontology:cs_authority_grounding('a43cde9e-881d-4acb-aa70-20d29bfeef05', expertise).
narrative_ontology:cs_interpretation_layer_present('a43cde9e-881d-4acb-aa70-20d29bfeef05').
narrative_ontology:cs_reading_relation('a43cde9e-881d-4acb-aa70-20d29bfeef05', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('a43cde9e-881d-4acb-aa70-20d29bfeef05', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('a43cde9e-881d-4acb-aa70-20d29bfeef05', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('a43cde9e-881d-4acb-aa70-20d29bfeef05', foundational, alphabetic_principle_is_foundational).
narrative_ontology:cs_axiom_status(alphabetic_principle_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('a43cde9e-881d-4acb-aa70-20d29bfeef05', alphabetic_principle_is_foundational, empirically_contingent).
narrative_ontology:cs_axiom('a43cde9e-881d-4acb-aa70-20d29bfeef05', foundational, explicit_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('a43cde9e-881d-4acb-aa70-20d29bfeef05', explicit_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('a43cde9e-881d-4acb-aa70-20d29bfeef05', science_of_reading_consensus).
narrative_ontology:cs_drift_state('a43cde9e-881d-4acb-aa70-20d29bfeef05', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a43cde9e-881d-4acb-aa70-20d29bfeef05', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_diverse_learning_styles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from increased demand for systematic phonics materials, professional development, and assessment tools. Their market share and influence grow as this pedagogical approach gains policy traction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Their research on the alphabetic principle and decoding mechanisms is directly validated and applied in policy. They gain funding, prestige, and influence over educational standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_science_researchers, beneficiary,
    institutional, generational, analytical, global).

% Are pressured to abandon established pedagogical practices focused on meaning-making and authentic literature. They face retraining requirements, curriculum mandates, and professional devaluation if they resist.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_teachers, payer,
    moderate, biographical, constrained, local).

% Are subjected to a uniform instructional approach that may not align with their individual learning needs, potentially leading to disengagement or misdiagnosis if their difficulties are not purely decoding-related.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, students_with_diverse_learning_styles, payer,
    powerless, immediate, trapped, local).

% Implement and enforce curriculum standards that prioritize systematic phonics. They respond to research, public pressure, and lobbying from various interest groups, seeking to improve literacy rates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, education_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from clear, explicit instruction that often yields measurable gains for children struggling with basic decoding. They are often strong advocates for this approach, seeking concrete solutions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, research-backed framework for early reading instruction, ensuring consistency across classrooms and providing clear guidance for teachers and curriculum developers.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources towards explicit, systematic phonics instruction, away from approaches emphasizing incidental learning or whole-word recognition. It also transfers professional development requirements and curriculum costs to schools and teachers.
% ABSENT_VOICES: Advocates for holistic child development, critical pedagogy, and culturally responsive teaching methods, who would argue for broader definitions of literacy and more flexible instructional approaches, are often marginalized in policy debates focused on narrow skill acquisition.
% DISAPPEARANCE_RATIONALE: If the primacy of phonics decoding vanished overnight, educational policy would immediately revert to a more diverse, and likely contested, set of instructional approaches. Curriculum mandates would dissolve, professional development would diversify, and the market for reading materials would shift dramatically, reorganizing around different pedagogical philosophies.
% FOUNDING_PROBLEM: Persistent low literacy rates, particularly among disadvantaged students, and a perceived lack of scientific rigor in reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and many parents attest that the problem of low literacy and ineffective instruction remains live. Some educators and advocates for broader literacy definitions contest that the problem is misdiagnosed, arguing that a narrow focus on decoding misses deeper issues of comprehension and engagement.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs imposed on teachers and students by mandated curricula and professional development, as well as the opportunity cost of foregone alternative approaches. Suppression (0.65) is significant due to policy mandates, funding incentives, and the marginalization of dissenting pedagogical views. Theater ratio (0.15) is relatively low, as the approach is actively implemented and enforced, though some performative compliance may exist. The claimed type is 'tangled_rope' because it offers a genuine coordination function (a clear instructional path) but also involves asymmetric extraction from those who must conform.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of phonics advocates and cognitive scientists, this constraint is a necessary 'rope' for effective literacy instruction, solving a critical coordination problem. From the perspective of whole language teachers and advocates for broader literacy, it is a 'snare' that extracts professional autonomy and limits pedagogical diversity, driven by a narrow definition of reading. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries include curriculum publishers and cognitive science researchers whose work is validated and commercialized. Victims are teachers whose professional autonomy is curtailed and students whose diverse learning needs may be overlooked by a one-size-fits-all approach. Education policymakers act as agenda-setters, mediating between various pressures. Parents of struggling readers often benefit from the clarity and perceived effectiveness of this approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_efficacy_for_all_learners,
    'Does systematic phonics instruction genuinely serve all student populations equally effectively, or does its universal application create hidden costs for some learners?',
    'Longitudinal studies comparing diverse student outcomes (including engagement, comprehension, and reading enjoyment, not just decoding scores) across different instructional approaches in varied contexts.',
    'If universal efficacy is disproven, the constraint''s extractiveness and suppression for certain student groups would be re-evaluated as higher, potentially shifting its classification towards a snare for those populations. If universal efficacy is confirmed, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_efficacy_for_all_learners, empirical, 'The empirical question of whether a single pedagogical approach is optimal for all learners.').

omega_variable(
    definition_of_reading_ambiguity,
    'Is ''reading'' fundamentally decoding, or is it a broader cognitive process encompassing meaning-making, critical thinking, and cultural engagement?',
    'Conceptual analysis and philosophical debate within the field of literacy studies, potentially informed by interdisciplinary research on language and cognition. This is a framing question, not purely empirical.',
    'If reading is defined more broadly, the ''phonics_decoding_primacy'' constraint would be seen as a partial or even misleading approach, increasing its perceived extractiveness by narrowing the scope of legitimate literacy. If the narrow definition holds, the constraint''s coordination function is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_reading_ambiguity, conceptual, 'The foundational conceptual disagreement over the definition of ''reading'' itself.').

omega_variable(
    teacher_autonomy_vs_standardization,
    'To what extent should pedagogical expertise and teacher autonomy be balanced against the benefits of standardized, evidence-based instructional mandates?',
    'Policy decisions reflecting societal values regarding professional discretion versus accountability, informed by research on both teacher morale/retention and student outcomes under different policy regimes.',
    'If teacher autonomy is prioritized, the suppression metric for teachers would decrease, potentially reclassifying the constraint as less extractive for that seat. If standardization is prioritized, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_vs_standardization, preference, 'The value-laden choice between professional autonomy and instructional standardization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
