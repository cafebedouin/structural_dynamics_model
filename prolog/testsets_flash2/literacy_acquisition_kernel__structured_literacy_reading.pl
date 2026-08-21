% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Acquisition (Orton-Gillingham Tradition)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'structured literacy' approach to reading
 *   instruction, rooted in the Orton-Gillingham tradition, which emphasizes
 *   explicit, systematic, and cumulative teaching of phonological awareness,
 *   phonics, fluency, vocabulary, and comprehension. While initially designed
 *   for students with dyslexia, its proponents advocate for universal
 *   application. This is one reading of the broader
 *   'literacy_acquisition_kernel', which is contested by other pedagogical
 *   approaches like phonics-only, whole language, and balanced literacy. The
 *   constraint is claimed as a 'tangled_rope' because it offers genuine
 *   coordination benefits (especially for students with learning
 *   disabilities) but involves significant, often uncompensated, extraction
 *   from general education teachers and school districts due to specialized
 *   training requirements and curriculum costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Acquisition (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '17c76d5d-85b7-40b3-aad0-353966bb8409').
narrative_ontology:cs_kernel_codification('17c76d5d-85b7-40b3-aad0-353966bb8409', formalized).
narrative_ontology:cs_authority_grounding('17c76d5d-85b7-40b3-aad0-353966bb8409', expertise).
narrative_ontology:cs_interpretation_layer_present('17c76d5d-85b7-40b3-aad0-353966bb8409').
narrative_ontology:cs_reading_relation('17c76d5d-85b7-40b3-aad0-353966bb8409', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('17c76d5d-85b7-40b3-aad0-353966bb8409', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('17c76d5d-85b7-40b3-aad0-353966bb8409', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('17c76d5d-85b7-40b3-aad0-353966bb8409', foundational, reading_is_a_learned_skill).
narrative_ontology:cs_axiom_status(reading_is_a_learned_skill, holdable).
narrative_ontology:cs_axiom_grounding('17c76d5d-85b7-40b3-aad0-353966bb8409', reading_is_a_learned_skill, empirically_contingent).
narrative_ontology:cs_axiom('17c76d5d-85b7-40b3-aad0-353966bb8409', foundational, explicit_systematic_instruction_is_optimal).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('17c76d5d-85b7-40b3-aad0-353966bb8409', explicit_systematic_instruction_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('17c76d5d-85b7-40b3-aad0-353966bb8409', science_of_reading_consensus).
narrative_ontology:cs_drift_state('17c76d5d-85b7-40b3-aad0-353966bb8409', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('17c76d5d-85b7-40b3-aad0-353966bb8409', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These students, particularly those with dyslexia, benefit significantly from the explicit, systematic, and cumulative nature of structured literacy, which addresses their specific learning needs and reduces their risk of reading failure. Their 'exit' from this approach often means continued struggle.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, identity_locked, local).

% Often trained in Orton-Gillingham or similar methodologies, they advocate for and implement structured literacy. They are beneficiaries of the clear framework and professional identity it provides, but constrained by resource limitations and broader school policies.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_teachers, agenda_setter,
    organized, biographical, constrained, regional).

% These companies and organizations develop and sell structured literacy programs, training, and materials. They benefit directly from the adoption of this pedagogical approach, with high demand for their specialized products and services.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_curriculum_providers, beneficiary,
    powerful, generational, arbitrage, national).

% Face increased training burdens and demands to implement complex, multi-component instructional methods, often without adequate time, resources, or ongoing support. They bear the cost of professional development and instructional shifts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Bear the financial and logistical costs of extensive teacher training, curriculum adoption, and ongoing professional development required for widespread implementation of structured literacy. They are often pressured by legislation or parent advocacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    institutional, generational, constrained, regional).

% While structured literacy is beneficial for many, some students may find the highly explicit and repetitive nature less engaging or efficient than other methods, potentially leading to disinterest or slower progress if not balanced with other approaches. They are trapped by the universal application.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_without_diagnosed_disabilities, payer,
    powerless, biographical, trapped, local).

% Academics and educators who champion whole language or balanced literacy approaches find their methodologies increasingly marginalized or legislated against in favor of structured literacy, despite their arguments for holistic reading development.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive, evidence-based framework for teaching reading that addresses multiple components (phonological awareness, phonics, fluency, vocabulary, comprehension) in a systematic and cumulative manner, ensuring all foundational skills are covered.
% TRANSFER_FUNCTION: Transfers pedagogical authority and instructional methods from generalist approaches to specialized, explicit, and systematic instruction, requiring significant investment in teacher training and curriculum materials, primarily from school districts to curriculum providers and trainers.
% ABSENT_VOICES: Advocates for whole language or purely balanced literacy approaches are increasingly excluded from policy discussions and curriculum adoption processes, despite their arguments about reading motivation and the role of meaningful text engagement.
% DISAPPEARANCE_RATIONALE: If the structured literacy approach vanished, particularly its emphasis on explicit phonics and phonological awareness, there would be a significant resurgence of less systematic methods, likely leading to increased reading failure rates, especially among students with dyslexia, and a re-fragmentation of pedagogical consensus.
% FOUNDING_PROBLEM: The persistent failure of many students, particularly those with dyslexia, to acquire reading proficiency through traditional or less explicit instructional methods, leading to a search for more effective, research-backed interventions.
% FOUNDING_PROBLEM_CORROBORATION: Educational psychologists, cognitive scientists, and parent advocacy groups consistently corroborate the ongoing problem of reading failure and the efficacy of structured literacy for at-risk learners. This is supported by decades of research outside the direct beneficiaries of curriculum sales.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the substantial financial and time investment required for teacher training and curriculum materials, which often falls on school districts and individual teachers. Suppression (0.75) is also high, as legislative mandates and strong advocacy suppress alternative pedagogical approaches and limit teacher autonomy. The theater ratio (0.15) is relatively low, indicating that the core instructional components are genuinely implemented, though some 'fidelity drift' may occur in practice. The rising extractiveness and suppression over time reflect the increasing institutionalization and legislative backing of structured literacy, moving from a specialized intervention to a mandated universal approach.
 *
 * PERSPECTIVAL GAP:
 *   Students with learning disabilities and special education teachers experience this as a beneficial, even life-changing, coordination mechanism. In contrast, general education teachers and school districts often experience it as a burdensome, top-down mandate with significant costs and limited flexibility. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with learning disabilities are clear beneficiaries (d=0.0-0.1) as the approach directly addresses their needs. Special education teachers and curriculum providers also benefit (d=0.1-0.2) from the clear framework and market for materials. General education teachers and school districts are payers (d=0.7-0.8) due to the high training and implementation costs. Students without diagnosed disabilities are also payers (d=0.6-0.7) as they are subjected to a potentially suboptimal universal approach. Whole language advocates are excluded (d=1.0) as their methods are actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_as_fourth_reading,
    'Is ''structured_literacy_reading'' a distinct fourth reading of the literacy acquisition kernel, or a more comprehensive variant of the ''phonics_reading''?',
    'Conceptual analysis of the core tenets and historical development of each approach, focusing on whether structured literacy introduces fundamentally new axioms beyond phonics, or merely elaborates on them.',
    'If a distinct reading, it strengthens the argument for a multi-faceted ''science of reading''. If a variant, it might be absorbed into the ''phonics_reading'' with a higher complexity offset, potentially altering its network relations and perceived distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_as_fourth_reading, conceptual, 'Ambiguity regarding the distinctness of structured literacy as a pedagogical paradigm.').

omega_variable(
    universal_applicability_efficacy,
    'Is the universal application of structured literacy equally effective for all students, or does it create suboptimal outcomes for some students without diagnosed learning disabilities?',
    'Large-scale, longitudinal comparative studies evaluating reading outcomes, engagement, and motivation across diverse student populations under universal structured literacy implementation versus differentiated instruction.',
    'If suboptimal for some, the extractiveness on ''students_without_diagnosed_disabilities'' would be re-evaluated upward, potentially shifting the overall classification towards a ''snare'' for that seat. If universally beneficial, it would reinforce the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_applicability_efficacy, empirical, 'Efficacy of universal structured literacy for all student populations.').

omega_variable(
    teacher_training_burden_vs_benefit,
    'Is the extensive training burden on general education teachers justified by a commensurate increase in their instructional efficacy and student outcomes, or is it an uncompensated cost?',
    'Cost-benefit analysis comparing teacher professional development costs and time investment against measurable improvements in teacher skill and student reading achievement in general education classrooms.',
    'If the burden is disproportionate to the benefit, the extractiveness on ''general_education_teachers'' and ''school_districts'' would be confirmed as high, strengthening the ''tangled_rope'' classification. If benefits outweigh costs, extractiveness would be re-evaluated downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_training_burden_vs_benefit, empirical, 'Balance of costs and benefits for general education teachers implementing structured literacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_certification_standards).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, special_education_funding_models).

% DUAL FORMULATION NOTE:
% This constraint is the 'structured_literacy_reading' of the 'literacy_acquisition_kernel'. It is linked to other readings of the same kernel, as well as to related policy and funding constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
