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
 *   human_readable: Structured Literacy Pedagogy (Orton-Gillingham Tradition)
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the pedagogical approach of Structured
 *   Literacy, rooted in the Orton-Gillingham tradition, which posits that
 *   reading is an acquired skill requiring explicit, systematic, cumulative
 *   instruction in phonological awareness, phonics, fluency, vocabulary, and
 *   comprehension. While initially designed for students with dyslexia, it is
 *   increasingly advocated for universal application. This constraint is one
 *   reading of the broader 'literacy_acquisition_kernel', which is contested
 *   by other pedagogical approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Pedagogy (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, 'ea3c0a01-73ee-4fff-be2a-d412de54c202').
narrative_ontology:cs_kernel_codification('ea3c0a01-73ee-4fff-be2a-d412de54c202', formalized).
narrative_ontology:cs_authority_grounding('ea3c0a01-73ee-4fff-be2a-d412de54c202', expertise).
narrative_ontology:cs_interpretation_layer_present('ea3c0a01-73ee-4fff-be2a-d412de54c202').
narrative_ontology:cs_reading_relation('ea3c0a01-73ee-4fff-be2a-d412de54c202', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('ea3c0a01-73ee-4fff-be2a-d412de54c202', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('ea3c0a01-73ee-4fff-be2a-d412de54c202', literacy_acquisition_kernel__balanced_literacy_reading, forecloses).
narrative_ontology:cs_axiom('ea3c0a01-73ee-4fff-be2a-d412de54c202', foundational, reading_is_an_acquired_skill).
narrative_ontology:cs_axiom_status(reading_is_an_acquired_skill, holdable).
narrative_ontology:cs_axiom_grounding('ea3c0a01-73ee-4fff-be2a-d412de54c202', reading_is_an_acquired_skill, empirically_contingent).
narrative_ontology:cs_axiom('ea3c0a01-73ee-4fff-be2a-d412de54c202', foundational, explicit_systematic_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ea3c0a01-73ee-4fff-be2a-d412de54c202', explicit_systematic_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('ea3c0a01-73ee-4fff-be2a-d412de54c202', orton_gillingham_principles).
narrative_ontology:cs_drift_state('ea3c0a01-73ee-4fff-be2a-d412de54c202', contemporary_reading_wars_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ea3c0a01-73ee-4fff-be2a-d412de54c202', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_professionals).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement structured literacy, often holding specialized certifications. They benefit from the validation of their expertise and the effectiveness of the methods for their students, but face resource constraints.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_professionals, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from explicit, systematic instruction that addresses their specific learning needs, leading to improved literacy outcomes. Their 'exit' from ineffective instruction is often dependent on the system adopting this approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, trapped, local).

% Advocate for structured literacy methods, often driven by frustration with prior ineffective instruction. They benefit from their children receiving effective support, but face the burden of advocating for systemic change.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Bear the primary burden of retraining and curriculum overhaul to implement structured literacy. They face increased workload and pressure to adopt new methods, often without adequate support or resources. Their exit options are limited by professional requirements.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Responsible for funding teacher training, purchasing new curricula, and managing the transition to structured literacy. They face significant financial and logistical costs, often under pressure from state mandates or parent advocacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    institutional, generational, constrained, local).

% Advocates for whole language or balanced literacy approaches, whose methods are increasingly de-legitimized by the rise of structured literacy. They are excluded from curriculum development and policy-making in areas adopting structured literacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, proponents_of_other_pedagogies, excluded,
    powerful, generational, constrained, national).

% Study the efficacy of various literacy pedagogies, including structured literacy. They provide evidence that informs policy and practice, but do not directly implement or enforce the constraint.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, educational_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a standardized, evidence-based, and effective approach to literacy instruction that addresses the needs of all students, particularly those with dyslexia, ensuring consistent pedagogical practice across classrooms and schools.
% TRANSFER_FUNCTION: Transfers pedagogical authority and resources towards specialized training and curriculum development aligned with structured literacy principles. It also transfers improved literacy outcomes to students, especially those who previously struggled.
% ABSENT_VOICES: Proponents of whole language and balanced literacy pedagogies are increasingly marginalized. They would argue that structured literacy is too narrow, stifles reading enjoyment, or that their methods are equally effective, but their influence on policy is diminishing.
% DISAPPEARANCE_RATIONALE: If structured literacy pedagogy and its associated mandates vanished, educational systems would likely revert to less consistent or less effective instructional methods, leading to a resurgence of reading difficulties, particularly for students with dyslexia. The current momentum towards evidence-based reading instruction would be lost, and the 'reading wars' would likely intensify with renewed vigor for other approaches.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among students with dyslexia, due to inconsistent, non-systematic, or ineffective instructional methods that did not align with cognitive science research on how reading is acquired.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by cognitive science research, longitudinal studies on reading outcomes, and advocacy groups for dyslexia. Independent educational researchers and parent organizations consistently highlight the ongoing need for effective literacy instruction, corroborating the problem's persistence from outside the direct beneficiaries of structured literacy.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.68) is high due to the significant investment required for teacher training and curriculum changes, which often falls disproportionately on general education teachers and school districts. Suppression (0.75) is also high, as the adoption of structured literacy often involves top-down mandates, de-legitimization of alternative methods, and professional pressure, limiting exit options for educators. Theater ratio is low (0.10) because the instruction, when implemented, is genuinely functional and aims for real outcomes, not mere performance. Accessibility collapse (0.60) reflects that while the concept is known, effective implementation requires specialized resources not universally available. Resistance (0.55) comes from teachers facing increased burden and from proponents of other pedagogies whose methods are being displaced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students with learning disabilities and their advocates, structured literacy is a vital, effective coordination mechanism that addresses a critical need. From the perspective of general education teachers and school districts, it represents a significant, often under-resourced, extractive burden imposed by shifting educational policy and professional norms. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Special education professionals and students with learning disabilities are primary beneficiaries, as the approach validates expertise and provides effective intervention. Parents of struggling readers also benefit from improved outcomes. General education teachers and school districts are victims, bearing the costs of retraining and curriculum adoption. Proponents of other pedagogies are excluded, as their methods are actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_vs_phonics_distinction,
    'Is ''structured_literacy_reading'' a distinct reading of the kernel, or merely a more comprehensive variant of ''phonics_reading''?',
    'Analysis of core pedagogical principles and implementation guidelines: if structured literacy consistently includes components (e.g., morphology, syntax) not central to typical phonics-only programs, it is distinct.',
    'If a variant, its relationship to other readings (e.g., ''balanced_literacy_reading'') might be less ''foreclosing'' and more ''influencing''; if distinct, its stronger claims for comprehensive instruction are more likely to foreclose alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_vs_phonics_distinction, conceptual, 'Ambiguity regarding the distinctness of structured literacy from phonics-only approaches.').

omega_variable(
    cost_benefit_for_general_education_teachers,
    'Does the long-term benefit of improved student outcomes for general education teachers (e.g., fewer struggling readers, less need for remediation) eventually outweigh the initial costs of retraining and curriculum overhaul?',
    'Longitudinal studies tracking teacher satisfaction, workload, and student outcomes in districts that have fully implemented structured literacy over 5-10 years.',
    'If long-term benefits outweigh costs, the ''payer'' role for general education teachers might shift towards ''symmetric'' or even ''beneficiary'' over time, reducing the constraint''s effective extraction from their seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_benefit_for_general_education_teachers, empirical, 'Whether the costs to general education teachers are offset by long-term benefits.').

omega_variable(
    suppression_mechanism_ambiguity_teachers,
    'Is the measured suppression on general education teachers primarily structural (mandates, lack of resources for alternatives) or internalized (professional identity tied to new methods, fear of being seen as ''behind'')?',
    'Post-mandate-removal surveys and observations: if teachers continue to adhere to structured literacy even when not mandated, it suggests a degree of internalized suppression or genuine belief in efficacy.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as teachers carry the suppression with them. If purely structural, removing mandates would lead to rapid reversion to prior practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity_teachers, empirical, 'Structural vs. internalized suppression mechanism for general education teachers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lite_tr_t2025, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(lite_be_t2025, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(lite_su_t2025, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_certification_standards).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, curriculum_development_processes).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on structured literacy. It is linked to other readings of the same kernel, as well as to downstream educational policy constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
