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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Pedagogy (Orton-Gillingham Tradition)
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint describes the pedagogical requirement for explicit,
 *   systematic, cumulative instruction in phonological awareness, phonics,
 *   fluency, vocabulary, and comprehension, rooted in the Orton-Gillingham
 *   tradition. While initially designed for students with dyslexia, its
 *   principles are increasingly advocated for universal application. This is
 *   one reading of the broader 'literacy_acquisition_kernel', emphasizing a
 *   structured, evidence-based approach.
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
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Pedagogy (Orton-Gillingham Tradition)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '39d6b981-bcbc-4a01-bafa-6fef1da30c04').
narrative_ontology:cs_kernel_codification('39d6b981-bcbc-4a01-bafa-6fef1da30c04', formalized).
narrative_ontology:cs_authority_grounding('39d6b981-bcbc-4a01-bafa-6fef1da30c04', expertise).
narrative_ontology:cs_interpretation_layer_present('39d6b981-bcbc-4a01-bafa-6fef1da30c04').
narrative_ontology:cs_reading_relation('39d6b981-bcbc-4a01-bafa-6fef1da30c04', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('39d6b981-bcbc-4a01-bafa-6fef1da30c04', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('39d6b981-bcbc-4a01-bafa-6fef1da30c04', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('39d6b981-bcbc-4a01-bafa-6fef1da30c04', foundational, reading_is_acquired_not_natural).
narrative_ontology:cs_axiom_status(reading_is_acquired_not_natural, holdable).
narrative_ontology:cs_axiom_grounding('39d6b981-bcbc-4a01-bafa-6fef1da30c04', reading_is_acquired_not_natural, empirically_contingent).
narrative_ontology:cs_axiom('39d6b981-bcbc-4a01-bafa-6fef1da30c04', foundational, explicit_systematic_cumulative_instruction_is_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_cumulative_instruction_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('39d6b981-bcbc-4a01-bafa-6fef1da30c04', explicit_systematic_cumulative_instruction_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('39d6b981-bcbc-4a01-bafa-6fef1da30c04', orton_gillingham_principles).
narrative_ontology:cs_drift_state('39d6b981-bcbc-4a01-bafa-6fef1da30c04', contemporary_science_of_reading_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('39d6b981-bcbc-4a01-bafa-6fef1da30c04', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, literacy_specialists).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, teacher_training_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement structured literacy. Their expertise is highly valued, leading to increased demand for their services and training programs. They set standards and provide professional development.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, literacy_specialists, agenda_setter,
    institutional, generational, mobile, national).

% Directly benefit from the explicit, systematic, and cumulative instruction, which is often critical for their reading acquisition. Without it, they face significant academic and life challenges.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, trapped, local).

% Bear the burden of extensive, specialized training and curriculum changes, often without adequate time or resources. This can lead to burnout and resistance, despite recognizing the benefits for students.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Incur significant costs for teacher training, new curriculum materials, and ongoing professional development. They are often mandated to adopt these methods by state policy or parent advocacy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_districts, payer,
    organized, generational, constrained, regional).

% Benefit from their children receiving effective reading intervention. They are often strong advocates for structured literacy, pushing for its adoption in schools.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, local).

% Evaluate the efficacy of different pedagogical approaches and often mandate the adoption of structured literacy based on research evidence. They balance educational outcomes with implementation costs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, educational_policymakers, observer,
    institutional, generational, analytical, national).

% Experience increased demand for their structured literacy certification programs and courses, leading to revenue and influence growth within the education sector.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, teacher_training_institutions, beneficiary,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, literacy_specialists).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize and implement an evidence-based approach to reading instruction that effectively addresses the needs of all students, particularly those with dyslexia, ensuring consistent pedagogical quality across classrooms and schools.
% TRANSFER_FUNCTION: Transfers specialized pedagogical knowledge and skills from literacy experts and research to general education teachers, enabling them to deliver effective reading instruction. It also transfers financial resources from school districts to teacher training institutions and curriculum providers.
% ABSENT_VOICES: Proponents of 'whole language' or less structured, more 'natural' reading acquisition methods are largely absent from current policy discussions, having been marginalized by the 'Science of Reading' movement. They would argue against the perceived rigidity and decontextualization of structured literacy.
% DISAPPEARANCE_RATIONALE: If structured literacy vanished overnight, many students, especially those with learning disabilities, would revert to struggling with reading due to a lack of effective, systematic instruction. Educational systems would lose a proven framework, leading to a resurgence of reading failure and significant societal costs.
% FOUNDING_PROBLEM: High rates of reading failure, particularly among students with dyslexia, due to inadequate, unsystematic, and often unscientific reading instruction methods prevalent in schools.
% FOUNDING_PROBLEM_CORROBORATION: Decades of cognitive science research, neuroscientific studies on reading, and advocacy from parent groups (e.g., International Dyslexia Association) consistently corroborate the problem of reading failure and the efficacy of structured literacy approaches. This corroboration comes from outside the direct beneficiaries of the pedagogical system itself.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) is primarily felt by general education teachers and school districts due to the significant investment in specialized training, curriculum overhaul, and time required for implementation. Suppression (0.75) reflects the increasing pressure from policymakers and parent advocacy groups to adopt this methodology, limiting alternatives for schools and teachers. The theater ratio is low (0.10) because the instruction, when implemented, is genuinely functional and effective, not merely performative. Resistance (0.55) comes from teachers overwhelmed by the demands and from proponents of alternative, less structured methods.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of students and parents, this constraint is a vital 'rope' or 'scaffold' providing necessary support. From the perspective of general education teachers and school districts, it can feel like a 'snare' due to the high costs and mandates, despite acknowledging the benefits for students. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with learning disabilities and their parents are clear beneficiaries, experiencing improved literacy outcomes. Literacy specialists and teacher training institutions also benefit from increased demand for their expertise and programs. General education teachers and school districts are the primary payers, bearing the costs of training and implementation. Policymakers act as observers, balancing efficacy with feasibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of widespread reading failure, especially for dyslexic students, remains 'live'. The constraint's persistence is not due to mandatrophy but to ongoing evidence of its efficacy and continued advocacy. The tension arises from the high cost of implementation, not from an obsolete mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structured_literacy_vs_phonics_distinction,
    'Is ''structured_literacy_reading'' a distinct reading of the kernel, or is it a more comprehensive variant of ''phonics_reading''?',
    'Conceptual analysis of the scope and emphasis: if structured literacy consistently includes elements beyond phonics (e.g., fluency, vocabulary, comprehension strategies) in a systematic way that phonics-only approaches do not, it is distinct.',
    'If a variant, its relationship to other readings (especially ''whole_language_reading'') might be less ''foreclosing'' and more ''influential''. If distinct, its unique contribution to the kernel is clearer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_literacy_vs_phonics_distinction, conceptual, 'Clarifying the boundary between structured literacy and phonics-only approaches.').

omega_variable(
    universal_applicability_justification,
    'Is the claim of universal applicability for structured literacy fully supported by evidence, or is its primary efficacy still concentrated on students with specific learning disabilities?',
    'Large-scale, longitudinal empirical studies comparing structured literacy outcomes for neurotypical students versus other pedagogical approaches, controlling for teacher training quality.',
    'If efficacy is primarily for learning disabilities, the ''payer'' burden on general education teachers and school districts for universal implementation might be disproportionate to the universal benefit, shifting the classification towards a more extractive ''snare'' for general education.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_justification, empirical, 'Assessing the evidence for structured literacy''s universal benefit versus its targeted efficacy.').

omega_variable(
    teacher_training_burden_justification,
    'Is the significant training burden on general education teachers justified by the overall societal benefit of improved literacy outcomes, or does it represent an unsustainable extraction?',
    'Cost-benefit analysis comparing the long-term societal gains from improved literacy (e.g., economic productivity, reduced social services) against the direct and indirect costs of teacher training and implementation, including teacher retention rates.',
    'If the burden is deemed unsustainable or disproportionate, it strengthens the ''snare'' aspect for teachers, potentially leading to policy changes that subsidize training more heavily or streamline implementation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_training_burden_justification, preference, 'Evaluating the sustainability and fairness of the teacher training burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1930, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(lite_tr_t1960, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lite_be_t1930, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1930, 0.3).
narrative_ontology:measurement(lite_be_t1960, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1930, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1930, 0.2).
narrative_ontology:measurement(lite_su_t1960, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_certification_standards).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, curriculum_development_processes).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on structured, systematic instruction. It is linked to other readings of the same kernel, which represent alternative pedagogical approaches to reading acquisition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
