% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration in Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   acquisition, which attempts to integrate explicit phonics instruction
 *   with immersion in authentic literature. It is a reading of the broader
 *   'reading_acquisition_legitimacy' kernel, which is contested by
 *   'phonics_decoding_primacy' and 'whole_language_meaning_primacy' readings.
 *   This reading emphasizes that reading requires both decoding and
 *   meaning-making, and legitimate instruction balances these components. The
 *   structural delta for this reading involves mixed instructional materials
 *   (both decodable and authentic texts), teachers toggling between direct
 *   instruction and facilitation, and struggling readers receiving both
 *   phonics intervention and guided reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.4).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.6).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'a58119ea-6491-40a0-b6c7-85b35e109877').
narrative_ontology:cs_kernel_codification('a58119ea-6491-40a0-b6c7-85b35e109877', formalized).
narrative_ontology:cs_authority_grounding('a58119ea-6491-40a0-b6c7-85b35e109877', lineage).
narrative_ontology:cs_interpretation_layer_present('a58119ea-6491-40a0-b6c7-85b35e109877').
narrative_ontology:cs_reading_relation('a58119ea-6491-40a0-b6c7-85b35e109877', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('a58119ea-6491-40a0-b6c7-85b35e109877', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('a58119ea-6491-40a0-b6c7-85b35e109877', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('a58119ea-6491-40a0-b6c7-85b35e109877', foundational, reading_is_both_decoding_and_meaning_making).
narrative_ontology:cs_axiom_status(reading_is_both_decoding_and_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('a58119ea-6491-40a0-b6c7-85b35e109877', reading_is_both_decoding_and_meaning_making, empirically_contingent).
narrative_ontology:cs_axiom('a58119ea-6491-40a0-b6c7-85b35e109877', foundational, instruction_must_balance_explicit_and_implicit_approaches).
narrative_ontology:cs_axiom_status(instruction_must_balance_explicit_and_implicit_approaches, holdable).
narrative_ontology:cs_axiom_grounding('a58119ea-6491-40a0-b6c7-85b35e109877', instruction_must_balance_explicit_and_implicit_approaches, conventional).
narrative_ontology:cs_reference_frame('a58119ea-6491-40a0-b6c7-85b35e109877', integrated_pedagogical_synthesis).
narrative_ontology:cs_drift_state('a58119ea-6491-40a0-b6c7-85b35e109877', contemporary_science_of_reading_movement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a58119ea-6491-40a0-b6c7-85b35e109877', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_institutions).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_lacking_specialized_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the demand for diverse instructional materials, including both decodable texts and authentic literature, which aligns with their broad product lines. They influence curriculum adoption processes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_publishers, beneficiary,
    organized, generational, mobile, national).

% Shape pedagogical approaches by training new teachers in balanced literacy methods, emphasizing a flexible approach to phonics and literature. They are invested in maintaining the legitimacy of their established curricula.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_training_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Profit from providing professional development and curriculum guidance to school districts implementing balanced literacy, often emphasizing the nuanced integration of various strategies.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants, beneficiary,
    powerful, biographical, mobile, regional).

% May not receive sufficiently explicit and systematic phonics instruction when teachers overemphasize meaning-making or rely on less effective strategies, leading to slower reading development and academic difficulties. They are dependent on the instructional methods provided.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Are expected to implement complex balanced literacy strategies, toggling between direct instruction and facilitation, without always receiving adequate, specialized training in the science of reading. This can lead to inconsistent application and burnout.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_lacking_specialized_training, payer,
    moderate, biographical, constrained, local).

% Often advocate for more explicit phonics instruction when their children struggle, but their voices may be marginalized in policy debates dominated by educational institutions and established pedagogical frameworks.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, excluded,
    organized, generational, constrained, local).

% Conduct research on reading acquisition and often critique balanced literacy for not sufficiently integrating findings from cognitive science regarding the importance of systematic phonics. They provide evidence but do not directly set policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, cognitive_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse pedagogical approaches and materials within a single framework, aiming to address both the decoding and comprehension aspects of reading, and providing a flexible model for teachers.
% TRANSFER_FUNCTION: Transfers pedagogical authority and curriculum design influence to institutions and consultants promoting balanced literacy, while transferring instructional burden and potential learning gaps to teachers and struggling readers.
% ABSENT_VOICES: Advocates for purely systematic phonics or purely whole language approaches are often excluded from the mainstream 'balanced' discourse, as are parents demanding more explicit, evidence-based phonics for their children.
% DISAPPEARANCE_RATIONALE: If balanced literacy vanished, the current educational landscape would fragment, with schools and districts likely adopting either more explicit phonics-based or more whole-language-based curricula, leading to significant shifts in teacher training, publishing, and student outcomes.
% FOUNDING_PROBLEM: The 'reading wars' of the 20th century created a polarized debate between phonics and whole language, leading to inconsistent and often ineffective reading instruction. Balanced literacy emerged as a compromise to integrate perceived strengths of both approaches.
% FOUNDING_PROBLEM_CORROBORATION: Educators, policymakers, and parents widely acknowledge the historical 'reading wars' and the need for comprehensive reading instruction. However, the effectiveness of 'balanced literacy' in solving the problem for all learners, especially struggling ones, is contested by cognitive scientists and advocates for structured literacy, who point to persistent literacy gaps.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate diverse pedagogical elements (phonics and whole language) but also exhibits asymmetric extraction. Beneficiaries include publishers and teacher training institutions that profit from the broad, flexible curriculum. Victims are struggling readers who may not receive the systematic instruction they need, and teachers who are expected to implement complex methods without sufficient specialized training. Extractiveness is moderate (0.4) as the approach is not purely exploitative but creates inefficiencies and gaps. Suppression is higher (0.6) due to the institutional inertia and professional consensus that can marginalize alternative, more evidence-based approaches. Theater ratio is low (0.2) as there is genuine pedagogical activity, but some aspects of 'balance' can become performative without deep understanding of the underlying science.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of teacher training institutions, balanced literacy is a robust, comprehensive approach that addresses the complexity of reading. From the perspective of struggling readers and their advocates, it can be an insufficient or even harmful approach that fails to provide necessary foundational skills, leading to persistent literacy challenges. The engine's classification as Tangled Rope captures this divergence between claimed coordination and actual asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher training institutions and mainstream publishers are beneficiaries (low d) as they shape and profit from the curriculum. Struggling readers and under-trained teachers are payers (high d) as they bear the costs of an approach that may not fully meet their needs. Cognitive scientists are observers, providing external critique. Parents of struggling readers are excluded, their advocacy often not fully integrated into policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balanced_literacy_vs_structured_literacy_efficacy,
    'Does the ''balanced literacy'' approach, as implemented, effectively teach all children to read, particularly those with dyslexia or other learning difficulties, compared to ''structured literacy'' approaches?',
    'Longitudinal studies comparing literacy outcomes of diverse student populations under balanced literacy vs. structured literacy implementations, controlling for teacher training and fidelity of implementation.',
    'If balanced literacy is shown to be less effective for struggling readers, it would strengthen arguments for its reclassification towards a Snare for those populations, or a Piton if its pedagogical function is largely performative. If equally effective, it would reinforce its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_literacy_vs_structured_literacy_efficacy, empirical, 'Empirical efficacy of balanced literacy for diverse learners.').

omega_variable(
    phonics_vs_whole_language_integration_ambiguity,
    'Is the ''balance'' in balanced literacy genuinely an integration of effective elements from both phonics and whole language, or is it a rhetorical compromise that dilutes the efficacy of systematic phonics?',
    'Analysis of curriculum materials and classroom observations to quantify the proportion and explicitness of systematic phonics instruction versus incidental phonics and whole language strategies. Expert consensus on whether the ''balance'' reflects cognitive science findings.',
    'If the balance is found to dilute systematic phonics, the constraint''s extractiveness and suppression would be re-evaluated upward, as it would be failing to provide a necessary component of reading instruction under the guise of comprehensiveness. This would push it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonics_vs_whole_language_integration_ambiguity, conceptual, 'The nature of ''balance'' in balanced literacy: genuine integration vs. rhetorical dilution.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''balanced_literacy_integration'' reading of the ''reading_acquisition_legitimacy'' kernel, or does it conflate elements of other readings?',
    'Expert review of the constraint''s description against the definitions of ''phonics_decoding_primacy'' and ''whole_language_meaning_primacy'' to ensure clear boundaries and no overlap in core claims.',
    'If conflated, the constraint would need to be decomposed into multiple, more precise readings, each with its own distinct ε and classification. This would clarify the specific mechanisms of coordination and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring precise identification of this specific reading within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, information_standard).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_professional_development_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_adoption_processes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel. Other readings include 'phonics_decoding_primacy' and 'whole_language_meaning_primacy', which represent alternative pedagogical approaches to reading instruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
