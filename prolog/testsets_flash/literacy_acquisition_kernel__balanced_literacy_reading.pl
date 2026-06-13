% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Instruction
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint represents the 'balanced literacy' approach to reading
 *   instruction, which attempts to synthesize systematic phonics with
 *   meaningful text engagement. It is one reading of the broader
 *   'literacy_acquisition_kernel' which is highly contested. This reading
 *   claims complementarity but is often criticized for being a rebranding of
 *   whole language with minimal phonics, leading to moderate extraction from
 *   struggling readers and teachers, while benefiting education schools and
 *   publishers who profit from the continuous churn of methods and materials.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.4).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '0f03304f-3a01-49e4-b018-659defa040ff').
narrative_ontology:cs_kernel_codification('0f03304f-3a01-49e4-b018-659defa040ff', formalized).
narrative_ontology:cs_authority_grounding('0f03304f-3a01-49e4-b018-659defa040ff', lineage).
narrative_ontology:cs_interpretation_layer_present('0f03304f-3a01-49e4-b018-659defa040ff').
narrative_ontology:cs_reading_relation('0f03304f-3a01-49e4-b018-659defa040ff', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f03304f-3a01-49e4-b018-659defa040ff', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f03304f-3a01-49e4-b018-659defa040ff', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('0f03304f-3a01-49e4-b018-659defa040ff', foundational, phonics_and_meaning_are_complementary).
narrative_ontology:cs_axiom_status(phonics_and_meaning_are_complementary, holdable).
narrative_ontology:cs_axiom_grounding('0f03304f-3a01-49e4-b018-659defa040ff', phonics_and_meaning_are_complementary, conventional).
narrative_ontology:cs_axiom('0f03304f-3a01-49e4-b018-659defa040ff', foundational, instructional_balance_is_key).
narrative_ontology:cs_axiom_status(instructional_balance_is_key, holdable).
narrative_ontology:cs_axiom_grounding('0f03304f-3a01-49e4-b018-659defa040ff', instructional_balance_is_key, conventional).
narrative_ontology:cs_reference_frame('0f03304f-3a01-49e4-b018-659defa040ff', post_reading_wars_synthesis).
narrative_ontology:cs_drift_state('0f03304f-3a01-49e4-b018-659defa040ff', contemporary_science_of_reading_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f03304f-3a01-49e4-b018-659defa040ff', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, educational_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) stems from the ambiguity of 'balance,' which often leads to insufficient explicit phonics for many learners, requiring additional interventions. Suppression (0.4) is present through institutional inertia and the difficulty for teachers to deviate from prescribed curricula. The high theater ratio (0.6) reflects that the 'balance' often serves more as a rhetorical compromise than a genuinely integrated, empirically optimal instructional method, with much activity focused on maintaining the appearance of synthesis rather than achieving consistent reading outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of education schools and publishers, balanced literacy is a necessary, evolving synthesis that coordinates diverse pedagogical needs. From the perspective of struggling readers and their advocates, it can be an extractive framework that fails to deliver foundational skills, masking instructional deficits under the guise of 'balance.'
 *
 * DIRECTIONALITY LOGIC:
 *   Education schools and educational publishers are beneficiaries, as they control the discourse and curriculum market. Classroom teachers and struggling readers are payers/victims, bearing the costs of implementation and potential instructional inadequacy. Cognitive scientists act as observers, providing critical analysis from outside the direct pedagogical system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebranding,
    'Is balanced literacy a genuine synthesis of effective instructional methods, or a rebranding of whole language with superficial phonics integration?',
    'Longitudinal studies comparing reading outcomes of balanced literacy programs with structured literacy programs, particularly for at-risk learners, focusing on the fidelity of phonics implementation.',
    'If a rebranding, its extractiveness and theater ratio would be reclassified higher, and its coordination function would be seen as a cover for maintaining a flawed pedagogical paradigm. If a genuine synthesis, its extractiveness would be lower, and its coordination function more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebranding, empirical, 'Assesses the true nature of balanced literacy''s instructional integration.').

omega_variable(
    victim_identity_ambiguity,
    'Are struggling readers victims of this approach, or are they simply not benefiting from a system designed for the ''average'' learner?',
    'Analysis of instructional time allocation: if explicit phonics instruction is consistently below empirically established thresholds for struggling readers, they are victims of systemic neglect within the ''balance.''',
    'If victims, the constraint''s effective extraction on them is higher, and the classification leans more towards Snare. If merely not benefiting, the extraction is lower, and the constraint is a less severe Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_ambiguity, conceptual, 'Clarifies the structural relationship of struggling readers to the ''balanced'' approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(lite_tr_t1998, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1998, 0.5).
narrative_ontology:measurement(lite_tr_t2006, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2006, 0.55).
narrative_ontology:measurement(lite_tr_t2014, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2014, 0.6).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(lite_be_t1998, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(lite_be_t2006, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2006, 0.52).
narrative_ontology:measurement(lite_be_t2014, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(lite_su_t1998, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(lite_su_t2006, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2006, 0.38).
narrative_ontology:measurement(lite_su_t2014, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, information_standard).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel,' which encompasses multiple, often conflicting, pedagogical approaches to reading instruction. This specific reading attempts to synthesize phonics and whole language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
