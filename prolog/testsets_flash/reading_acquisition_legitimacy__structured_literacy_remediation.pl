% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Mandate for Reading Acquisition
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint mandates that reading instruction prioritize the needs of
 *   the most vulnerable learners, implementing explicit, cumulative, and
 *   diagnostic principles derived from structured literacy. This approach is
 *   often presented as a necessary remediation for widespread literacy
 *   failures, particularly for students with dyslexia and other learning
 *   disabilities. It requires significant shifts in teacher training,
 *   curriculum adoption, and assessment practices, leading to substantial
 *   costs and resistance from those invested in alternative pedagogical
 *   approaches.
 *
 * KEY AGENTS:
 *   - structured_literacy_advocates: Agenda setter (institutional/arbitrage) — champions and enforces the mandate.
 *   - teachers_trained_in_other_methods: Payer (moderate/constrained) — bears the cost of retraining and curriculum change.
 *   - school_districts_with_legacy_curricula: Payer (organized/constrained) — bears the financial and logistical burden of systemic change.
 *   - students_with_dyslexia: Beneficiary (powerless/trapped) — directly benefits from targeted, effective instruction.
 *   - teacher_training_institutions: Beneficiary (institutional/mobile) — profits from increased demand for structured literacy certification.
 *   - parents_of_struggling_readers: Organized (powerful/constrained) — advocates for the mandate, but also bears indirect costs.
 *   - students_without_specific_learning_disabilities: Victim (powerless/trapped) — may experience instruction that is overly rigid or slow-paced, not optimally suited for their learning style, due to the 'vulnerable first' principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.7).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Mandate for Reading Acquisition").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, 'c504a001-b48d-41f9-9851-b24a3b74f8da').
narrative_ontology:cs_kernel_codification('c504a001-b48d-41f9-9851-b24a3b74f8da', formalized).
narrative_ontology:cs_authority_grounding('c504a001-b48d-41f9-9851-b24a3b74f8da', expertise).
narrative_ontology:cs_interpretation_layer_present('c504a001-b48d-41f9-9851-b24a3b74f8da').
narrative_ontology:cs_reading_relation('c504a001-b48d-41f9-9851-b24a3b74f8da', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('c504a001-b48d-41f9-9851-b24a3b74f8da', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('c504a001-b48d-41f9-9851-b24a3b74f8da', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('c504a001-b48d-41f9-9851-b24a3b74f8da', foundational, vulnerable_learners_first_design).
narrative_ontology:cs_axiom_status(vulnerable_learners_first_design, holdable).
narrative_ontology:cs_axiom_grounding('c504a001-b48d-41f9-9851-b24a3b74f8da', vulnerable_learners_first_design, empirically_contingent).
narrative_ontology:cs_axiom('c504a001-b48d-41f9-9851-b24a3b74f8da', foundational, explicit_cumulative_diagnostic_instruction).
narrative_ontology:cs_axiom_status(explicit_cumulative_diagnostic_instruction, holdable).
narrative_ontology:cs_axiom_grounding('c504a001-b48d-41f9-9851-b24a3b74f8da', explicit_cumulative_diagnostic_instruction, empirically_contingent).
narrative_ontology:cs_reference_frame('c504a001-b48d-41f9-9851-b24a3b74f8da', universal_structured_literacy_implementation).
narrative_ontology:cs_drift_state('c504a001-b48d-41f9-9851-b24a3b74f8da', contemporary_policy_adoption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c504a001-b48d-41f9-9851-b24a3b74f8da', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_training_institutions).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_other_methods).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_with_legacy_curricula).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, students_without_specific_learning_disabilities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates effective instruction for vulnerable learners (a clear benefit) but does so with significant asymmetric extraction. The extractiveness (0.65) stems from the high cost of retraining, curriculum overhaul, and the suppression (0.70) of alternative, potentially effective, methods. The 'vulnerable first' principle, while beneficial for some, can impose costs on others. Theater ratio (0.20) is low, as the mandate is actively implemented, though some performative compliance may exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of structured_literacy_advocates and parents_of_struggling_readers, this is a necessary Rope, solving a critical coordination problem for effective literacy. From the perspective of teachers_trained_in_other_methods and school_districts_with_legacy_curricula, it is a Snare, imposing significant costs and suppressing established practices without full consideration of alternatives or local contexts. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured literacy advocates and teacher training institutions are clear beneficiaries (d near 0.0) due to increased influence and revenue. Students with dyslexia are also beneficiaries, as the instruction is designed for them. Teachers trained in other methods and school districts are payers (d near 1.0) due to the costs of compliance and loss of autonomy. Students without specific learning disabilities are victims, as the instruction may not be optimal for them. Parents of struggling readers are complex: beneficiaries of improved outcomes but also payers of increased taxes or tuition for specialized programs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint addresses a persistent problem of reading failure, so mandatrophy is not resolved. The classification as Tangled Rope acknowledges the genuine coordination function (improving literacy for vulnerable learners) while highlighting the extractive elements (costs imposed on other stakeholders, suppression of alternatives). It prevents mislabeling as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, independent mandate for structured literacy, or primarily a reaction to the failures of other reading instruction methods?',
    'Longitudinal study of implementation in contexts without prior ''reading wars'' controversies; analysis of policy documents for proactive vs. reactive framing.',
    'If primarily reactive, its classification might shift towards a Scaffold (temporary support to fix a problem) rather than a Tangled Rope (ongoing coordination with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''structured_literacy_remediation'' reading of the ''reading_acquisition_legitimacy'' kernel. It emphasizes designing instruction for vulnerable learners first, using explicit, cumulative, diagnostic principles. Sibling readings include ''phonics_decoding_primacy'', ''whole_language_meaning_primacy'', and ''balanced_literacy_integration''.').

omega_variable(
    implementation_fidelity_vs_cost,
    'Is the high cost and training burden of implementing structured literacy a necessary coordination cost for effective remediation, or an extractive barrier to entry for teachers and districts?',
    'Cost-benefit analysis comparing student outcomes with implementation costs across diverse socioeconomic contexts; analysis of alternative, lower-cost structured literacy models.',
    'If costs are disproportionately high relative to outcomes, the ''extractiveness'' metric would be further amplified, potentially pushing the classification closer to a Snare for some stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_vs_cost, empirical, 'Assesses whether implementation costs are justified by outcomes or represent an extractive burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, resource_allocation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reading_acquisition_legitimacy' kernel. Its ε value differs significantly from other readings due to its specific emphasis on remediation and the associated costs and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
