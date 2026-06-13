% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes preparedness as a system of live, exercised
 *   knowledge, where routines are actively maintained to ensure operational
 *   capacity across generations of personnel. It emphasizes adaptive
 *   capacity, realistic drills, and effective training to absorb generational
 *   turnover and prevent institutional memory loss. This is one reading of
 *   the 'preparedness_commitment' kernel, focusing on genuine competence
 *   rather than symbolic performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '6076dfef-bde9-473b-b208-a40a61adffae').
narrative_ontology:cs_kernel_codification('6076dfef-bde9-473b-b208-a40a61adffae', formalized).
narrative_ontology:cs_authority_grounding('6076dfef-bde9-473b-b208-a40a61adffae', expertise).
narrative_ontology:cs_interpretation_layer_present('6076dfef-bde9-473b-b208-a40a61adffae').
narrative_ontology:cs_reading_relation('6076dfef-bde9-473b-b208-a40a61adffae', preparedness_commitment__husk_reading, influences).
narrative_ontology:cs_reading_relation('6076dfef-bde9-473b-b208-a40a61adffae', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6076dfef-bde9-473b-b208-a40a61adffae', foundational, adaptive_capacity_is_paramount).
narrative_ontology:cs_axiom_status(adaptive_capacity_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6076dfef-bde9-473b-b208-a40a61adffae', adaptive_capacity_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('6076dfef-bde9-473b-b208-a40a61adffae', foundational, tacit_knowledge_requires_exercise).
narrative_ontology:cs_axiom_status(tacit_knowledge_requires_exercise, holdable).
narrative_ontology:cs_axiom_grounding('6076dfef-bde9-473b-b208-a40a61adffae', tacit_knowledge_requires_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('6076dfef-bde9-473b-b208-a40a61adffae', continuously_adaptive_organization).
narrative_ontology:cs_drift_state('6076dfef-bde9-473b-b208-a40a61adffae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6076dfef-bde9-473b-b208-a40a61adffae', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, affected_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, institutional_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, budget_allocators).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, adaptive_capacity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, organizational_learning_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from effective training and drills that maintain their operational capacity and safety. They are the primary implementers of preparedness routines and their lives depend on the routines being effective. Their exit options are constrained by professional commitment and the high cost of retraining.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    organized, biographical, constrained, local).

% Are the ultimate beneficiaries of a competent preparedness system, as their safety and recovery depend on it. They have little direct control over the system's design or maintenance, making them highly vulnerable to its failure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, affected_communities, beneficiary,
    powerless, generational, trapped, local).

% Responsible for allocating resources, designing training programs, and ensuring the long-term viability of preparedness routines. They benefit from the legitimacy and trust that a demonstrably competent system provides. Their exit options are constrained by political and professional accountability.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Design and implement the drills and educational programs that embody 'live exercised knowledge'. They are critical to maintaining the competence reading of preparedness. They benefit from the professional satisfaction of effective training and the resources allocated to their function.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, training_and_exercise_staff, agenda_setter,
    moderate, biographical, mobile, regional).

% Control the financial resources necessary for robust training, equipment, and personnel. They bear the direct costs of maintaining a high-competence preparedness system and often face pressure to reduce these costs in the absence of immediate threats.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budget_allocators, payer,
    institutional, immediate, mobile, national).

% Argue for preparedness based on past disasters, often emphasizing symbolic actions or adherence to outdated protocols rather than adaptive competence. Their voices are often sidelined in a system focused on live, exercised knowledge, as their approach can lead to 'husk' outcomes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, historical_precedent_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse agencies and personnel can effectively coordinate their actions during a crisis, by maintaining shared operational knowledge and decision-making routines through regular, realistic exercises.
% TRANSFER_FUNCTION: Transfers knowledge, skills, and adaptive capacity across generations of personnel, from experienced practitioners to new recruits, ensuring that institutional memory is embodied in live practice rather than static documents.
% ABSENT_VOICES: Those who advocate for 'preparedness as performance' (the husk reading) are often excluded from the design of competence-focused routines. They would argue for simpler, less resource-intensive drills that prioritize symbolic compliance over genuine operational readiness.
% DISAPPEARANCE_RATIONALE: If preparedness as live exercised knowledge vanished, institutional memory would rapidly degrade into static, unexercised plans. Operational capacity would collapse during the next crisis, leading to catastrophic failures in response and recovery, fundamentally altering the safety and resilience of communities.
% FOUNDING_PROBLEM: The historical problem of institutional amnesia and the 'next time will be different' fallacy, where lessons from past disasters are lost with personnel turnover, leading to repeated failures in crisis response.
% FOUNDING_PROBLEM_CORROBORATION: Academic research on organizational learning and disaster sociology consistently corroborates the ongoing challenge of maintaining institutional memory and adaptive capacity across generations, even in well-resourced organizations. Post-incident reviews frequently highlight failures stemming from unexercised or outdated knowledge, supporting the 'live' status of this problem.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination and public good provision, with minimal rent-seeking. Suppression is also low (0.20) as participation is largely driven by professional norms and the clear benefits of effective training, rather than coercion. Theater ratio is very low (0.05), indicating that nearly all activity is functional, with little performative maintenance. The slight increase in extractiveness and suppression early in the interval reflects the initial investment and institutionalization costs of building a robust system, which then stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of frontline responders and affected communities, this constraint is a pure Rope, providing essential coordination and safety. From the budget allocators' perspective, it's a necessary but costly Rope, requiring continuous investment. The institutional leadership sees it as a vital Rope that maintains their legitimacy and public trust.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline responders and affected communities are clear beneficiaries, gaining safety and operational effectiveness. Institutional leadership and training staff are agenda-setters and beneficiaries, as they design and implement the system and gain legitimacy from its success. Budget allocators are payers, bearing the direct financial costs. Historical precedent advocates are excluded, as their focus on symbolic adherence conflicts with the competence-driven approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_ambiguity,
    'Is the observed preparedness system genuinely maintaining live, exercised knowledge, or is it drifting towards a ''husk'' where routines are performed without operational competence?',
    'Independent, unannounced, high-fidelity drills with external evaluators, focusing on adaptive problem-solving rather than rote adherence to plans. Analysis of post-incident performance against pre-incident training metrics.',
    'If it''s a ''husk'', the effective extractiveness and theater ratio would be significantly higher, and the classification would shift towards a Piton or even a Snare, as resources are consumed for symbolic rather than functional outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_husk_ambiguity, empirical, 'Distinguishing genuine competence from performative compliance in preparedness routines.').

omega_variable(
    generational_knowledge_transfer_efficacy,
    'How effectively are adaptive capacities and tacit knowledge transferred across generational shifts in personnel, beyond explicit training curricula?',
    'Longitudinal studies tracking the performance of new cohorts in crisis simulations compared to experienced personnel, and qualitative analysis of mentorship and informal learning networks.',
    'If transfer is ineffective, the system''s long-term resilience is compromised, increasing the risk of catastrophic failure and raising the effective cost of maintaining the ''competence'' facade, potentially shifting it towards a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_efficacy, empirical, 'Measuring the success of tacit knowledge transfer in preparedness.').

omega_variable(
    resource_allocation_for_competence,
    'Is the current resource allocation sufficient to maintain the ''competence reading'' of preparedness, or is it underfunded relative to the actual demands of live exercised knowledge?',
    'Comparative analysis of resource inputs (funding, personnel, training hours) against empirically derived benchmarks for maintaining adaptive capacity in similar high-stakes domains.',
    'If underfunded, the system is structurally brittle and prone to degradation, increasing the likelihood of a shift towards a ''husk'' reading or a Piton classification due to insufficient maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_for_competence, empirical, 'Assessing resource adequacy for maintaining preparedness competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(prep_tr_t50, preparedness_commitment__competence_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(prep_be_t50, preparedness_commitment__competence_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(prep_su_t50, preparedness_commitment__competence_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'competence_reading' of the 'preparedness_commitment' kernel. It is structurally distinct from the 'husk_reading' (preparedness as memorial performance) and the 'hybrid_reading' (a layered system), which are modeled as separate constraints. The competence reading influences the others by setting a high bar for functional effectiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
