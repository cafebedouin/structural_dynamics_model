% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness Persistence: Competence Reading
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'competence reading' of preparedness
 *   persistence, where drills and inspections are understood as genuinely
 *   maintaining operational readiness. It posits that the physical and
 *   cognitive infrastructure for effective disaster response is a 'Mountain'
 *   (unchangeable physical/logical limits) and the coordination among actors
 *   is a 'Rope' (pure coordination). There is no significant extraction,
 *   suppression, or theatricality in this reading; the costs are inherent to
 *   maintaining readiness. This reading is one instantiation of the
 *   'preparedness_persistence' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.1).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, mountain).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Persistence: Competence Reading").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:emerges_naturally(preparedness_persistence__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '56a3ffbd-6b9f-4c7e-97a4-371339f99ab9').
narrative_ontology:cs_kernel_codification('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', implicit).
narrative_ontology:cs_authority_grounding('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', expertise).
narrative_ontology:cs_interpretation_layer_present('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9').
narrative_ontology:cs_reading_relation('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', foundational, operational_competence_requires_live_exercise).
narrative_ontology:cs_axiom_status(operational_competence_requires_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', operational_competence_requires_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', foundational, preparedness_is_a_collective_good).
narrative_ontology:cs_axiom_status(preparedness_is_a_collective_good, holdable).
narrative_ontology:cs_axiom_grounding('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', preparedness_is_a_collective_good, deontological).
narrative_ontology:cs_reference_frame('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', continuous_operational_readiness).
narrative_ontology:cs_drift_state('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('56a3ffbd-6b9f-4c7e-97a4-371339f99ab9', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, public_safety_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, affected_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, operational_readiness_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, institutional_learning_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies design, implement, and evaluate drills and inspections. They are responsible for maintaining operational readiness and see these activities as essential for competence. They benefit from the enhanced capability and public trust that genuine preparedness brings.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, public_safety_agencies, agenda_setter,
    institutional, generational, constrained, national).

% These are the citizens and communities who rely on effective disaster response. They are the ultimate beneficiaries of genuine operational readiness, as their safety and well-being depend on it. They have no direct control over the drills but bear the consequences of their failure.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, affected_populations, beneficiary,
    powerless, immediate, trapped, local).

% These individuals participate directly in drills and inspections, investing time and effort to maintain their skills and coordination. While they benefit from improved competence, the training itself is a cost in terms of time and resources. Their exit options are limited by professional obligations.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_responders, payer,
    moderate, biographical, constrained, local).

% Independent bodies tasked with assessing the effectiveness and compliance of preparedness activities. They provide an external check on the competence claims and can identify gaps or failures in the system. Their role is to ensure accountability and genuine readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, auditing_bodies, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that diverse public safety agencies, emergency responders, and critical infrastructure operators can coordinate effectively and execute complex response plans under stress, by regularly exercising their knowledge and procedures.
% TRANSFER_FUNCTION: Transfers knowledge, skills, and coordinated action capabilities from training and inspection activities into the operational readiness of emergency response systems, ultimately benefiting affected populations by reducing disaster impact.
% ABSENT_VOICES: Those who would argue for a purely theoretical or 'paper' preparedness, without the cost and disruption of live drills, are absent from the conversation among those committed to genuine readiness. Their perspective is dismissed as naive or dangerous by this reading.
% DISAPPEARANCE_RATIONALE: If the commitment to live exercised knowledge vanished, operational readiness would rapidly degrade. Agencies would lose coordination, responders would lose critical skills, and the ability to respond effectively to disasters would collapse, leading to catastrophic outcomes for affected populations.
% FOUNDING_PROBLEM: The problem of maintaining complex operational readiness in the face of personnel turnover, evolving threats, and the decay of unexercised knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Public safety experts, military strategists, and disaster recovery specialists universally corroborate that operational readiness is a 'use it or lose it' capability, requiring continuous practice. Historical disaster analyses consistently show that failures in preparedness lead to worse outcomes.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(preparedness_persistence__competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(preparedness_persistence__competence_reading),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.1) because the costs associated with drills and inspections are seen as necessary investments in a collective good, not as rents extracted by any party. Suppression is low (0.05) because participation is driven by a shared understanding of necessity and professional duty, rather than coercion. Theater ratio is low (0.05) as activities are genuinely functional, aimed at improving real-world performance. Accessibility collapse is high (0.9) because there are no viable alternatives to active practice for maintaining complex operational readiness. Resistance is low (0.1) because the value of genuine competence is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   In this competence reading, all stakeholders largely align on the functional necessity and benefit of drills. The costs are seen as investments. This contrasts sharply with other readings where the same activities might be perceived as extractive or purely performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Public safety agencies are agenda-setters and beneficiaries, gaining enhanced capability and public trust. Affected populations are primary beneficiaries, as their safety is directly improved. Emergency responders are payers, investing time and effort, but also beneficiaries of their own improved competence. Auditing bodies are observers, ensuring accountability. No party is a victim in this reading, as all costs are justified by the collective benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a ''competence_reading'' of preparedness persistence, or does it contain elements of ritual or extraction that would align it with the ''husk_reading'' or ''hybrid_reading''?',
    'Empirical audit of drill outcomes, post-incident analysis, and expert assessment of operational readiness, specifically looking for discrepancies between claimed competence and actual performance, or for evidence of resource diversion.',
    'If elements of ritual or extraction are found, the constraint would be reclassified towards a ''tangled_rope'' or ''snare'' (husk_reading) or a ''scaffold'' (hybrid_reading), indicating a divergence from genuine competence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Distinguishes this competence reading from other interpretations of preparedness persistence.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the necessity of ''exercised knowledge'' for operational readiness a natural law (Mountain), or a socially constructed norm (Rope)?',
    'Cross-cultural and historical analysis of complex systems maintenance: if all successful complex systems universally require active, live exercise to maintain readiness, it supports the natural law claim. If it varies significantly with cultural or institutional norms, it suggests a social construct.',
    'If a natural law, the constraint''s persistence is irreducible. If a social construct, it is subject to change and potential degradation if the underlying social commitment weakens, potentially shifting to a ''piton'' if the norm atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between natural law and social construct for the ''exercised knowledge'' principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__competence_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(prep_tr_t50, preparedness_persistence__competence_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__competence_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(prep_be_t50, preparedness_persistence__competence_reading, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__competence_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__competence_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__competence_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__competence_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(prep_su_t50, preparedness_persistence__competence_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
