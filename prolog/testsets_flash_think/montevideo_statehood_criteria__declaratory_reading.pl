% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria (Declaratory Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'declaratory reading' of the Montevideo
 *   Statehood Criteria, which posits that statehood is an objective legal
 *   fact established by meeting four criteria (permanent population, defined
 *   territory, government, capacity to enter into relations with other
 *   states), independent of recognition by other states. Recognition is
 *   merely an acknowledgment of this pre-existing legal fact. The constraint
 *   itself, as a set of objective criteria, is claimed as a Mountain,
 *   reflecting its status as a fundamental legal principle. The low
 *   extractiveness and suppression reflect the intrinsic nature of the
 *   criteria, while any extraction or suppression arises from political acts
 *   that *deny* the legal fact of statehood, rather than from the criteria
 *   themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.15).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.1).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, mountain).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'a87ea7ac-6226-4a50-994a-84fe3a88bff5').
narrative_ontology:cs_kernel_codification('a87ea7ac-6226-4a50-994a-84fe3a88bff5', formalized).
narrative_ontology:cs_authority_grounding('a87ea7ac-6226-4a50-994a-84fe3a88bff5', lineage).
narrative_ontology:cs_interpretation_layer_present('a87ea7ac-6226-4a50-994a-84fe3a88bff5').
narrative_ontology:cs_reading_relation('a87ea7ac-6226-4a50-994a-84fe3a88bff5', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('a87ea7ac-6226-4a50-994a-84fe3a88bff5', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a87ea7ac-6226-4a50-994a-84fe3a88bff5', foundational, statehood_is_objective_fact).
narrative_ontology:cs_axiom_status(statehood_is_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('a87ea7ac-6226-4a50-994a-84fe3a88bff5', statehood_is_objective_fact, deontological).
narrative_ontology:cs_axiom('a87ea7ac-6226-4a50-994a-84fe3a88bff5', foundational, recognition_is_declaratory).
narrative_ontology:cs_axiom_status(recognition_is_declaratory, holdable).
narrative_ontology:cs_axiom_grounding('a87ea7ac-6226-4a50-994a-84fe3a88bff5', recognition_is_declaratory, conventional).
narrative_ontology:cs_reference_frame('a87ea7ac-6226-4a50-994a-84fe3a88bff5', montevideo_convention_1933).
narrative_ontology:cs_drift_state('a87ea7ac-6226-4a50-994a-84fe3a88bff5', contemporary_international_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a87ea7ac-6226-4a50-994a-84fe3a88bff5', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, newly_emerging_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_law_scholars).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, states_denying_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that meet the four objective criteria (population, territory, government, capacity for relations) are immediately considered states under this reading, gaining legal personality and rights, regardless of external recognition. Their challenge is to assert this legal fact against political opposition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, newly_emerging_states, beneficiary,
    powerless, biographical, constrained, regional).

% As members of the international community, they are bound by the legal fact of statehood once criteria are met. While they may politically delay recognition, they cannot legally deny statehood itself. They bear the cost of adapting to new states without prior political leverage.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the clarity and objectivity this reading brings to international law, providing a stable framework for analysis and adjudication of statehood claims. They advocate for the consistent application of the criteria.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholars, beneficiary,
    analytical, generational, analytical, global).

% States from which new entities may emerge (e.g., former colonial powers, states facing secession). This reading reduces their leverage to condition or deny statehood based on political considerations, as statehood is an objective fact once criteria are met. They bear the cost of losing control over their former territories' legal status.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    powerful, biographical, constrained, national).

% States that, for political or strategic reasons, refuse to recognize entities that clearly meet the Montevideo criteria. Under this reading, their denial is merely a political act, not a legal one, and their position is weakened in international legal discourse. They bear the cost of their actions being legally challenged.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, states_denying_recognition, payer,
    institutional, biographical, constrained, global).

% Entities that effectively control territory and population, and have a government, but lack widespread international recognition. This reading provides them with a strong legal basis for claiming statehood, even if they remain politically isolated. They are beneficiaries of the criteria but can be trapped by political non-recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a set of objective, universally applicable criteria for determining when an entity qualifies as a state, thereby coordinating international legal understanding and facilitating stable relations.
% TRANSFER_FUNCTION: Transfers the power to confer statehood from the subjective political act of recognition by existing states to the objective fulfillment of legal criteria by the aspiring entity.
% ABSENT_VOICES: Constitutive theorists would argue that statehood requires recognition by the existing community of states, and hybrid theorists would insist on normative legitimacy (e.g., democratic governance, human rights) as additional requirements. These voices are absent from the pure declaratory framing.
% DISAPPEARANCE_RATIONALE: If the Montevideo criteria, even in their declaratory interpretation, vanished, statehood would revert to a purely political and arbitrary process of recognition, leading to significant international instability, increased conflict over territorial claims, and a lack of legal certainty for emerging entities. The international system would have to reorganize around a new, likely more chaotic, basis for legal personality.
% FOUNDING_PROBLEM: To establish a clear, objective, and depoliticized basis for statehood in international law, preventing arbitrary denial of legal personality and promoting stability in a world of evolving political entities.
% FOUNDING_PROBLEM_CORROBORATION: The continued relevance of the Montevideo Convention in international legal discourse, its frequent citation by international courts and legal scholars, and the practice of many states in acknowledging de facto states that meet the criteria, all corroborate the ongoing need for objective statehood criteria. This is attested by international legal bodies and independent scholars, not just benefiting states.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, ExtMetricName, E),
    domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect that the criteria themselves are not designed to extract or coerce; they are a standard. The high accessibility collapse (0.88) indicates that while meeting the criteria is difficult, once met, statehood is legally established, leaving few alternatives to this legal fact. Resistance (0.12) is low against the criteria themselves, though high against their *application* when it conflicts with political interests. The claimed type is Mountain because this reading asserts statehood as an objective legal fact, a structural feature of international law, rather than a human-constructed arrangement that requires active maintenance to persist. The slight increase in extractiveness over time reflects the growing tension between the declaratory principle and the persistent political practice of conditional recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of newly emerging states, this constraint is a foundational principle that grants them legal standing. From the perspective of states denying recognition, it is a challenge to their political power and discretion. The engine will compute these divergent experiences based on the declared roles and structural positions, even though the constraint itself is a Mountain of legal principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Newly emerging states and de facto authorities are beneficiaries because the criteria provide them with a legal basis for statehood, reducing their dependence on political recognition. International law scholars also benefit from the clarity. Parent states and states denying recognition are 'payers' in this context because the declaratory reading undermines their political leverage to control or condition statehood, forcing them to acknowledge legal facts that may be politically inconvenient.
 *
 * MANDATROPHY ANALYSIS:
 *   The declaratory reading of the Montevideo criteria is intended to prevent the mandatrophy of statehood itself, by ensuring that the legal fact of statehood does not become a 'dead problem' where the criteria are met but the entity remains in limbo due to political inertia or extraction. By asserting statehood as an objective fact, it aims to keep the 'founding problem' of clear state definition 'live' and prevent the constraint from becoming a Piton of theatrical non-recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_vs_constitutive_ambiguity,
    'Is statehood truly an objective legal fact established by meeting the Montevideo criteria (declaratory), or does it require political recognition by the existing community of states (constitutive)?',
    'Analysis of state practice and international judicial decisions in cases where entities meet criteria but lack recognition. If legal rights and duties are consistently applied to such entities, it supports the declaratory view.',
    'If resolved as purely constitutive, the constraint''s extractiveness would be higher (as recognition becomes a tool for political leverage), and its claimed type would shift towards a Tangled Rope or Snare. If resolved as purely declaratory, the Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(declaratory_vs_constitutive_ambiguity, conceptual, 'Ambiguity between the declaratory and constitutive theories of statehood.').

omega_variable(
    sufficiency_of_criteria_ambiguity,
    'Are the four objective Montevideo criteria sufficient for statehood, or do normative elements (e.g., democratic governance, human rights, non-aggression) also apply (hybrid reading)?',
    'Examination of state practice regarding recognition of entities with questionable normative credentials, and the evolving jurisprudence of international bodies on conditional recognition. If normative conditions are consistently applied as prerequisites, it supports the hybrid view.',
    'If resolved as hybrid, the constraint''s accessibility_collapse would be higher (more criteria to meet), and its claimed type might shift towards a Rope or Tangled Rope, reflecting a more complex coordination problem with potential for extraction through normative conditionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_criteria_ambiguity, empirical, 'Whether statehood criteria include normative elements beyond the objective Montevideo criteria.').

omega_variable(
    impact_of_recognition_denial,
    'To what extent does political denial of recognition, despite meeting declaratory criteria, functionally suppress the legal personality and operational capacity of a newly emerging state?',
    'Empirical study of the economic, diplomatic, and security consequences for entities that meet Montevideo criteria but are denied recognition by key states. If functional suppression is severe, the effective suppression of the constraint is higher than its base measure.',
    'If functional suppression is high, the effective extractiveness for newly emerging states is significantly amplified, potentially shifting their per-seat classification towards a Snare, even if the underlying criteria remain a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_recognition_denial, empirical, 'The practical effect of political non-recognition on entities that meet declaratory statehood criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(mont_tr_t1953, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1953, 0.05).
narrative_ontology:measurement(mont_tr_t1973, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1973, 0.05).
narrative_ontology:measurement(mont_tr_t1993, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(mont_tr_t2013, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(mont_tr_t2023, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2023, 0.05).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(mont_be_t1953, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1953, 0.11).
narrative_ontology:measurement(mont_be_t1973, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1973, 0.12).
narrative_ontology:measurement(mont_be_t1993, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1993, 0.13).
narrative_ontology:measurement(mont_be_t2013, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2013, 0.14).
narrative_ontology:measurement(mont_be_t2023, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2023, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.08).
narrative_ontology:measurement(mont_su_t1953, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1953, 0.08).
narrative_ontology:measurement(mont_su_t1973, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1973, 0.09).
narrative_ontology:measurement(mont_su_t1993, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1993, 0.09).
narrative_ontology:measurement(mont_su_t2013, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2013, 0.1).
narrative_ontology:measurement(mont_su_t2023, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2023, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Montevideo Statehood Criteria' kernel. It focuses on the declaratory theory, where statehood is an objective legal fact. Sibling constraints (constitutive_reading, hybrid_reading) offer alternative interpretations of the same kernel, with different implications for extraction and state agency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
