% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Definition of Combatant Status (Geneva Article 4)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint defines combatant status primarily through the lens of
 *   state organization, requiring formal state military affiliation and
 *   adherence to Article 4 of the Geneva Conventions for POW protections.
 *   Non-state armed groups are categorically excluded from these protections,
 *   making their members vulnerable to prosecution under domestic law. This
 *   story instantiates the 'state_centric_reading' of the broader
 *   'combatant_status_definition' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.85).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.9).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Definition of Combatant Status (Geneva Article 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa').
narrative_ontology:cs_kernel_codification('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', fixed_text).
narrative_ontology:cs_authority_grounding('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', lineage).
narrative_ontology:cs_interpretation_layer_present('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa').
narrative_ontology:cs_reading_relation('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', foundational, state_sovereignty_in_war).
narrative_ontology:cs_axiom_status(state_sovereignty_in_war, holdable).
narrative_ontology:cs_axiom_grounding('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', state_sovereignty_in_war, conventional).
narrative_ontology:cs_axiom('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', foundational, reciprocity_of_protections).
narrative_ontology:cs_axiom_status(reciprocity_of_protections, holdable).
narrative_ontology:cs_axiom_grounding('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', reciprocity_of_protections, conventional).
narrative_ontology:cs_reference_frame('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', westphalian_state_monopoly_on_force).
narrative_ontology:cs_drift_state('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe8dcb8d-c20b-4c3b-861b-d9bf26a361aa', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, states_prosecuting_non_state_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, individual_non_state_fighters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their personnel are guaranteed POW protections if captured, provided they meet Article 4 criteria. This framework legitimizes their actions in armed conflict and provides a clear legal status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, mobile, global).

% Benefit from the discretion to prosecute captured non-state fighters under domestic criminal law, rather than treating them as POWs. They actively enforce this definition to maintain control over the legal framework of conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, states_prosecuting_non_state_fighters, agenda_setter,
    institutional, biographical, arbitrage, global).

% Their members are categorically denied POW status, making them vulnerable to prosecution as criminals if captured, even if they adhere to the laws of war. This significantly raises the stakes for their operations and personnel.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, regional).

% Face the highest personal risk, as capture means criminal prosecution and potential long-term detention without POW protections. Their identity as a fighter is criminalized by this definition.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, individual_non_state_fighters, payer,
    powerless, immediate, trapped, local).

% Analyze the application and evolution of combatant status, often highlighting the gap between the state-centric definition and the realities of modern asymmetric conflicts involving non-state actors.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% Advocate for broader protections for all persons involved in armed conflict, including non-state actors, arguing that the state-centric definition leads to abuses and undermines the spirit of IHL. They are excluded from the direct enforcement mechanisms but exert pressure through public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, humanitarian_advocacy_groups, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, states_prosecuting_non_state_fighters).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, reciprocal framework for states to distinguish between lawful combatants (who receive POW status) and other actors in armed conflict, aiming to regularize interstate warfare.
% TRANSFER_FUNCTION: Transfers the right to POW protections from non-state armed groups and their members to the discretion of states, allowing states to prosecute them under domestic law.
% ABSENT_VOICES: Non-state armed groups and their captured members, who are directly impacted by the denial of POW status, are excluded from the legal and political processes that define and enforce this constraint. Humanitarian and human rights advocates often speak on their behalf but lack direct agency in the definition's application.
% DISAPPEARANCE_RATIONALE: If this state-centric definition vanished, the legal landscape of armed conflict would fundamentally shift. States would lose a primary tool for prosecuting non-state fighters, potentially leading to a re-evaluation of detention policies and the scope of protections for all conflict participants. The dynamics of asymmetric warfare would be profoundly altered.
% FOUNDING_PROBLEM: To establish clear criteria for who is a legitimate participant in warfare, ensuring discipline within armed forces, distinguishing combatants from civilians, and enabling reciprocal treatment of captured personnel, thereby limiting the brutality of war.
% FOUNDING_PROBLEM_CORROBORATION: State legal advisors, military manuals, and the historical record of the Geneva Conventions consistently attest to the ongoing relevance of distinguishing combatants. While challenged by modern conflict realities, the underlying problem of regulating armed violence remains live for states.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) and suppression (0.90) are high because this definition actively denies fundamental protections (POW status) to a significant class of actors in modern conflicts, enabling their criminalization. Accessibility collapse (0.95) is near total for non-state actors seeking POW status under this reading. Resistance (0.75) is substantial from non-state groups and human rights advocates. The theater ratio is low (0.10) as the definition is actively and consistently applied by states, not merely performed. The claimed type is 'tangled_rope' because it coordinates states by providing a clear legal framework for their militaries while simultaneously extracting heavily from non-state actors through denial of protections, requiring active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and their militaries, this definition provides essential order and reciprocity in warfare, ensuring their personnel are protected. From the perspective of non-state armed groups and individual fighters, it is a highly extractive and suppressive mechanism that denies them basic legal protections and criminalizes their participation in conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries are beneficiaries (low d) as their members receive POW protections. States prosecuting non-state fighters are agenda-setters and beneficiaries (low d) as they gain discretion and power. Non-state armed groups and individual fighters are clear targets (high d) as they bear the full cost of denied protections and face criminalization. Humanitarian advocacy groups are excluded, bearing costs indirectly through the impact on those they represent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, independent definition, or one reading of a contested kernel?',
    'Analysis of international legal discourse and state practice reveals multiple, competing interpretations of combatant status, confirming it as a kernel with distinct readings.',
    'Recognizing it as a reading enables comparative analysis with sibling readings, revealing the structural delta in extraction and suppression across different interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''state_centric_reading'' of the ''combatant_status_definition'' kernel.').

omega_variable(
    relevance_in_asymmetric_warfare,
    'Does the state-centric definition remain functionally relevant in an era dominated by asymmetric conflicts involving non-state actors, or has its original coordination function atrophied?',
    'Empirical analysis of conflict patterns and detention practices: if the majority of contemporary armed conflicts involve non-state actors whose status is consistently denied, the definition''s original purpose of regulating interstate war may be superseded by its extractive function.',
    'If its coordination function has atrophied, the constraint''s classification would shift closer to a pure Snare, as its primary effect would be extraction rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relevance_in_asymmetric_warfare, empirical, 'The functional relevance of the state-centric definition in modern asymmetric warfare.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-state actors'' claims to POW status structural (legal/institutional barriers) or internalized (lack of recognition/legitimacy)?',
    'Analysis of non-state actor behavior: if non-state actors consistently attempt to meet Article 4 criteria despite formal denial, the suppression is primarily structural. If they abandon such efforts due to perceived futility, it suggests internalized suppression.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the target carries the suppression with them, impacting their strategic choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-state actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1969, combatant_status_definition__state_centric_reading, theater_ratio, 1969, 0.07).
narrative_ontology:measurement(comb_tr_t1989, combatant_status_definition__state_centric_reading, theater_ratio, 1989, 0.08).
narrative_ontology:measurement(comb_tr_t2004, combatant_status_definition__state_centric_reading, theater_ratio, 2004, 0.09).
narrative_ontology:measurement(comb_tr_t2014, combatant_status_definition__state_centric_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.7).
narrative_ontology:measurement(comb_be_t1969, combatant_status_definition__state_centric_reading, base_extractiveness, 1969, 0.75).
narrative_ontology:measurement(comb_be_t1989, combatant_status_definition__state_centric_reading, base_extractiveness, 1989, 0.8).
narrative_ontology:measurement(comb_be_t2004, combatant_status_definition__state_centric_reading, base_extractiveness, 2004, 0.83).
narrative_ontology:measurement(comb_be_t2014, combatant_status_definition__state_centric_reading, base_extractiveness, 2014, 0.84).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.75).
narrative_ontology:measurement(comb_su_t1969, combatant_status_definition__state_centric_reading, suppression_requirement, 1969, 0.8).
narrative_ontology:measurement(comb_su_t1989, combatant_status_definition__state_centric_reading, suppression_requirement, 1989, 0.85).
narrative_ontology:measurement(comb_su_t2004, combatant_status_definition__state_centric_reading, suppression_requirement, 2004, 0.88).
narrative_ontology:measurement(comb_su_t2014, combatant_status_definition__state_centric_reading, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, rules_of_engagement_state_militaries).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, detention_policies_state_actors).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, prosecution_of_terrorists_domestic_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'combatant_status_definition' kernel, focusing on the state-centric interpretation. It is linked to sibling readings that offer alternative framings of combatant status and protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
