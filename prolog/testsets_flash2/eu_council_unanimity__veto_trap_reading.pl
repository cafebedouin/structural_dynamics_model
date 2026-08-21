% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity: Veto Trap Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.85).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.7).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity: Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '2b77e04d-98b3-4e5d-aa7d-149464bb974c').
narrative_ontology:cs_kernel_codification('2b77e04d-98b3-4e5d-aa7d-149464bb974c', formalized).
narrative_ontology:cs_authority_grounding('2b77e04d-98b3-4e5d-aa7d-149464bb974c', lineage).
narrative_ontology:cs_interpretation_layer_present('2b77e04d-98b3-4e5d-aa7d-149464bb974c').
narrative_ontology:cs_reading_relation('2b77e04d-98b3-4e5d-aa7d-149464bb974c', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b77e04d-98b3-4e5d-aa7d-149464bb974c', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('2b77e04d-98b3-4e5d-aa7d-149464bb974c', foundational, unanimity_as_minoritarian_leverage).
narrative_ontology:cs_axiom_status(unanimity_as_minoritarian_leverage, holdable).
narrative_ontology:cs_axiom_grounding('2b77e04d-98b3-4e5d-aa7d-149464bb974c', unanimity_as_minoritarian_leverage, empirically_contingent).
narrative_ontology:cs_axiom('2b77e04d-98b3-4e5d-aa7d-149464bb974c', secondary, policy_stasis_as_extracted_value).
narrative_ontology:cs_axiom_status(policy_stasis_as_extracted_value, holdable).
narrative_ontology:cs_axiom_grounding('2b77e04d-98b3-4e5d-aa7d-149464bb974c', policy_stasis_as_extracted_value, empirically_contingent).
narrative_ontology:cs_reference_frame('2b77e04d-98b3-4e5d-aa7d-149464bb974c', unanimity_as_extraction_mechanism).
narrative_ontology:cs_drift_state('2b77e04d-98b3-4e5d-aa7d-149464bb974c', contemporary_eu_policy_making, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2b77e04d-98b3-4e5d-aa7d-149464bb974c', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_citizens_affected_by_policy_stasis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single EU member state that uses its unanimity veto power to block proposed legislation, not to protect its sovereignty, but to extract concessions or opt-outs from the majority. Benefits directly from the value transferred through these concessions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter,
    powerful, biographical, arbitrage, national).

% The group of member states that support a proposed policy but are forced to make concessions to a blocking state to advance the legislation. They bear the cost of policy dilution or delayed implementation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states, payer,
    institutional, generational, constrained, continental).

% Citizens across the EU who are negatively impacted by the inability of the Council to pass necessary legislation due to veto threats, leading to policy stasis or suboptimal outcomes. They bear diffuse costs without direct recourse.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens_affected_by_policy_stasis, payer,
    powerless, immediate, trapped, continental).

% The European Commission and Parliament, which propose and debate legislation, but are ultimately constrained by the Council's unanimity rule. They observe the extraction but have limited power to directly overcome it.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_institutions, observer,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The unanimity rule nominally ensures that all member states are aligned on critical decisions, fostering collective ownership and preventing policies that deeply harm any single state's core interests.
% TRANSFER_FUNCTION: Transfers policy influence and material concessions from the coalition majority to the blocking minority, in exchange for allowing legislation to proceed. This can manifest as opt-outs, financial benefits, or watering down of policy ambition.
% ABSENT_VOICES: EU citizens, particularly those in states whose governments are part of the blocking minority but whose populations would benefit from the blocked policy, are absent from the direct negotiation. Their interests are often subordinated to the short-term leverage of their national government.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, the EU Council's decision-making process would fundamentally change. Legislation would pass more quickly, but potentially at the cost of alienating minority states. The balance of power would shift dramatically, leading to a reorganization of political alliances and negotiation strategies.
% FOUNDING_PROBLEM: The unanimity rule was established to protect the sovereign interests of individual member states, ensuring that no state could be forced into a policy against its fundamental will, particularly in sensitive areas like foreign policy or taxation.
% FOUNDING_PROBLEM_CORROBORATION: Blocking member states and some legal scholars attest that the founding problem of protecting sovereignty remains live. However, a majority of member states, EU institutions, and political scientists argue that the rule is now primarily used for minoritarian extraction, indicating the founding problem is either dead or severely distorted from its original intent, supported by empirical studies of veto patterns and policy outcomes.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_extraction_intent,
    'Is a given veto exercised primarily to protect a core sovereign interest (as per the founding problem) or to extract unrelated concessions?',
    'Detailed case studies of veto incidents, analyzing the content of concessions granted versus the stated reasons for the veto, and comparing with independent assessments of the ''core'' nature of the sovereign interest.',
    'If primarily for extraction, the Snare classification is strengthened. If genuinely for core sovereign protection, the constraint might lean towards a Tangled Rope or even a Rope (as a legitimate coordination mechanism for diverse states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_extraction_intent, empirical, 'Distinguishing legitimate sovereignty protection from opportunistic extraction.').

omega_variable(
    policy_stasis_causality,
    'To what extent is policy stasis in the EU Council directly attributable to the unanimity rule, versus other factors like fundamental policy disagreements or lack of political will?',
    'Counterfactual analysis comparing policy outcomes in areas under unanimity versus qualified majority voting, controlling for policy complexity and political salience. Expert surveys on the primary drivers of legislative deadlock.',
    'If unanimity is a primary driver, the Snare classification is reinforced due to its direct causal role in negative outcomes. If other factors dominate, the extractiveness attributed to the unanimity rule might be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_stasis_causality, empirical, 'Causal attribution of policy stasis to the unanimity rule.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''veto trap'' reading, how do the ''sovereignty guarantor'' and ''diplomatic capital'' readings structurally differ in their assessment of extractiveness and suppression?',
    'Comparative analysis of the three constraint stories (this one and its siblings) to identify specific metric divergences and their underlying structural assumptions about the unanimity rule''s function and effects.',
    'The divergence highlights the contested nature of the kernel. If the other readings show significantly lower extractiveness, it underscores the ''veto trap'' as a critical counter-narrative to more benign interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between the ''veto trap'' and sibling readings of EU Council unanimity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__veto_trap_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__veto_trap_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__veto_trap_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__veto_trap_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__veto_trap_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
