% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Income Support: Targeting Efficiency Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'targeting efficiency' reading of the
 *   broader 'income support commitment' kernel. It posits that welfare
 *   resources should be concentrated on those with demonstrated need, rather
 *   than distributed universally. This reading emphasizes fiscal prudence and
 *   avoiding perceived 'waste,' often leading to complex means-testing and
 *   eligibility requirements. While nominally benefiting recipients, the
 *   system's design and enforcement can create significant burdens and
 *   exclusions, leading to a high extraction profile, particularly when
 *   compared to alternative universal systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.8).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.75).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Income Support: Targeting Efficiency Reading").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624').
narrative_ontology:cs_kernel_codification('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', formalized).
narrative_ontology:cs_authority_grounding('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', lineage).
narrative_ontology:cs_interpretation_layer_present('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624').
narrative_ontology:cs_reading_relation('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', foundational, resource_scarcity_necessitates_targeting).
narrative_ontology:cs_axiom_status(resource_scarcity_necessitates_targeting, holdable).
narrative_ontology:cs_axiom_grounding('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', resource_scarcity_necessitates_targeting, empirically_contingent).
narrative_ontology:cs_axiom('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', foundational, universal_distribution_is_wasteful).
narrative_ontology:cs_axiom_status(universal_distribution_is_wasteful, holdable).
narrative_ontology:cs_axiom_grounding('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', universal_distribution_is_wasteful, empirically_contingent).
narrative_ontology:cs_reference_frame('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', post_war_welfare_consensus).
narrative_ontology:cs_drift_state('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', contemporary_neoliberal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ff1ea00-11ef-48fc-9c3c-d9b43c2d9624', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, taxpayers_concerned_with_efficiency).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_households_not_meeting_eligibility).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, advocates_for_universal_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, implement, and enforce complex eligibility criteria and means-testing procedures for income support programs. They justify these as necessary for fiscal responsibility and preventing 'waste.' Their power and budgets are tied to the complexity and administration of these targeted systems.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perception that their tax money is being used 'efficiently' by being directed only to those 'demonstrably in need.' They often advocate for stricter eligibility and against universal programs, believing they reduce overall tax burden and prevent 'freeloading.'
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, taxpayers_concerned_with_efficiency, beneficiary,
    organized, biographical, mobile, national).

% Receive essential income support, making them nominal beneficiaries. However, they bear the costs of administrative burden, stigma, and the constant threat of losing benefits due to minor changes in circumstances. They are also victims in the sense that this reading's suppression of universal alternatives means they lose out on potentially higher, less conditional support (e.g., the $19,100 loss under UBI replacement in the prompt's example).
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, beneficiary).

% Are excluded from income support due to strict eligibility criteria, even if they are in significant need. They bear the costs of poverty without the safety net, and their situation is often invisible to the system designed to target 'demonstrated need.'
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_households_not_meeting_eligibility, payer,
    powerless, immediate, trapped, local).

% Argue for universal basic income or other unconditional support systems, believing they are more effective, less stigmatizing, and provide a true freedom floor. They are often excluded from policy-making discussions dominated by the 'targeting efficiency' paradigm.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, advocates_for_universal_programs, excluded,
    organized, generational, constrained, national).

% Study the effects of targeted vs. universal income support, evaluating their efficiency, poverty reduction, and social impacts. Their analyses often challenge the assumptions of the 'targeting efficiency' reading but are not always integrated into policy.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, economic_policy_analysts, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, welfare_state_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To allocate limited public resources to individuals and families deemed most in need, based on specific eligibility criteria, thereby preventing perceived 'waste' and ensuring fiscal responsibility.
% TRANSFER_FUNCTION: Moves public funds from general taxation to a subset of the population identified through means-testing and other eligibility requirements, as a conditional safety net.
% ABSENT_VOICES: Those who are eligible but cannot navigate the bureaucratic hurdles, and advocates for universal programs, would object. They would argue for simpler, less stigmatizing systems that reach all in need without administrative barriers or the creation of poverty traps.
% DISAPPEARANCE_RATIONALE: If the entire system of targeted income support vanished overnight, millions of vulnerable individuals and families would lose their primary source of income, leading to widespread social crisis, increased poverty, and a massive reorganization of social welfare provision.
% FOUNDING_PROBLEM: To provide a safety net for the most vulnerable members of society while minimizing public expenditure, avoiding disincentives to work, and ensuring that support is directed only to those who genuinely cannot support themselves.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (welfare administrators, some politicians) argue the problem of limited resources and potential work disincentives is still live. Critics (social scientists, poverty advocates, some NGOs) argue that while the problem of vulnerability remains, the 'efficiency' framing has shifted to prioritize fiscal austerity and control over genuine need-meeting, and that the founding problem is now a cover for a system that creates new forms of extraction and exclusion. Legislative hearing testimony and independent academic research from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the significant administrative burden, stigma, and opportunity costs imposed on recipients, as well as the exclusion of many in need. The high suppression (0.75) stems from the active enforcement of complex eligibility rules and the suppression of alternative, simpler universal systems. The moderate theater ratio (0.4) indicates that while some administrative functions are genuine, a substantial portion of activity is performative, designed to demonstrate 'efficiency' and 'fraud prevention' rather than genuinely optimizing support delivery. Accessibility collapse is high (0.7) because for many, the targeted system is the only perceived path to support, despite its barriers. Resistance (0.6) is significant from advocates for universal programs and those struggling with the system.
 *
 * PERSPECTIVAL GAP:
 *   Administrators and efficiency-minded taxpayers perceive the system as a necessary, efficient allocation of resources. Recipients and excluded households experience it as a burdensome, stigmatizing, and often insufficient system that extracts significant non-monetary costs. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare state administrators are beneficiaries, gaining power and budget from managing complexity. Taxpayers concerned with efficiency are also beneficiaries, perceiving responsible use of funds. Current targeted program recipients are both nominal beneficiaries (receiving support) and victims (bearing administrative burden, stigma, and the opportunity cost of foregone universal benefits). Low-income households not meeting eligibility are clear victims, excluded from support. Advocates for universal programs are excluded from the policy-making process.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'targeting efficiency' reading risks mandatrophy by shifting focus from the original mandate of poverty alleviation to the secondary goal of 'efficiency' and fiscal control. The persistence of complex, burdensome systems, even as the founding problem of 'waste' becomes contested, suggests the constraint may be maintained more for the benefit of its administrators and the ideological comfort of certain taxpayers than for optimal social outcomes. The high extractiveness and suppression, coupled with contested founding problem status, indicate a potential snare where the coordination story (efficient allocation) serves as cover for extraction (administrative burden, exclusion, opportunity cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_vs_stigma_tradeoff,
    'Is the administrative efficiency gained by targeting income support outweighed by the social costs of stigma, administrative burden, and exclusion for those in need?',
    'Comparative studies of targeted vs. universal programs, measuring not just fiscal cost but also health outcomes, social cohesion, and administrative burden on recipients. Longitudinal studies tracking recipients'' experiences.',
    'If social costs outweigh efficiency gains, the constraint''s effective extractiveness is higher than currently measured, and its coordination function is severely compromised, pushing it further towards a snare. If efficiency gains are demonstrably high and social costs low, it might lean more towards a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_stigma_tradeoff, empirical, 'Trade-off between administrative efficiency and social costs of targeting.').

omega_variable(
    true_cost_of_targeting,
    'Does the administrative overhead of means-testing, enforcement, and fraud prevention in targeted programs actually result in net fiscal savings compared to a simpler, less conditional universal system, or does it create hidden costs?',
    'Comprehensive economic analysis comparing the total system costs (including administrative, enforcement, and indirect social costs) of targeted programs against the projected costs of universal basic income or similar universal schemes.',
    'If the true costs of targeting are higher than perceived, the ''efficiency'' justification for the constraint collapses, revealing a higher degree of extraction and theater, and strengthening its classification as a snare. If targeting is genuinely more cost-effective, the extractiveness might be slightly lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_targeting, empirical, 'Fiscal cost-effectiveness of targeted vs. universal income support.').

omega_variable(
    ubiquitous_extraction_from_poor_framing,
    'Is the ''extraction'' from current recipients (e.g., the $19,100 loss under UBI replacement) an inherent feature of this reading''s framing of welfare reform, or a contingent outcome of specific policy choices?',
    'Conceptual analysis of the ''targeting efficiency'' doctrine: does it structurally require that any universal alternative must cannibalize existing targeted programs, or could it theoretically coexist with or be replaced by a UBI that does not create such losses for the poor? This is a question of the reading''s internal logic.',
    'If the extraction is structurally required by the reading''s logic, it reinforces the snare classification by demonstrating how the reading itself generates victims. If it''s a contingent policy choice, the reading might be capable of less extractive instantiations, making it a more ambiguous tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ubiquitous_extraction_from_poor_framing, conceptual, 'Structural vs. contingent nature of extraction from the poor under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t1980, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(inco_tr_t1990, income_support_commitment__targeting_efficiency_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(inco_tr_t2000, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(inco_tr_t2010, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(inco_tr_t2024, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(inco_be_t1980, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(inco_be_t1990, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(inco_be_t2000, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(inco_be_t2010, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(inco_be_t2024, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t1980, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(inco_su_t1990, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(inco_su_t2000, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(inco_su_t2010, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(inco_su_t2024, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_commitment' kernel, focusing on targeting efficiency. It is structurally distinct from the 'freedom_floor_reading' and 'dependency_trap_reading' due to differing core premises regarding distribution and social outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
