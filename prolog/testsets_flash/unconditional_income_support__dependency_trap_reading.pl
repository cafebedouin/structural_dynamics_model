% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint models unconditional income support (UIS) from the
 *   'dependency trap' perspective, where it is viewed as an
 *   incentive-distorting subsidy. It rewards idleness, crowds out more
 *   effective targeted aid, and redistributes wealth upward to non-needy
 *   populations, creating a net fiscal burden on taxpayers and harming the
 *   working poor by replacing programs that offered greater net benefit. The
 *   constraint is actively enforced through tax collection and welfare
 *   program restructuring.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.85).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '5f4aac09-e571-4a74-b9c8-f95b15f9f2ae').
narrative_ontology:cs_kernel_codification('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', formalized).
narrative_ontology:cs_authority_grounding('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', extraction).
narrative_ontology:cs_interpretation_layer_present('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae').
narrative_ontology:cs_reading_relation('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', foundational, incentives_matter_for_labor).
narrative_ontology:cs_axiom_status(incentives_matter_for_labor, holdable).
narrative_ontology:cs_axiom_grounding('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', incentives_matter_for_labor, empirically_contingent).
narrative_ontology:cs_axiom('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', foundational, targeted_aid_is_more_efficient).
narrative_ontology:cs_axiom_status(targeted_aid_is_more_efficient, holdable).
narrative_ontology:cs_axiom_grounding('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', targeted_aid_is_more_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', traditional_welfare_state_incentive_design).
narrative_ontology:cs_drift_state('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', contemporary_ubi_proposals, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5f4aac09-e571-4a74-b9c8-f95b15f9f2ae', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience a net loss of welfare as targeted aid programs, which provided greater overall benefit, are replaced or reduced by a universal income that does not adequately compensate for the loss. They face disincentives to work and limited pathways out of poverty.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Bear the significant fiscal cost of the universal income program, estimated at $1.4 trillion after offsets, without receiving proportional benefits, especially those in higher income brackets who are net contributors.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Receive unconditional income transfers despite not needing them for basic subsistence, leading to an upward redistribution of wealth and a net benefit without significant behavioral changes.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients, beneficiary,
    powerful, biographical, mobile, national).

% Gain political capital and validation for their policy agenda, benefiting from the implementation of a universal income program regardless of its actual economic and social outcomes for the most vulnerable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocates, beneficiary,
    organized, generational, analytical, global).

% Responsible for implementing and enforcing the unconditional income program, including tax collection and the restructuring or elimination of existing targeted welfare programs. They manage the fiscal and administrative burden.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Conduct research and publish analyses (e.g., AEI meta-analysis) highlighting the negative employment impacts, fiscal costs, and regressive redistributive effects of unconditional income support, informing policy debates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, economists_critiquing_ubi, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simplifies welfare administration by replacing complex, means-tested programs with a single, universal transfer, theoretically reducing bureaucratic overhead and stigma.
% TRANSFER_FUNCTION: Transfers a fixed income amount to all citizens, funded by general taxation, effectively moving wealth from net taxpayers to all recipients, including those who do not need it.
% ABSENT_VOICES: Advocates for highly targeted, conditional welfare programs are marginalized, as the universal approach inherently de-emphasizes conditionality and specific needs. Their arguments for more efficient poverty reduction are not central to the UBI debate.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished, the welfare state would revert to a more targeted, conditional system, and the fiscal burden on taxpayers would decrease. Labor market incentives would shift, and the working poor would likely see a return to more beneficial targeted programs, reorganizing the social safety net.
% FOUNDING_PROBLEM: The problem of poverty, welfare stigma, and the administrative complexity of existing social safety nets.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates claim the founding problem is still live, arguing for its continued necessity. Economists critiquing UBI and advocates for targeted aid argue that the founding problem of poverty and administrative complexity is better addressed by alternative means, and that UBI itself creates new problems, corroborating the 'dead' or 'contested' status of the founding problem for this specific solution.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the program's universal nature means significant funds are transferred to individuals who do not need them, while simultaneously reducing or eliminating targeted programs that provided greater net benefit to the working poor. Suppression (0.75) is high due to the structural changes in the welfare state that reduce alternatives for the working poor and the coercive nature of taxation. Theater ratio is low (0.2) as the program's stated goals (e.g., poverty reduction) are seen as largely performative cover for its actual redistributive and incentive-distorting effects.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the working poor, the constraint is a Snare, as it reduces their net welfare and limits their options. For middle/upper-class recipients, it is a diffuse benefit, while for UBI advocates, it is a policy win. Taxpayers experience it as a net cost. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The middle/upper-class recipients and UBI advocates are beneficiaries (d near 0.0) as they receive transfers without needing them or gain political capital. The working poor are victims (d near 1.0) as they lose more valuable targeted aid and face disincentives to work. Taxpayers are also victims (d near 1.0) due to the significant fiscal burden. The constraint subsidizes some while extracting from others through the same universal mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the mandate of unconditional income support, if framed as poverty reduction, has atrophied. Instead of solving the problem, it exacerbates it for the most vulnerable by creating a dependency trap and misallocating resources. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting its extractive and harmful aspects for specific groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine dependency trap, or an autonomy-enabling floor (freedom_floor_reading) or a politically ambiguous Trojan horse (universality_paradox_reading)?',
    'Longitudinal studies on labor market participation, poverty reduction, and fiscal impact, disaggregated by income quintile and pre-existing welfare program access.',
    'If the dependency_trap_reading is confirmed, the constraint is a Snare. If freedom_floor_reading is confirmed, it is a Rope. If universality_paradox_reading is confirmed, it is a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is one reading of the ''unconditional_income_support'' kernel, specifically the ''dependency_trap_reading''.').

omega_variable(
    employment_impact_measurement,
    'What is the true net impact of unconditional income support on labor force participation and economic productivity, accounting for all direct and indirect effects?',
    'Comprehensive meta-analysis of large-scale, long-term randomized control trials (RCTs) and natural experiments across diverse economic contexts, controlling for pre-existing welfare program displacement.',
    'If the negative employment impact (e.g., -3.2% as per AEI meta-analysis) is robust, it reinforces the high extractiveness and Snare classification. If the impact is neutral or positive, it would reduce extractiveness and shift classification towards Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_impact_measurement, empirical, 'Uncertainty regarding the precise and comprehensive employment impact of UBI.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__dependency_trap_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__dependency_trap_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__dependency_trap_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__dependency_trap_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__dependency_trap_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__dependency_trap_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__dependency_trap_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__dependency_trap_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__dependency_trap_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel. This 'dependency_trap_reading' emphasizes the negative incentive effects and regressive redistribution, contrasting with the 'freedom_floor_reading' (autonomy-enabling) and 'universality_paradox_reading' (politically ambiguous).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
