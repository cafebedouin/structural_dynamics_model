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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeting Efficiency in Income Support
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'targeting efficiency' reading of the
 *   broader income_support_commitment kernel. It asserts that income support
 *   should be concentrated on demonstrated need, rather than universally
 *   distributed. This reading frames universal programs, like UBI, as
 *   inefficient and potentially harmful, especially if they are funded by
 *   dismantling existing targeted programs. The core tension is that those
 *   nominally 'benefiting' from targeted programs (low-income households) can
 *   become 'victims' if these programs are replaced by a less generous
 *   universal scheme, highlighting the extractive nature of this 'efficiency'
 *   from their perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.85).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.75).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeting Efficiency in Income Support").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'ebca4606-19ec-49bc-9708-5bde838c3b72').
narrative_ontology:cs_kernel_codification('ebca4606-19ec-49bc-9708-5bde838c3b72', formalized).
narrative_ontology:cs_authority_grounding('ebca4606-19ec-49bc-9708-5bde838c3b72', extraction).
narrative_ontology:cs_interpretation_layer_present('ebca4606-19ec-49bc-9708-5bde838c3b72').
narrative_ontology:cs_reading_relation('ebca4606-19ec-49bc-9708-5bde838c3b72', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('ebca4606-19ec-49bc-9708-5bde838c3b72', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ebca4606-19ec-49bc-9708-5bde838c3b72', foundational, resource_scarcity_requires_prioritization).
narrative_ontology:cs_axiom_status(resource_scarcity_requires_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('ebca4606-19ec-49bc-9708-5bde838c3b72', resource_scarcity_requires_prioritization, empirically_contingent).
narrative_ontology:cs_axiom('ebca4606-19ec-49bc-9708-5bde838c3b72', secondary, moral_hazard_of_unconditional_aid).
narrative_ontology:cs_axiom_status(moral_hazard_of_unconditional_aid, holdable).
narrative_ontology:cs_axiom_grounding('ebca4606-19ec-49bc-9708-5bde838c3b72', moral_hazard_of_unconditional_aid, empirically_contingent).
narrative_ontology:cs_reference_frame('ebca4606-19ec-49bc-9708-5bde838c3b72', post_new_deal_means_testing_consensus).
narrative_ontology:cs_drift_state('ebca4606-19ec-49bc-9708-5bde838c3b72', contemporary_ubi_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ebca4606-19ec-49bc-9708-5bde838c3b72', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, targeted_program_administrators).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, taxpayers_concerned_with_waste).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_households_under_ubi_replacement).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, marginalized_groups_facing_stigma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer complex means-tested programs, justifying their existence by the need to efficiently allocate resources to the truly needy. Their institutional power and budgets are tied to the complexity and specificity of these programs.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perception that their tax dollars are not 'wasted' on those who don't 'deserve' it, or on universal programs that include the wealthy. They support policies that emphasize targeting and efficiency, even if the administrative costs are high.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, taxpayers_concerned_with_waste, beneficiary,
    organized, biographical, mobile, national).

% Currently receive substantial targeted benefits (e.g., $31,100). Under a UBI system funded by cannibalizing these programs, they would lose net income (e.g., $19,100), becoming victims of a system ostensibly designed to help. Their options are limited by their economic precarity.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_households_under_ubi_replacement, payer,
    powerless, immediate, trapped, local).

% Bear the non-monetary costs of means-testing, including stigma, invasive eligibility checks, and the psychological burden of proving 'need.' Their identity is often tied to their status as recipients, making exit from the system difficult without losing essential support.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, marginalized_groups_facing_stigma, payer,
    powerless, biographical, identity_locked, local).

% Argue for universal distribution as a more efficient and dignified approach, but their proposals are often framed as fiscally irresponsible or as undermining the 'targeting efficiency' principle. They are excluded from the core policy-making conversation that prioritizes means-testing.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_advocates, excluded,
    moderate, generational, constrained, national).

% Analyze the effectiveness, costs, and outcomes of targeted vs. universal income support programs. They provide evidence on administrative overhead, poverty reduction, and social impacts, often highlighting the trade-offs between targeting and other policy goals.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, social_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of limited public funds to individuals and families deemed most in need, preventing perceived 'waste' on those who could support themselves.
% TRANSFER_FUNCTION: Transfers public funds from the general tax base to specific low-income households, while simultaneously transferring administrative costs and the burden of proving need onto recipients and the state bureaucracy.
% ABSENT_VOICES: Advocates for universal basic income (UBI) are largely absent from the policy-making table, as their proposals directly challenge the foundational premise of targeting efficiency. They would argue for the dignity and administrative simplicity of universal programs.
% DISAPPEARANCE_RATIONALE: If the commitment to targeting efficiency vanished, the entire structure of welfare programs would need to be rethought. Funds would likely be redirected towards universal programs, fundamentally altering the relationship between the state and its citizens regarding income support. Targeted program administrators would lose their mandate.
% FOUNDING_PROBLEM: The problem of efficiently allocating scarce public resources to alleviate poverty and ensure basic living standards, while minimizing perceived misuse of funds.
% FOUNDING_PROBLEM_CORROBORATION: Targeted program administrators and many taxpayers attest that the problem of scarce resources and the need for efficient allocation remains live. Social policy researchers, from outside the benefiting parties, corroborate the ongoing challenge of poverty but often question whether current targeting mechanisms are the most effective solution.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because the administrative overhead of means-testing is substantial, and the 'efficiency' often comes at the cost of dignity and actual net benefit for the poorest, especially when compared to potential UBI alternatives. Suppression (0.75) is high due to the bureaucratic hurdles, stigma, and the lack of viable alternatives for those dependent on these programs. Theater ratio (0.20) is low, as the administrative machinery is genuinely active, though its 'efficiency' claims are often performative rather than truly cost-effective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of program administrators and certain taxpayers, this constraint is a 'rope' or 'scaffold' that efficiently allocates resources. However, from the perspective of low-income households, it operates as a 'snare,' trapping them in a system that extracts dignity and can lead to net losses if alternatives are suppressed. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted program administrators are clear beneficiaries, as their institutional power and budgets are tied to the complexity of means-testing. Taxpayers concerned with waste also benefit from the perception of fiscal prudence. Low-income households, particularly those who would lose net benefits under a UBI replacement, are victims, as are marginalized groups who bear the stigma and administrative burden of proving need. UBI advocates are excluded, as their proposals fundamentally challenge the premise of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_benefit_under_alternative,
    'What would be the actual net financial and non-financial impact on low-income households if targeted programs were replaced by a universal basic income?',
    'Empirical studies comparing the total value of targeted benefits (including in-kind support) with proposed UBI levels, accounting for administrative burden and stigma.',
    'If net benefits for the poorest decrease, it strengthens the ''snare'' classification by demonstrating direct extraction from the vulnerable. If net benefits increase, it weakens the ''snare'' classification, suggesting the ''victim'' status is misattributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_benefit_under_alternative, empirical, 'Quantifies the real-world impact of replacing targeted programs with UBI on vulnerable populations.').

omega_variable(
    administrative_efficiency_vs_dignity,
    'Is the ''efficiency'' gained by targeting income support outweighed by the administrative costs and the loss of dignity and autonomy for recipients?',
    'Comparative analysis of administrative costs for targeted vs. universal programs, combined with qualitative studies on recipient experience and social outcomes.',
    'If administrative costs are high and dignity is low, it undermines the ''efficiency'' claim and reinforces the extractive nature of the constraint. If the system is genuinely efficient and respectful, it would challenge the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_efficiency_vs_dignity, conceptual, 'Examines the trade-off between fiscal efficiency and human dignity in welfare policy.').

omega_variable(
    framing_of_need_vs_right,
    'Is income support fundamentally a response to ''demonstrated need'' (a conditional grant) or a ''universal right'' (an unconditional entitlement)?',
    'Analysis of public discourse, legislative debates, and judicial rulings over time, tracing the evolution of the underlying normative framework.',
    'If framed as a ''need,'' the targeting efficiency reading is reinforced. If framed as a ''right,'' the targeting efficiency reading becomes a form of extraction from a universal entitlement, strengthening the ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_need_vs_right, preference, 'The fundamental normative grounding of income support policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__targeting_efficiency_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__targeting_efficiency_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
