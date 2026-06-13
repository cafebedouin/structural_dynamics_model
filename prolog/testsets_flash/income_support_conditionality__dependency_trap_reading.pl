% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'dependency trap' reading of
 *   unconditional income support. It posits that providing income without
 *   work requirements leads to long-term dependency, skill atrophy, and a
 *   drain on public resources. The constraint is framed as a Snare, trapping
 *   recipients in idleness and extracting from taxpayers. The metrics reflect
 *   a high degree of extraction and suppression, as the system is seen to
 *   actively disincentivize work and limit recipients' agency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.85).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.75).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '30a31080-0dc3-40d0-9678-6890a41d8936').
narrative_ontology:cs_kernel_codification('30a31080-0dc3-40d0-9678-6890a41d8936', implicit).
narrative_ontology:cs_authority_grounding('30a31080-0dc3-40d0-9678-6890a41d8936', extraction).
narrative_ontology:cs_interpretation_layer_present('30a31080-0dc3-40d0-9678-6890a41d8936').
narrative_ontology:cs_reading_relation('30a31080-0dc3-40d0-9678-6890a41d8936', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('30a31080-0dc3-40d0-9678-6890a41d8936', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('30a31080-0dc3-40d0-9678-6890a41d8936', foundational, work_is_primary_source_of_dignity_and_skill).
narrative_ontology:cs_axiom_status(work_is_primary_source_of_dignity_and_skill, holdable).
narrative_ontology:cs_axiom_grounding('30a31080-0dc3-40d0-9678-6890a41d8936', work_is_primary_source_of_dignity_and_skill, deontological).
narrative_ontology:cs_axiom('30a31080-0dc3-40d0-9678-6890a41d8936', foundational, unconditional_transfers_distort_labor_markets).
narrative_ontology:cs_axiom_status(unconditional_transfers_distort_labor_markets, holdable).
narrative_ontology:cs_axiom_grounding('30a31080-0dc3-40d0-9678-6890a41d8936', unconditional_transfers_distort_labor_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('30a31080-0dc3-40d0-9678-6890a41d8936', conditional_welfare_state_with_work_incentives).
narrative_ontology:cs_drift_state('30a31080-0dc3-40d0-9678-6890a41d8936', contemporary_ubi_advocacy_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('30a31080-0dc3-40d0-9678-6890a41d8936', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, bureaucratic_administrators).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_advocates_for_dependency_trap_narrative).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, unconditional_income_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals receiving unconditional income support. From this reading's perspective, they are trapped in idleness, experience skill degradation, and become dependent on the state, losing the capacity for self-sufficiency. Their 'payment' is the loss of agency and human capital.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, unconditional_income_recipients, payer,
    powerless, biographical, identity_locked, national).

% Individuals and entities funding unconditional income programs through taxes. They bear the cost of supporting a non-productive population, leading to resentment and a perceived drain on the economy. Their exit options are limited to political action or emigration.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Government agencies and personnel responsible for implementing and managing unconditional income programs. They benefit from the expansion of their mandate and budget, even if the program's outcomes are framed as negative for recipients. They administer the 'trap'.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, bureaucratic_administrators, agenda_setter,
    institutional, biographical, mobile, national).

% Political parties, think tanks, and media outlets that use the 'dependency trap' narrative to advocate for conditional welfare policies, reduced social spending, or specific economic ideologies. They gain political capital and influence from this framing.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_advocates_for_dependency_trap_narrative, beneficiary,
    powerful, generational, arbitrage, national).

% Businesses that rely on a supply of labor willing to accept low wages. While not directly part of the income support system, they are excluded from the debate about its effects on labor supply, which this reading claims is negative for the economy by reducing the available workforce.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers_of_low_wage_labor, excluded,
    powerful, biographical, mobile, national).

% Academics and researchers studying the effects of unconditional income on labor supply, wages, and economic productivity. They analyze data and models to assess the claims of dependency and skill atrophy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint, as a 'dependency trap,' coordinates the allocation of social resources away from unconditional support towards conditional programs, aiming to incentivize work and prevent perceived idleness.
% TRANSFER_FUNCTION: Transfers tax revenue from productive taxpayers to non-productive recipients, and transfers the burden of skill atrophy and dependency onto the recipients themselves.
% ABSENT_VOICES: Advocates for unconditional income as a 'freedom floor' or 'wage subsidy' are excluded from this reading's internal logic; they would argue that the 'dependency' is a mischaracterization of increased worker bargaining power or a necessary social safety net.
% DISAPPEARANCE_RATIONALE: If the belief that unconditional income creates dependency vanished, policy debates would shift dramatically, potentially leading to widespread adoption of UBI or similar programs. The political and social structures built around conditional welfare would be dismantled, and labor market dynamics would fundamentally change.
% FOUNDING_PROBLEM: The problem of ensuring social welfare while maintaining work incentives and preventing long-term reliance on state support.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by political commentators, conservative think tanks, and some economists who cite historical examples of welfare programs and their perceived negative effects on labor participation. However, this corroboration is often from parties who benefit from the dependency trap narrative itself.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the perceived cost to taxpayers and the 'extraction' of human capital from recipients through skill atrophy. Suppression (0.75) is high because the system, by providing just enough to subsist, suppresses the incentive to seek employment and limits alternatives to dependency. The low theater ratio (0.1) indicates that the perceived negative effects are considered real and not merely performative. The rising extractiveness and suppression over time reflect a growing concern and perceived worsening of the 'dependency trap' phenomenon.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of recipients and taxpayers (as framed by this reading), the constraint is a Snare, extracting resources and agency. From the perspective of bureaucratic administrators and political advocates, it is a mechanism that, while problematic, justifies their intervention and policy positions. The engine's classification will likely align with the Snare type given the high extraction and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Unconditional income recipients are victims (payers of human capital and agency) as they are seen to lose skills and become dependent. Taxpayers are also victims (payers of financial resources). Bureaucratic administrators and political advocates of this narrative are beneficiaries, as the system expands their influence and validates their policy positions. The 'dependency trap' itself is the mechanism that extracts from recipients and taxpayers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_dependency,
    'Does robust empirical evidence consistently demonstrate that unconditional income support leads to long-term dependency and skill atrophy across diverse contexts?',
    'Longitudinal studies of UBI pilots and large-scale unconditional cash transfer programs, comparing labor market outcomes, skill development, and psychological well-being of recipients versus control groups.',
    'If evidence for dependency is weak or mixed, the extractiveness and suppression metrics would be significantly lower, potentially reclassifying the constraint away from a Snare. If strong, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_dependency, empirical, 'The empirical basis for the dependency trap claim.').

omega_variable(
    causality_vs_correlation,
    'Is the observed ''dependency'' a direct causal effect of unconditional income, or a correlation with pre-existing conditions (e.g., lack of opportunities, health issues) that unconditional income merely fails to solve?',
    'Causal inference studies (e.g., randomized controlled trials) designed to isolate the effect of unconditional income from confounding factors, and qualitative studies exploring recipients'' motivations and constraints.',
    'If causality is weak, the ''suppression'' metric would decrease, as the constraint is less actively ''trapping'' and more passively failing to address underlying issues. This would shift the classification towards a less extractive type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_vs_correlation, empirical, 'Distinguishing causal effects from correlation in dependency claims.').

omega_variable(
    alternative_framings_impact,
    'How would the classification of this constraint change if viewed through the ''freedom_floor_reading'' or ''wage_subsidy_reading''?',
    'Generate separate constraint stories for each sibling reading, with their own metrics and stakeholder analyses, and compare the resulting classifications.',
    'The ''freedom_floor_reading'' would likely classify it as a Rope (coordination for autonomy), while the ''wage_subsidy_reading'' might classify it as a Snare (extracting from workers to subsidize employers) but with a different victim set and beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framings_impact, conceptual, 'Impact of alternative readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t1970, income_support_conditionality__dependency_trap_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(inco_tr_t1985, income_support_conditionality__dependency_trap_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(inco_tr_t2000, income_support_conditionality__dependency_trap_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(inco_tr_t2010, income_support_conditionality__dependency_trap_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(inco_tr_t2024, income_support_conditionality__dependency_trap_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t1970, income_support_conditionality__dependency_trap_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(inco_be_t1985, income_support_conditionality__dependency_trap_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(inco_be_t2000, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(inco_be_t2010, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(inco_be_t2024, income_support_conditionality__dependency_trap_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t1970, income_support_conditionality__dependency_trap_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(inco_su_t1985, income_support_conditionality__dependency_trap_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(inco_su_t2000, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(inco_su_t2010, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(inco_su_t2024, income_support_conditionality__dependency_trap_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_conditionality' kernel, focusing on the dependency trap. Sibling readings include 'freedom_floor_reading' and 'wage_subsidy_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
