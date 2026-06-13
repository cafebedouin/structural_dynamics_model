% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage Subsidy
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint describes unconditional income support (e.g., Universal
 *   Basic Income) as functioning primarily as an employer subsidy, rather
 *   than a worker freedom floor. In this reading, the income support allows
 *   employers to pay wages below subsistence levels, knowing that the state
 *   will cover the difference, thereby suppressing overall wage growth and
 *   maintaining a cheap labor pool. The coordination function is to ensure
 *   basic subsistence for workers, but this is coupled with an extractive
 *   transfer to employers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.65).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.7).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'fc08c6f9-1c4a-4e8c-952c-f533bdefee93').
narrative_ontology:cs_kernel_codification('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', formalized).
narrative_ontology:cs_authority_grounding('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', lineage).
narrative_ontology:cs_interpretation_layer_present('fc08c6f9-1c4a-4e8c-952c-f533bdefee93').
narrative_ontology:cs_reading_relation('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', income_support_conditionality__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', foundational, labor_market_power_asymmetry).
narrative_ontology:cs_axiom_status(labor_market_power_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', labor_market_power_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', foundational, income_support_externalizes_labor_costs).
narrative_ontology:cs_axiom_status(income_support_externalizes_labor_costs, holdable).
narrative_ontology:cs_axiom_grounding('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', income_support_externalizes_labor_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', neoclassical_labor_market_equilibrium).
narrative_ontology:cs_drift_state('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', contemporary_policy_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc08c6f9-1c4a-4e8c-952c-f533bdefee93', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, government_agencies).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a stable supply of labor willing to accept lower wages, as the state covers basic subsistence. This reduces their labor costs and increases profit margins, without needing to innovate or improve working conditions significantly.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    powerful, biographical, mobile, national).

% Receive basic income support, which prevents destitution but also reduces their bargaining power. They are still compelled to work low-wage jobs to supplement their income, effectively subsidizing their employers through suppressed wages.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, local).

% Fund the unconditional income support through taxes, part of which effectively flows to low-wage employers as a subsidy. They bear the cost without directly receiving the benefits of either worker freedom or employer profit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers, payer,
    organized, generational, mobile, national).

% Administer the income support program, ensuring social stability and preventing extreme poverty. They benefit from a simplified welfare system and reduced social unrest, even if the policy's secondary effects include wage suppression.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Would advocate for policies that genuinely empower workers and increase wages, rather than subsidizing employers. Their influence is often marginalized in policy debates where income support is framed solely as a welfare measure.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic income floor to ensure subsistence for all citizens, simplifying welfare administration and stabilizing social conditions by reducing poverty and precarity.
% TRANSFER_FUNCTION: Transfers public funds (from taxpayers) to individuals, which then indirectly transfers a portion of potential wage gains from low-wage workers to low-wage employers by reducing pressure for higher wages.
% ABSENT_VOICES: Labor unions and worker advocacy groups, who would argue that the policy, in its current form, undermines collective bargaining and wage growth, are often sidelined in discussions that frame UBI solely as a poverty reduction tool.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage employers would face immediate pressure to raise wages to subsistence levels, or risk severe labor shortages. Social welfare systems would revert to more complex, conditional forms, and poverty rates would likely increase sharply, leading to significant social and economic reorganization.
% FOUNDING_PROBLEM: The problem of widespread poverty, increasing precarity in the labor market, and the administrative complexity of traditional welfare systems.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of unconditional income support (e.g., some academics, social policy advocates) attest that the founding problems of poverty and precarity remain live. Critics (e.g., some labor economists, union representatives) corroborate the existence of these problems but dispute whether unconditional income support, without other labor market interventions, effectively solves them or merely shifts the burden.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because employers benefit from reduced wage pressure, effectively externalizing a portion of their labor costs onto the state/taxpayers. Suppression (0.7) is high as workers, despite receiving income support, remain constrained by the low-wage labor market and the need to supplement their basic income, limiting their ability to demand higher wages or exit exploitative jobs. The theater ratio (0.2) is low, as the income support genuinely provides subsistence, but its 'freedom-enhancing' narrative masks its wage-suppressing effect.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage employers, the income support is a beneficial policy that stabilizes the labor market and reduces business costs. From the perspective of low-wage workers, while providing a safety net, it also entrenches them in precarious employment by removing the pressure for employers to offer living wages. Taxpayers experience it as a cost, part of which subsidizes private enterprise.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers are primary beneficiaries (d=0.0-0.1) as they can maintain lower wages. Government agencies administering the program are also beneficiaries (d=0.1-0.2) as it stabilizes social welfare without directly challenging the existing labor market structure. Low-wage workers are victims (d=0.7-0.8) as their wages are suppressed, even if their basic needs are met. Taxpayers are also victims (d=0.6-0.7) as they fund the subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests that while the original mandate might have been to provide a safety net or increase worker autonomy, the constraint has drifted to primarily serve as an employer subsidy. The classification as Tangled Rope captures this dual function: it coordinates basic subsistence but extracts value by suppressing wages. Resolving this mandatrophy would require policies that prevent wage suppression, such as minimum wage increases or stronger labor protections, to ensure the income support genuinely empowers workers rather than subsidizing employers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine employer subsidy, or does it primarily function as a freedom floor or dependency trap?',
    'Empirical studies on wage elasticity and labor supply responses to unconditional income, disaggregated by employer size and sector. Analysis of worker exit rates from low-wage jobs.',
    'If primarily an employer subsidy, the classification as Tangled Rope is robust. If it functions more as a freedom floor, it would shift towards Rope; if a dependency trap, towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Distinguishing the wage subsidy reading from other interpretations of unconditional income support.').

omega_variable(
    subsidy_capture_mechanism,
    'To what extent is the income support captured by employers through wage suppression versus directly benefiting workers?',
    'Econometric analysis of wage trends in sectors with high UBI uptake compared to control groups, controlling for productivity and market conditions. Worker surveys on perceived bargaining power.',
    'Higher capture by employers strengthens the Tangled Rope classification; higher direct worker benefit would weaken the extractive component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_capture_mechanism, empirical, 'Quantifying the degree of employer capture of income support.').

omega_variable(
    policy_intent_vs_outcome,
    'Is the observed wage suppression an unintended consequence of a policy designed for worker welfare, or an implicit goal of maintaining a low-wage labor pool?',
    'Analysis of policy documents, legislative debates, and lobbying efforts by employer groups. Interviews with policymakers and labor economists.',
    'If unintended, it suggests a design flaw in a Rope-like policy; if implicit, it reinforces the Snare-like aspects of the Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome, conceptual, 'Distinguishing between policy intent and actual structural outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__wage_subsidy_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(inco_tr_t50, income_support_conditionality__wage_subsidy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(inco_be_t50, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(inco_su_t50, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'income_support_conditionality' kernel, focusing on its function as an employer wage subsidy. It is linked to the 'freedom_floor_reading' and 'dependency_trap_reading' as alternative interpretations of the same policy mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
