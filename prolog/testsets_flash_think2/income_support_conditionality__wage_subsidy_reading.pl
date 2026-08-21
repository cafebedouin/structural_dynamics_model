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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint story instantiates the 'wage_subsidy_reading' of the
 *   'income_support_conditionality' kernel. It analyzes unconditional income
 *   support policies (e.g., UBI, certain welfare programs) not as a pure
 *   safety net or freedom-enhancing measure, but as a mechanism that, by
 *   providing a subsistence floor, allows employers to suppress wages below a
 *   living standard, effectively transferring public funds to corporate
 *   profits. The policy is presented as a 'rope' for workers, but functions
 *   as a 'tangled_rope' due to the asymmetric extraction by employers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.78).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.85).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '66905dad-2cb5-413b-9ffd-29c66fd4cb02').
narrative_ontology:cs_kernel_codification('66905dad-2cb5-413b-9ffd-29c66fd4cb02', formalized).
narrative_ontology:cs_authority_grounding('66905dad-2cb5-413b-9ffd-29c66fd4cb02', lineage).
narrative_ontology:cs_interpretation_layer_present('66905dad-2cb5-413b-9ffd-29c66fd4cb02').
narrative_ontology:cs_reading_relation('66905dad-2cb5-413b-9ffd-29c66fd4cb02', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('66905dad-2cb5-413b-9ffd-29c66fd4cb02', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('66905dad-2cb5-413b-9ffd-29c66fd4cb02', foundational, income_support_displaces_wages).
narrative_ontology:cs_axiom_status(income_support_displaces_wages, holdable).
narrative_ontology:cs_axiom_grounding('66905dad-2cb5-413b-9ffd-29c66fd4cb02', income_support_displaces_wages, empirically_contingent).
narrative_ontology:cs_axiom('66905dad-2cb5-413b-9ffd-29c66fd4cb02', foundational, labor_market_power_asymmetry).
narrative_ontology:cs_axiom_status(labor_market_power_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('66905dad-2cb5-413b-9ffd-29c66fd4cb02', labor_market_power_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('66905dad-2cb5-413b-9ffd-29c66fd4cb02', neoclassical_labor_market_equilibrium).
narrative_ontology:cs_drift_state('66905dad-2cb5-413b-9ffd-29c66fd4cb02', contemporary_labor_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66905dad-2cb5-413b-9ffd-29c66fd4cb02', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, state_fiscal_authorities).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a stable supply of labor willing to accept lower wages, as income support covers basic subsistence. This reduces pressure to increase wages or improve working conditions, effectively subsidizing their labor costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    institutional, biographical, arbitrage, national).

% Receive income support, which prevents destitution but simultaneously reduces their bargaining power. They remain in low-wage, often precarious, employment because the income support makes such work minimally viable, rather than enabling exit to better opportunities.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, local).

% Administer the income support program, which is presented as a social safety net. They benefit from reduced social unrest and a seemingly lower official poverty rate, while indirectly facilitating a low-wage economy. The fiscal burden is shifted to taxpayers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, state_fiscal_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Fund the income support program through taxes. From this reading's perspective, they are indirectly subsidizing low-wage employers by covering the difference between market wages and subsistence needs, without necessarily realizing this transfer.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Actively critique the income support system for its wage-suppressing effects, arguing it undermines collective bargaining and the fight for living wages. They are excluded from the policy's design process but engage in public advocacy and organizing.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the macroeconomic and social impacts of income support, often highlighting the unintended consequences of employer subsidy and wage suppression. They provide empirical and theoretical arguments that challenge the dominant narrative of the policy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, economists_critical_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a basic income floor to ensure citizen subsistence and maintain social stability, preventing widespread poverty and its associated social costs.
% TRANSFER_FUNCTION: Transfers public funds (collected from taxpayers) to individuals, which then indirectly transfers value to low-wage employers by enabling them to pay below-subsistence wages without facing labor shortages or social unrest.
% ABSENT_VOICES: Labor unions and advocates for living wages are structurally marginalized in the policy design, as their arguments for higher wages directly conflict with the implicit employer subsidy. They would demand policies that ensure wages cover living costs, rather than relying on public subsidies.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage employers would immediately face immense pressure to raise wages to attract and retain workers, or face severe labor shortages. Social unrest and widespread destitution among the working poor would dramatically increase, forcing a rapid reorganization of labor markets and social welfare systems.
% FOUNDING_PROBLEM: The policy was ostensibly built to address poverty, economic insecurity, and the challenges of a changing labor market (e.g., automation, precarious work) by providing a safety net.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some policymakers, think tanks) argue the founding problem of poverty and insecurity is still live. Critics (e.g., labor economists, social justice advocates, some unions) argue that while poverty persists, the policy's actual function has drifted to subsidize employers, citing empirical studies on wage stagnation and corporate profits as corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because a significant portion of the public funds intended for worker support is effectively captured by employers through wage suppression. Suppression is very high (0.85) as the policy actively suppresses wage growth and worker bargaining power, making exit from low-wage work less feasible. Theater ratio is moderate (0.40) because the policy does provide a genuine safety net function, but a substantial part of its operation serves the hidden function of employer subsidy. The increasing trend in extractiveness and suppression over time reflects the deepening entrenchment of this dynamic as the policy matures.
 *
 * PERSPECTIVAL GAP:
 *   The state and employers frame income support as a benevolent social policy or a necessary adjustment to market realities. Low-wage workers and critical economists, however, experience it as a system that entrenches precarity and transfers wealth upwards. The engine's computation of a 'tangled_rope' classification from the authored metrics, despite the 'rope' claim, captures this fundamental perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers are clear beneficiaries (d near 0.0) as they gain from reduced wage pressure. State fiscal authorities also benefit (d near 0.15) from social stability and reduced direct welfare costs. Low-wage workers are targets (d near 0.9) as their bargaining power is suppressed, and taxpayers are indirect targets (d near 0.7) as their funds subsidize employers. Labor unions and critical economists are observers/excluded, analyzing the system's true function.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a hybrid coordination/extraction mechanism as pure coordination. While income support provides a coordination function (social stability, basic needs), its substantial and increasing extractiveness, coupled with active enforcement (tax collection, distribution, wage market dynamics), reveals it as a Tangled Rope. The analysis highlights how the policy's mandate to alleviate poverty has been co-opted to subsidize employers, leading to a contested founding problem status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_elasticity_of_demand_for_labor,
    'To what extent do wages for low-skill labor actually adjust downwards in response to the introduction or expansion of unconditional income support?',
    'Empirical studies comparing wage trends in regions with and without such policies, controlling for other economic factors, or natural experiments from policy changes.',
    'If wages show high downward elasticity, it strongly supports the wage subsidy reading, increasing the measured extractiveness. If elasticity is low, the subsidy effect is weaker, and the policy leans more towards a pure safety net.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_elasticity_of_demand_for_labor, empirical, 'The degree to which income support influences wage levels.').

omega_variable(
    policy_intent_vs_outcome_divergence,
    'Is the observed wage suppression an intended consequence of the policy (e.g., to maintain labor supply for low-wage sectors), or an unintended, emergent property of the labor market?',
    'Analysis of legislative intent, policy documents, and public statements from proponents versus independent economic analysis of actual outcomes. This is a conceptual distinction about causality and responsibility.',
    'If intended, the policy''s claimed type shifts closer to a Snare (deliberate extraction). If unintended but emergent, it remains a Tangled Rope (hybrid function with emergent extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_vs_outcome_divergence, conceptual, 'Distinguishing deliberate policy design from emergent market effects.').

omega_variable(
    structural_vs_behavioral_suppression,
    'Is the suppression of worker bargaining power primarily due to structural changes in the labor market (e.g., increased labor supply at low wages) or behavioral changes by workers (e.g., reduced incentive to demand higher wages)?',
    'Sociological and psychological studies on worker agency and decision-making under income support, combined with labor market analyses of supply and demand shifts.',
    'If primarily structural, the constraint''s suppression is more deeply embedded and harder to counteract. If primarily behavioral, interventions targeting worker agency or information could be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_behavioral_suppression, empirical, 'Mechanism of worker bargaining power suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(inco_tr_t6, income_support_conditionality__wage_subsidy_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(inco_tr_t18, income_support_conditionality__wage_subsidy_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__wage_subsidy_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(inco_be_t6, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 6, 0.69).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(inco_be_t18, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(inco_su_t6, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(inco_su_t18, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_laws).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, labor_bargaining_power).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, social_safety_net_design).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'income_support_conditionality' kernel, focusing on its function as an employer wage subsidy. It is distinct from the 'freedom_floor_reading' (positive freedom) and 'dependency_trap_reading' (undermining work incentives), which analyze different structural effects of the same policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
