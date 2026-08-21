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
 *   This constraint describes unconditional income support (e.g., UBI) as a
 *   mechanism that, rather than empowering workers, functions as an employer
 *   subsidy. By providing a baseline income, it allows employers to pay wages
 *   below subsistence levels, effectively capturing the subsidy and
 *   suppressing overall wage growth for low-wage workers. The constraint is
 *   claimed as a Tangled Rope because it has a genuine coordination function
 *   (preventing destitution) but also an asymmetric extractive component
 *   (subsidizing employers at the expense of worker bargaining power). This
 *   is one reading of the 'income_support_conditionality' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.75).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '158cd883-9e13-418c-a1a3-9686cf394615').
narrative_ontology:cs_kernel_codification('158cd883-9e13-418c-a1a3-9686cf394615', formalized).
narrative_ontology:cs_authority_grounding('158cd883-9e13-418c-a1a3-9686cf394615', lineage).
narrative_ontology:cs_interpretation_layer_present('158cd883-9e13-418c-a1a3-9686cf394615').
narrative_ontology:cs_reading_relation('158cd883-9e13-418c-a1a3-9686cf394615', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('158cd883-9e13-418c-a1a3-9686cf394615', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('158cd883-9e13-418c-a1a3-9686cf394615', foundational, labor_market_power_asymmetry).
narrative_ontology:cs_axiom_status(labor_market_power_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('158cd883-9e13-418c-a1a3-9686cf394615', labor_market_power_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('158cd883-9e13-418c-a1a3-9686cf394615', foundational, income_support_as_wage_floor_not_exit_ramp).
narrative_ontology:cs_axiom_status(income_support_as_wage_floor_not_exit_ramp, holdable).
narrative_ontology:cs_axiom_grounding('158cd883-9e13-418c-a1a3-9686cf394615', income_support_as_wage_floor_not_exit_ramp, empirically_contingent).
narrative_ontology:cs_reference_frame('158cd883-9e13-418c-a1a3-9686cf394615', post_industrial_welfare_state).
narrative_ontology:cs_drift_state('158cd883-9e13-418c-a1a3-9686cf394615', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('158cd883-9e13-418c-a1a3-9686cf394615', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, government_agencies).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a reduced need to pay subsistence wages, as the income support covers the gap. This allows them to maintain lower labor costs and higher profit margins, effectively capturing the subsidy intended for workers. They face no direct costs from the income support program.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    institutional, biographical, arbitrage, national).

% Receive income support that prevents destitution but find their wages suppressed by employers who factor in the subsidy. This traps them in low-wage jobs, as the income support is not enough to enable true exit from the labor market, but rather subsidizes their continued participation at suppressed rates. Their 'benefit' is subsistence, but at the cost of wage growth.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, local).

% Administer the income support program, justifying it as a social safety net and poverty reduction measure. They bear the administrative costs and political pressure, but also benefit from social stability and reduced visible poverty, which can mask underlying wage stagnation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Fund the income support program through taxes. They are told the program supports the poor, but under this reading, they are indirectly subsidizing low-wage employers. Their exit options are limited to political action or emigration.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers, payer,
    organized, biographical, mobile, national).

% Analyze the effects of income support, often arguing that it fails to empower workers and instead props up exploitative labor practices. They advocate for higher minimum wages or more robust labor protections to prevent wage suppression.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, advocacy_groups_for_workers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a baseline income floor that ensures worker subsistence, preventing widespread destitution and social unrest, and stabilizing the low-wage labor market by ensuring a supply of workers.
% TRANSFER_FUNCTION: Transfers public funds (from taxpayers) to low-wage workers, which then indirectly flows to low-wage employers by enabling them to pay below-subsistence wages.
% ABSENT_VOICES: Workers who might otherwise demand higher wages or seek alternative employment if not for the income support, which, under this reading, reduces their bargaining power. Also, economists who advocate for direct wage interventions rather than indirect subsidies.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, low-wage workers would face immediate destitution, leading to increased social instability and pressure for employers to raise wages to subsistence levels, or for the government to implement other forms of social assistance. The low-wage labor market would be significantly disrupted.
% FOUNDING_PROBLEM: To alleviate poverty, reduce social inequality, and provide a safety net for individuals facing economic hardship or unemployment.
% FOUNDING_PROBLEM_CORROBORATION: Government agencies and some social welfare advocates attest the problem is live. Labor economists and worker advocacy groups, from outside the benefiting parties, argue that while poverty remains, the income support mechanism has been co-opted, and its original problem-solving function is now distorted by its wage-suppressing effect.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) reflects the degree to which the subsidy is captured by employers through wage adjustments, rather than accruing to workers as increased disposable income or bargaining power. Suppression (0.75) is high because workers' exit options from low-wage labor are constrained by the very mechanism meant to support them; the income floor reduces the urgency for employers to raise wages. The theater ratio (0.20) is low, indicating that the program genuinely provides a safety net, but a significant portion of its stated 'empowerment' function is performative, masking its role in wage suppression. The metrics show a gradual increase in extractiveness and suppression over time as the market adjusts to the presence of the income support.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of low-wage employers, the income support is a beneficial stabilization of the labor market. From the perspective of low-wage workers, it's a trap that institutionalizes their precarious position. Government agencies may see it as a successful social safety net, while critics (like advocacy groups) see it as a failed intervention that entrenches existing power imbalances. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers are clear beneficiaries (d=0.0-0.1) as their labor costs are reduced. Low-wage workers are victims (d=0.7-0.8) as their wages are suppressed, despite receiving the income support. Government agencies are agenda-setters (d=0.4-0.5), balancing social stability with the unintended economic consequences. Taxpayers are payers (d=0.6-0.7) funding a system that, under this reading, benefits employers more than intended.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure 'Rope' (genuine coordination) or 'Scaffold' (temporary support for transition). By highlighting the employer subsidy aspect, it identifies the mechanism by which the coordination function (subsistence) is coupled with asymmetric extraction (wage suppression), characteristic of a Tangled Rope. The founding problem (alleviating poverty) is contested because, while poverty persists, the mechanism's effect on wages means it's not solving the problem in the way originally intended, but rather creating new forms of dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_elasticity_of_income_support,
    'To what extent do employers actually adjust wages downward in response to the introduction or increase of unconditional income support?',
    'Empirical studies comparing wage trends in regions with and without unconditional income support, controlling for other economic factors. Microeconomic analysis of firm-level wage-setting behavior.',
    'If wage elasticity is high, the extractiveness of this constraint is higher than estimated, strengthening its Tangled Rope classification. If elasticity is low, the employer subsidy effect is weaker, pushing the constraint closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_elasticity_of_income_support, empirical, 'Empirical measurement of wage suppression due to income support.').

omega_variable(
    worker_bargaining_power_impact,
    'Does unconditional income support genuinely increase worker bargaining power by enabling them to refuse exploitative work, or does it reduce it by institutionalizing low wages?',
    'Sociological studies on worker agency and labor market participation, qualitative interviews with low-wage workers, and analysis of unionization rates and strike activity in affected sectors.',
    'If bargaining power increases, the ''freedom_floor_reading'' gains strength, and this ''wage_subsidy_reading'' would be partially foreclosed. If it decreases, this reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_bargaining_power_impact, empirical, 'Impact of income support on worker agency and labor market leverage.').

omega_variable(
    framing_of_income_support_purpose,
    'Is the primary purpose of income support to provide a ''freedom floor'' (as per the freedom_floor_reading) or to prevent social instability by ensuring subsistence (as per this wage_subsidy_reading''s implicit function)?',
    'Analysis of legislative intent, public discourse, and policy outcomes. This is a conceptual choice about the constraint''s ''true'' function.',
    'If the ''freedom floor'' framing is adopted, the constraint''s classification shifts towards a Rope, as the extractive element becomes an unintended side effect. If the ''subsistence-only'' framing is adopted, this reading is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_income_support_purpose, conceptual, 'Conceptual framing of the core purpose of unconditional income support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__wage_subsidy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__wage_subsidy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
