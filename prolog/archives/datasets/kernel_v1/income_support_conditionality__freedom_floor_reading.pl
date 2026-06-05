% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested kernel
 *   'income support conditionality' — the freedom floor reading.
 *   Unconditional income support is understood as a mechanism that
 *   decommodifies labor power by providing genuine exit options from coercive
 *   labor arrangements. In this reading, the constraint is fundamentally
 *   about enabling workers to refuse exploitative conditions without facing
 *   destitution. The structural claim is that labor coercion (the employer's
 *   ability to suppress wages through threat of absolute deprivation) is not
 *   a natural law or necessary feature of labor markets, but a contingent
 *   institutional arrangement that can be disrupted through income support.
 *   The freedom floor reading produces a rope classification: the constraint
 *   solves a collective action problem (labor market stability, worker
 *   dignity) with minimal coercive overhead and without extracting
 *   asymmetrically from identifiable agents. This contrasts sharply with the
 *   dependency trap reading (which sees income support as creating
 *   disincentives to work and long-term dependency) and the wage subsidy
 *   reading (which sees income support as allowing employers to suppress
 *   wages while the state subsidizes worker survival). These are not
 *   disagreements about facts alone — they are disputes over which
 *   institutional commitment (freedom from coercion, work incentive
 *   preservation, or labor market efficiency) should ground policy
 *   legitimacy. The kernel itself (the formalized commitment to provide
 *   income support) is formally specified but subject to fundamentally
 *   different interpretations.
 *
 * KEY AGENTS:
 *   - Precarious workers (primary beneficiary, moderate/mobile): gain genuine exit option from coercive wage suppression; experience constraint as enabling coordination
 *   - Low-wage employers (power erosion target, institutional/constrained): lose coercive firing and wage-setting capacity; experience constraint as external extraction of previous privilege
 *   - Excluded labor market participants (beneficiary, powerless/mobile): gain capacity for partial labor market participation without total commodification; experience constraint as barrier removal
 *   - Welfare state administrative system (coordination provider, institutional/arbitrage): solves collective action problems of labor instability; experiences constraint as coordination mechanism with net social benefit
 *   - Dependency trap advocates (competing reading authority, analytical): hold that work incentives are the foundational commitment and income support violates this
 *   - Wage subsidy advocates (competing reading authority, analytical): hold that labor market efficiency is the foundational commitment and income support distorts wage signals
 *   - Freedom floor advocates (this reading's authority, analytical): hold that positive freedom (capacity to refuse coercion) is the foundational commitment and income support protects this
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Labor Decommodification (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'ece425c3-5686-42ef-8632-12dd544a6075').
narrative_ontology:cs_kernel_codification('ece425c3-5686-42ef-8632-12dd544a6075', formalized).
narrative_ontology:cs_authority_grounding('ece425c3-5686-42ef-8632-12dd544a6075', lineage).
narrative_ontology:cs_interpretation_layer_present('ece425c3-5686-42ef-8632-12dd544a6075').
narrative_ontology:cs_reading_relation('ece425c3-5686-42ef-8632-12dd544a6075', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('ece425c3-5686-42ef-8632-12dd544a6075', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('ece425c3-5686-42ef-8632-12dd544a6075', foundational, positive_freedom_from_coercion_foundational).
narrative_ontology:cs_axiom_status(positive_freedom_from_coercion_foundational, holdable).
narrative_ontology:cs_axiom_grounding('ece425c3-5686-42ef-8632-12dd544a6075', positive_freedom_from_coercion_foundational, deontological).
narrative_ontology:cs_axiom('ece425c3-5686-42ef-8632-12dd544a6075', foundational, labor_coercion_contingent_not_natural).
narrative_ontology:cs_axiom_status(labor_coercion_contingent_not_natural, holdable).
narrative_ontology:cs_axiom_grounding('ece425c3-5686-42ef-8632-12dd544a6075', labor_coercion_contingent_not_natural, deontological).
narrative_ontology:cs_reference_frame('ece425c3-5686-42ef-8632-12dd544a6075', decommodified_labor_market).
narrative_ontology:cs_drift_state('ece425c3-5686-42ef-8632-12dd544a6075', contemporary_neoliberal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ece425c3-5686-42ef-8632-12dd544a6075', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, excluded_labor_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (ROPE) — Unconditional income support provides genuine exit option from coercive labor arrangements. Worker can refuse exploitative conditions without destitution. The constraint is pure coordination: income support enables negotiation on terms of work rather than suppressing alternatives. No victim status — worker's structural position improved by constraint's existence.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-WAGE EMPLOYER (SNARE) — Experiences income support as constraint on labor market coercion. Previously could extract labor through wage suppression (threat of destitution if worker refuses). Now faces worker mobility and wage pressure. Employer experiences extraction: coercive firing and wage-setting power are reduced. However, this is extraction FROM the employer (loss of previous privilege), not extraction OF the employer. Classification reflects employer's reduced capacity to coerce, not the employer's victimization.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE STATE COORDINATION (ROPE) — The constraint is fundamentally coordinative from the state's administrative perspective. Income support solves the collective action problem of labor market instability: prevents poverty-driven desperation (social unrest, health crises, crime), enables skill investment and geographic mobility, reduces administrative costs of poverty management. Pure coordination with minimal coercive overhead. No extractive mechanism from this perspective — the system redistributes without asymmetric gain to the state.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXCLUDED PARTICIPANT (ROPE) — Populations structurally unable to participate in formal labor markets (caregivers, disabled workers, rural populations with limited employment) gain capacity to participate on terms that do not require total commodification of their time. Income support enables partial labor market participation, skill development, or non-market contribution (care work, community service) without destitution. Pure coordination benefit with no extraction mechanism — the constraint dissolves structural barriers.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational scope, unconditional income support represents a temporary coordination mechanism with a sunset condition. If labor market institutions restructure to eliminate coercive wage suppression on their own (through full-employment policies, strong collective bargaining, or alternative work arrangements), the decommodification function becomes redundant and the income support transfers into pure redistribution. The scaffold classification reflects that this reading sees the constraint as solving a transitional problem in labor commodification, with an exit condition when structural labor market dynamics change.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL FREEDOMS (MOUNTAIN) — From a philosophical perspective grounded in freedom theory, the constraint appears as an immutable property of human dignity: the capacity to refuse coercive arrangements is a foundational human freedom, not a contingent policy choice. Income support merely makes manifest a freedom that should always have existed. This perspective risks naturalizing what is actually a contingent institutional choice about labor market structure. The engine's false summit detector will flag this as a candidate for FSM evaluation.
constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_conditionality__freedom_floor_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18, declining from 0.35): This reading frames income support as decommodifying labor rather than extracting. The metric is low because the reading identifies zero victim groups — no agent bears extractive cost in this framing. The declining trajectory (0.35→0.18 over the interval) models institutional maturation: as income support norms consolidate and employer adaptation occurs, the coercive labor market dynamics that income support disrupts begin to dissolve (wages adjust upward under worker bargaining power, job quality improves). The initial higher value (0.35) represents the transition cost to employers and the institutional friction of norm-change; the terminal value (0.18) represents the new equilibrium where workers' exit option becomes normal and extraction through coercion drops to background level. Suppression (0.12, declining from 0.25): Very low because the reading explicitly identifies income support as reducing suppression. The declining trajectory models progressive norm internalization: initial pushback against income support (welfare stigma, policy resistance) declines as the policy becomes established and workers' behavioral adaptation occurs. Theater ratio (0.35, declining from 0.55): Moderate-low, declining. In this reading, income support is substantive (genuine exit option) rather than performative. The initial theater ratio reflects administrative performance and political ritual around income support implementation; as the mechanism matures and becomes normalized, the theatrical component (political debate, administrative verification) declines and the functional component (redistribution, exit option provision) dominates.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap appears between the low-wage employer (snare, constrained exit) and precarious worker (rope, mobile exit). The employer experiences the constraint as loss of previous capacity (which the snare classification captures), while the worker experiences it as gain of new capacity (which the rope classification captures). Both are empirically accurate from their respective positions — the constraint does reduce employer coercive power AND does increase worker bargaining power. The gap reveals that the same institutional mechanism can be simultaneously liberating (from worker perspective) and restrictive (from employer perspective). The analytical observer's scaffold perspective introduces temporal dimension: if labor market restructuring eventually eliminates the need for income support (through other mechanisms), the constraint becomes transitional. The mountain perspective attempts to naturalize positive freedom as an immutable property, but the structural data contradicts this — income support is a contingent institutional choice, not a law of nature. The gap between rope/scaffold perspectives (all positive or neutral) and the mountain perspective (all perspectives converge on mountain type, suggesting invariance) reveals that this reading does not support the mountain classification from structural grounds alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Direction from structurally positive to worker-benefit: In this reading, beneficiaries are clearly identified (precarious workers, excluded participants) and no victims exist in the system. This produces very low directionality (d near 0.0) from the worker perspective and directional inversion from the employer perspective. The employer experiences loss of coercive power (previous privilege eroded), which the sigmoid function treats as directionality toward the employer (d near 1.0 for employer), BUT this is loss of extractive capacity, not victimization — the employer is not extracted FROM; rather, the employer's previous extraction mechanism (coercive wage suppression) is blocked. The analytical observer derives d from the structural fact that no agent is systematically harmed by income support provision in this reading — the mechanism is purely coordinative. The power atoms (moderate, institutional, powerless, analytical) are assigned based on agents' capacity to shape policy and labor market dynamics; exit options (mobile, arbitrage, analytical) reflect capacity to leave or opt out of the constraint. Workers gain mobile exit because income support provides alternative to wage dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by explicitly choosing which commitment (positive freedom from coercion) grounds policy legitimacy, rather than trying to optimize work incentives or labor market efficiency simultaneously. The reading accepts that different readings prioritize different commitments and occupy different institutional authorities — labor rights traditions emphasize freedom; welfare economics emphasizes efficiency; developmentalism emphasizes skill investment. By making positive freedom the explicit kernel commitment, the freedom floor reading clarifies what the constraint is protecting and accepts that other readings (dependency trap, wage subsidy) prioritize different commitments. This is not a resolution through empirical fact (all three readings face real empirical questions about labor supply elasticity, wage response, etc.), but through institutional alignment: which authority gets to define what income support is for? The freedom floor reading answers: positive freedom (capacity to refuse coercive arrangements). Under this answer, the other readings become misframings rather than alternative truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_incentive_vs_freedom_threshold,
    'At what income support level does decommodification begin (worker has genuine exit option) versus dependency trap begins (work disincentive dominates)?',
    'Empirical labor supply elasticity studies; comparison of wage/employment outcomes across income support generosity levels; longitudinal tracking of career investment and skill accumulation by beneficiary cohorts',
    'If threshold is low (10-20% of median wage): freedom reading dominates — income support enables exit. If threshold is high (40%+ of median wage): dependency trap reading gains credibility — income support suppresses work incentives. If no stable threshold (outcome varies by individual, skill, context): both readings coexist and the kernel allows multiple framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_incentive_vs_freedom_threshold, empirical, 'Labor supply elasticity threshold distinguishing decommodification from disincentive').

omega_variable(
    employer_wage_response_mechanism,
    'Do employers respond to income support by reducing wages (wage subsidy reading), maintaining wages despite lost coercive power (freedom reading), or adjusting job composition/automation (alternative mechanism)?',
    'Comparative wage analysis in jurisdictions with/without income support; matched-pair studies controlling for sector, skill level, labor market conditions; analysis of labor-demand changes (hours offered, job quality, automation investment)',
    'If wage suppression dominant: wage_subsidy_reading correct — income support subsidizes employers. If wages stable despite lost coercion: freedom_floor_reading correct — decommodification genuine. If automation/job displacement dominant: neither reading captures mechanism; constraint classification shifts to infrastructure/employment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_wage_response_mechanism, empirical, 'Employer response to income support: wage suppression, maintenance, or job destruction').

omega_variable(
    labor_market_coercion_baseline,
    'What is the counterfactual labor market structure absent income support? Does income support create exit options (freedom reading) or merely reduce severity of existing coercion (dependency trap reading with qualification)?',
    'Historical analysis of labor market coercion mechanisms pre-income support; comparative institutional analysis across countries with/without income support; ethnographic studies of worker decision-making under threat of destitution',
    'If baseline includes absolute destitution threat: income support creates qualitatively new exit capacity (freedom reading confirmed). If baseline includes partial safety nets (unemployment insurance, food assistance): income support''s marginal decommodification effect is smaller (wage subsidy or dependency trap readings gain relative weight).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_coercion_baseline, empirical, 'Counterfactual labor market coercion baseline').

omega_variable(
    kernel_identity_ambiguity,
    'Is the kernel ''income support'' vs ''labor coercion'' OR is it ''work incentives'' vs ''human dignity''? Different kernel identities produce different reading structures.',
    'Analyze which institution or norm bears authority for the kernel definition. If labor market efficiency doctrines dominate (employer authority), kernel is framed as work-incentive question (dependency trap reading). If human-rights traditions dominate (international labor conventions), kernel is framed as dignity/freedom question (freedom floor reading). If mixed authority, kernel remains genuinely ambiguous.',
    'If authority is ambiguous, all three readings remain live in different institutional contexts. If authority consolidates, one reading gains institutional legitimacy and others shift to minority positions. Classification outcomes depend partly on this meta-kernel about what the contested kernel is.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Meta-kernel ambiguity: is the kernel about work incentives or human dignity?').

omega_variable(
    reading_change_via_policy_design,
    'Can policy design choices (conditionality, benefit duration, eligibility criteria) shift which reading is structurally correct without changing the underlying economic reality?',
    'Policy mechanism analysis: does adding work requirements change labor supply elasticity? Does time-limiting benefits change incentive structures? Does means-testing change exit option availability? Distinguish between reading-change via design versus reading-change via actual behavioral/economic outcome shift.',
    'If policy design can shift reading without changing outcomes: readings are partly performative (policy choice about which constraint narrative dominates). If outcomes change first and readings follow: readings track real structural changes. This affects whether the kernel framing is genuinely contested or merely rhetorically contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_change_via_policy_design, empirical, 'Whether policy design choices can shift reading without changing outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incsupp_ff_theater_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(incsupp_ff_theater_t5, income_support_conditionality__freedom_floor_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(incsupp_ff_theater_t10, income_support_conditionality__freedom_floor_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(incsupp_ff_extract_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(incsupp_ff_extract_t5, income_support_conditionality__freedom_floor_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(incsupp_ff_extract_t10, income_support_conditionality__freedom_floor_reading, base_extractiveness, 10, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(incsupp_ff_suppress_t0, income_support_conditionality__freedom_floor_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(incsupp_ff_suppress_t5, income_support_conditionality__freedom_floor_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(incsupp_ff_suppress_t10, income_support_conditionality__freedom_floor_reading, suppression_requirement, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, labor_market_coercion_mechanism).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, poverty_driven_wage_suppression).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories instantiating different readings of the 'income_support_conditionality' kernel. Each reading produces a different classification type and different beneficiary/victim structure from the same base policy. The freedom_floor_reading (this story) produces rope classification with workers as beneficiaries. The dependency_trap_reading produces snare with workers as victims. The wage_subsidy_reading produces tangled_rope with workers as both beneficiaries (survive) and victims (trapped in low-wage subsidy). All three share the same base institution (income support) but structure it through fundamentally different institutional commitments. Link all three stories with network.affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
