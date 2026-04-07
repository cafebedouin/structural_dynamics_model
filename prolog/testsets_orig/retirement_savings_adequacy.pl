% ============================================================================
% CONSTRAINT STORY: retirement_savings_adequacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_retirement_savings_adequacy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: retirement_savings_adequacy
 *   human_readable: Retirement Savings Adequacy Constraint
 *   domain: financial/economic/social
 *
 * SUMMARY:
 *   The retirement savings adequacy constraint structures the relationship
 *   between working-age populations and the financial institutions managing
 *   their deferred consumption. The constraint functions simultaneously as
 *   coordination mechanism (enabling long-term capital formation and deferred
 *   consumption) and extraction mechanism (concentrating returns and risk in
 *   institutions while shifting longevity and market risk to individuals).
 *   Extractiveness has increased over the interval as real wages stagnated,
 *   fee structures proliferated, defined benefit pensions were terminated,
 *   and life expectancy extended — while required savings levels rose. The
 *   constraint exhibits all six classification types from different
 *   perspectives, revealing a fundamental tension: the coordination function
 *   is genuine and necessary, but the distribution of costs and benefits is
 *   strongly asymmetric. Theater ratio reflects the proliferation of
 *   actuarial and financial complexity that obscures the underlying
 *   simplicity: populations must fund their non-working years. The gap
 *   between coordination necessity and extraction mechanism reveals itself
 *   when comparing international systems with lower costs and better
 *   outcomes.
 *
 * KEY AGENTS:
 *   - Low-income workers: Primary victim (powerless/trapped) — inadequate wages preclude adequate savings; no alternative pathway to retirement security
 *   - Gig and contingent workers: Primary victim (powerless/trapped) — excluded from employer-based savings mechanisms; face full longevity and market risk
 *   - Middle-income salaried workers: Secondary victim (moderate/constrained) — benefit from employer matching and forced savings discipline but face fee extraction and market concentration risk
 *   - Financial services industry: Primary beneficiary (institutional/arbitrage) — captures management fees, service charges, and investment returns on managed assets
 *   - Asset management firms: Primary beneficiary (institutional/arbitrage) — profit from mandatory flow of retirement savings into equity and bond markets
 *   - Pension reform coalition: Organized agents (organized/constrained) — advocate for public pension expansion and portable benefits; face political barriers
 *   - Defined benefit pension sponsors: Institutional actor (institutional/arbitrage) — shifted risk to defined contribution as cost-control mechanism
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing distributional choices as actuarial inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(retirement_savings_adequacy, 0.58).
domain_priors:suppression_score(retirement_savings_adequacy, 0.65).
domain_priors:theater_ratio(retirement_savings_adequacy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(retirement_savings_adequacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(retirement_savings_adequacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(retirement_savings_adequacy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(retirement_savings_adequacy, tangled_rope).
narrative_ontology:human_readable(retirement_savings_adequacy, "Retirement Savings Adequacy Constraint").
narrative_ontology:topic_domain(retirement_savings_adequacy, "financial/economic/social").

domain_priors:requires_active_enforcement(retirement_savings_adequacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(retirement_savings_adequacy, financial_services_industry).
narrative_ontology:constraint_beneficiary(retirement_savings_adequacy, asset_management_firms).
narrative_ontology:constraint_beneficiary(retirement_savings_adequacy, pension_plan_administrators).
narrative_ontology:constraint_victim(retirement_savings_adequacy, low_income_workers).
narrative_ontology:constraint_victim(retirement_savings_adequacy, gig_workers).
narrative_ontology:constraint_victim(retirement_savings_adequacy, early_career_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME WORKER (SNARE) — Trapped by inadequate wage levels and mandatory retirement contribution mechanisms. Cannot exit the savings regime through wage negotiation or alternative employment. Bears full extraction cost as savings reduce immediate consumption without guarantee of adequate retirement income. Maximum suppression through lack of alternatives.
constraint_indexing:constraint_classification(retirement_savings_adequacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME SALARIED WORKER (TANGLED ROPE) — Experiences both genuine coordination (retirement planning enables long-term security, employer matching provides real benefit) and asymmetric extraction (fee structures, market risk concentration, inflation erosion). Constrained by high costs of not saving while facing significant extraction within the savings mechanism itself.
constraint_indexing:constraint_classification(retirement_savings_adequacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SERVICES INDUSTRY (ROPE) — Primary beneficiary. Experiences constraint as pure coordination mechanism: enabling workers to save creates asset bases for investment management, fee extraction, and financial system growth. Benefits from mandatory contribution flows and regulatory compliance requirements. High exit optionality through arbitrage across markets and products.
constraint_indexing:constraint_classification(retirement_savings_adequacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PENSION REFORM COALITION (SCAFFOLD) — Organized agents (consumer advocacy groups, policy reformers, social security expansion advocates) see the current savings adequacy regime as a temporary coordination failure with a sunset. Alternative mechanisms (expanded social security, public pension funds, portable benefits) represent pathways to lower-extraction retirement security. Constrained but organized with exit path visible.
constraint_indexing:constraint_classification(retirement_savings_adequacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFINED BENEFIT PENSION SYSTEM (LEGACY) (PITON) — Traditional employer pensions represent a largely degraded coordination mechanism, maintained through institutional inertia rather than function. Most private sector DB plans are frozen or terminated; the remnant public sector plans operate at unsustainable cost. Theater ratio reflects that pension actuarialism and solvency calculations are complex rituals obscuring the underlying shift of retirement risk to workers. Piton classification derives from high theater ratio despite moderate extraction.
constraint_indexing:constraint_classification(retirement_savings_adequacy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, retirement security constraints reflect an immutable natural limit: the actuarial reality of human longevity exceeding working years. All societies face the problem of sustaining non-working populations. This perspective sees retirement adequacy as a structural inevitability rather than an institutional extraction. However, the constraint operates at high extraction and suppression values despite this characterization — suggesting the 'natural' framing naturalizes what is actually a contingent distributional choice.
constraint_indexing:constraint_classification(retirement_savings_adequacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(retirement_savings_adequacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(retirement_savings_adequacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(retirement_savings_adequacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(retirement_savings_adequacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(retirement_savings_adequacy, TR),
    TR >= 0.70.

:- end_tests(retirement_savings_adequacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from workers through fee structures, investment risk concentration, and the shift of longevity risk to individuals. However, it is not maximal extraction because genuine coordination benefits exist: enforced savings discipline produces capital accumulation that individuals would not achieve independently, and employer matching provides real wealth transfers. The 0.58 value reflects the upward drift over the interval as retirement system costs increased faster than wages. Suppression (0.65): Moderate-high. Workers face substantial barriers to exiting the savings regime through lack of alternative pathways, income inadequacy to self-insure, and institutional lock-in (vesting schedules, tax penalties, employer-based system architecture). However, suppression is not total: some workers can accumulate through homeownership, social security provides a baseline, and portable savings accounts exist. Theater ratio (0.58): Moderate. The proliferation of retirement planning complexity (asset allocation strategies, tax optimization, required minimum distributions, actuarial calculations) creates significant performative activity. However, the underlying coordination function (moving income from high-earning to low-earning years) is genuinely necessary. Theater has increased as complexity increased and outcomes diverged from promises.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural arrangement (mandatory savings coordination) classifies as six distinct types. The financial industry sees Rope (pure coordination enabling capital flows). The reform coalition sees Scaffold (a temporary institution with a sunset as public pensions expand). Legacy DB pensions see Piton (institutional inertia, degraded function, high theater). Middle-income workers see Tangled Rope (real coordination benefits plus significant extraction). Low-income workers see Snare (pure extraction with no exit). The analytical observer risks seeing Mountain (actuarial inevitability) but the structural data reveals false summit: the current extraction and theater levels are contingent institutional choices, not laws of nature. The gap between beneficiary (Rope) and victim (Snare) perspectives is the maximum possible in this system.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial services industry occupies the institutional/arbitrage position: they benefit from the constraint's existence, experience it as pure coordination (managing savings flows), and can exit through arbitrage across products and markets. Their directionality d is low (approximately 0.10-0.20), producing negative or near-zero chi — they perceive minimal extraction because extraction runs toward them. Low-income workers occupy the powerless/trapped position: they bear the constraint's full cost, cannot exit, and benefit minimally. Their d is high (approximately 0.90+), producing high chi — maximum experienced extraction. Middle-income workers with constrained exit face intermediate d (approximately 0.55-0.70), producing moderate chi. The organized reform coalition occupies the organized/constrained position: they have agency and see an alternative pathway (public pension expansion), producing moderate d (approximately 0.45-0.55) and moderate chi despite nominal organization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The retirement savings constraint resolves the mandatrophy by separating the coordination function (genuine and necessary) from the distribution of costs and benefits (contingent and extractive). The constraint is a Tangled Rope at the analytical level: it both coordinates deferred consumption (essential) and extracts value asymmetrically (contingent). The false summit (Mountain from analytical perspective) arises when the coordination necessity is conflated with the current distributional mechanism. Alternative coordination mechanisms (expanded social security, public pension funds, portable defined contribution plans with lower fee structures) demonstrate that the coordination function can be delivered at lower extraction. The mandatrophy is not 'is this coordination or extraction?' but 'under what distributional terms should coordination occur?' The current regime naturalizes its particular distributional choice as inevitable, which is the defining move of false summit reasoning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replacement_income_threshold,
    'What replacement income ratio (percentage of pre-retirement income needed in retirement) represents structural necessity versus institutional preference?',
    'Cross-national comparison of retirement income adequacy standards; correlation between stated replacement ratios and actual adequacy outcomes; analysis of consumption pattern shifts in retirement',
    'If structural necessity is lower than current standard (70-80%): extraction is being masked as requirement. If structural necessity is higher: current regime is inadequate by its own measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_income_threshold, empirical, 'Threshold for distinguishing necessity from institutional preference in replacement income standards').

omega_variable(
    market_return_assumption_dependency,
    'To what extent does retirement adequacy depend on assumptions about future asset market returns that are substantially above historical real returns?',
    'Stress-testing retirement savings models with conservative return assumptions (2-3% real vs historical 5-7%); analysis of adequacy trajectories under different market scenarios',
    'If current adequacy relies on above-historical returns: the extraction mechanism depends on risk transfer to workers. If adequacy holds under conservative assumptions: extraction is genuinely moderated by coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_return_assumption_dependency, empirical, 'Dependency of adequacy on market return assumptions').

omega_variable(
    wage_stagnation_coupling,
    'Is the retirement savings adequacy crisis fundamentally coupled to real wage decline, or is it a separate extraction mechanism operating on stable wages?',
    'Decomposition of inadequacy drivers: proportion attributable to wage decline vs increased fees vs increased life expectancy vs decreased employer contributions',
    'If coupled to wage decline: the constraint may dissolve if wages recover; extraction is indirect. If separate: extraction is direct and requires separate policy intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_stagnation_coupling, empirical, 'Whether adequacy crisis couples to wage stagnation or operates independently').

omega_variable(
    public_pension_substitution_feasibility,
    'Could expanded social security or public pension systems deliver adequate retirement security at lower cost than privatized savings mechanisms?',
    'International benchmarking of public vs privatized system costs and outcomes; administrative cost analysis; risk-pooling efficiency comparison',
    'If yes: the current regime''s extraction is contingent on institutional choice, not necessity; scaffold sunset is real. If no: current mechanisms represent minimum-cost coordination despite high apparent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_pension_substitution_feasibility, conceptual, 'Feasibility of public pension substitution for private savings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(retirement_savings_adequacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retire_tr_t0, retirement_savings_adequacy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(retire_tr_t10, retirement_savings_adequacy, theater_ratio, 10, 0.52).
narrative_ontology:measurement(retire_tr_t20, retirement_savings_adequacy, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(retire_be_t0, retirement_savings_adequacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(retire_be_t10, retirement_savings_adequacy, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(retire_be_t20, retirement_savings_adequacy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(retirement_savings_adequacy, resource_allocation).
narrative_ontology:affects_constraint(retirement_savings_adequacy, wage_stagnation_crisis).
narrative_ontology:affects_constraint(retirement_savings_adequacy, financial_services_fee_extraction).
narrative_ontology:affects_constraint(retirement_savings_adequacy, longevity_risk_transfer).

% DUAL FORMULATION NOTE:
% Retirement savings adequacy should be decomposed into three structurally distinct constraints: (1) deferred consumption coordination (genuine coordination, low extraction), (2) longevity risk distribution (mixed coordination/extraction), (3) financial services fee structure (pure extraction). The current story treats all three as a single constraint. Future decomposition recommended per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(retirement_savings_adequacy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
