% ============================================================================
% CONSTRAINT STORY: uk_help_to_buy_scheme
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_help_to_buy_scheme, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: uk_help_to_buy_scheme
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic
 *
 * SUMMARY:
 *   The UK 'Help to Buy' Equity Loan Scheme (2013-2023) aimed to assist
 *   first-time buyers in purchasing new-build homes by providing government
 *   equity loans. The scheme's impact is multifaceted, with both positive and
 *   negative consequences for various stakeholders. Builders benefited from
 *   increased demand, while first-time buyers faced potential debt burdens.
 *   Taxpayers bore the risk of loan defaults, and the scheme's long-term
 *   effects on housing affordability remain uncertain.
 *
 * KEY AGENTS:
 *   - First-Time Buyers: Primary target (powerless/trapped) - incentivized to enter the market with limited savings.
 *   - New Home Builders: Primary beneficiary (institutional/arbitrage) - increased demand and sales volumes.
 *   - UK Taxpayers: Secondary target (moderate/constrained) - bear the risk of loan defaults.
 *   - Mortgage Lenders: Secondary beneficiary (institutional/mobile) - increased lending activity.
 *   - Government Housing Policy: (Institutional/Analytical) - attempting to stimulate economy through housing market
 *   - Analytical Observer: Global perspective (analytical/analytical) - assesses the scheme's overall impact and sustainability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_help_to_buy_scheme, 0.55).
domain_priors:suppression_score(uk_help_to_buy_scheme, 0.45).
domain_priors:theater_ratio(uk_help_to_buy_scheme, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, extractiveness, 0.55).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_help_to_buy_scheme, tangled_rope).
narrative_ontology:human_readable(uk_help_to_buy_scheme, "UK 'Help to Buy' Equity Loan Scheme").
narrative_ontology:topic_domain(uk_help_to_buy_scheme, "economic").

domain_priors:requires_active_enforcement(uk_help_to_buy_scheme).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, new_home_builders).
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, mortgage_lenders).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, first_time_buyers).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, uk_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% First-time buyers with limited savings are highly incentivized (almost 'trapped') into using the scheme to get on the property ladder. They face high repayment costs relative to income if property values stagnate or fall, especially in later years of the loan. Limited exit options once committed.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% New home builders benefited from increased demand and sales volumes due to the scheme. They were able to sell properties more quickly and at higher prices than they might have otherwise. They have arbitrage exit options given market incentives.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% UK Taxpayers bear the risk of the government's equity loans not being repaid in full if property values fall significantly. While the scheme aimed to stimulate the housing market, taxpayers are constrained by the potential financial liability if the housing market underperforms. Also, they benefit indirectly from the increased housing supply. Moderate power due to election process.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Help to Buy scheme, having ended in 2023, has left a legacy of altered market dynamics. Future government housing policies may be influenced by the scheme's successes and failures, leading to a reliance on similar interventions even if their effectiveness diminishes over time. High theater ratio due to performative need to 'help' first time buyers even if it is economically unsound.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% From a global analytical perspective, the scheme represents a targeted intervention in the housing market. It exhibits characteristics of both coordination (helping first-time buyers) and extraction (inflating prices, benefiting builders at taxpayer expense). The long-term effects on housing affordability and market stability are uncertain.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_help_to_buy_scheme_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_help_to_buy_scheme, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_help_to_buy_scheme, TR),
    TR >= 0.70.

:- end_tests(uk_help_to_buy_scheme_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-High. The scheme extracted value from first-time buyers through higher house prices and potential debt burdens if prices stagnated. Also from taxpayers via potential defaults. Suppression (0.45): Moderate. The scheme suppressed alternative options for first-time buyers, pushing them towards new-build homes and government-backed mortgages. Theater Ratio (0.30): Low. The scheme had a relatively low theater ratio as it directly intervened in the housing market with tangible loans and subsidies. It did what it set out to do, but at what cost?
 *
 * PERSPECTIVAL GAP:
 *   First-time buyers experience the scheme as a snare, as they are highly incentivized to participate but face significant financial risks. New home builders see it as a rope, facilitating increased sales and profits. Taxpayers may view it as a tangled rope, as they bear the risk of loan defaults but may also benefit from a stimulated economy. The analytical observer recognizes the mixed effects of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agents' structural positions within the scheme. First-time buyers (victims) have limited exit options and bear the costs of participation, resulting in a high 'd' value. New home builders (beneficiaries) have arbitrage options and benefit from increased demand, resulting in a low 'd' value. UK Taxpayers have moderate power and constrained exit due to the election cycle, so they bear moderate costs. The analytical perspective observes from outside the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    house_price_volatility,
    'How will future house price volatility impact the repayment of equity loans and the financial stability of first-time buyers?',
    'Longitudinal analysis of house price data and repayment rates under different economic scenarios.',
    'High volatility could lead to significant losses for both first-time buyers and the government. Low volatility would make the scheme more sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(house_price_volatility, empirical, 'Impact of house price volatility on equity loan repayments.').

omega_variable(
    additionality_vs_displacement,
    'To what extent did the scheme generate additional housing supply versus simply displacing existing demand?',
    'Comparative analysis of new housing starts and sales before, during, and after the scheme, controlling for other market factors.',
    'High additionality would suggest the scheme successfully stimulated construction. High displacement would indicate it mainly inflated prices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(additionality_vs_displacement, empirical, 'The effect of the scheme on new housing supply.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_help_to_buy_scheme, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_h_tr_t0, uk_help_to_buy_scheme, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uk_h_tr_t5, uk_help_to_buy_scheme, theater_ratio, 5, 0.2).
narrative_ontology:measurement(uk_h_tr_t10, uk_help_to_buy_scheme, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(uk_h_be_t0, uk_help_to_buy_scheme, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uk_h_be_t5, uk_help_to_buy_scheme, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(uk_h_be_t10, uk_help_to_buy_scheme, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_help_to_buy_scheme, resource_allocation).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, uk_housing_affordability).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, mortgage_market_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
