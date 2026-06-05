% ============================================================================
% CONSTRAINT STORY: trillion_bond_rush_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trillion_bond_rush_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trillion_bond_rush_2026
 *   human_readable: Global $1 Trillion Bond Issuance Record
 *   domain: economic/financial
 *
 * SUMMARY:
 *   Global bond issuance surpassed $1 trillion on Feb 2, 2026—the fastest
 *   pace in financial history. This surge in bond issuance presents a complex
 *   scenario with both short-term benefits and potential long-term risks.
 *   Issuing corporations and investment banks benefit from increased access
 *   to capital and underwriting fees, while long-term investors and future
 *   taxpayers may bear the burden of lower returns and increased financial
 *   instability.
 *
 * KEY AGENTS:
 *   - Issuing Corporations: Primary beneficiary (institutional/arbitrage) - benefit from low-interest rates
 *   - Investment Banks: Primary beneficiary (powerful/arbitrage) - benefit from underwriting fees
 *   - Long-Term Investors: Primary victim (moderate/constrained) - potential for lower returns
 *   - Future Taxpayers: Secondary victim (powerless/trapped) - bear the burden of debt repayment
 *   - Analytical Observer: Global financial observer (analytical/analytical) - assess the situation holistically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trillion_bond_rush_2026, 0.55).
domain_priors:suppression_score(trillion_bond_rush_2026, 0.4).
domain_priors:theater_ratio(trillion_bond_rush_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trillion_bond_rush_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trillion_bond_rush_2026, tangled_rope).
narrative_ontology:human_readable(trillion_bond_rush_2026, "Global $1 Trillion Bond Issuance Record").
narrative_ontology:topic_domain(trillion_bond_rush_2026, "economic/financial").

domain_priors:requires_active_enforcement(trillion_bond_rush_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, issuing_corporations).
narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, investment_banks).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, long_term_investors).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future taxpayers are trapped by the debt incurred today and lack the power to influence current decisions. They bear the burden of repayment or potential default, experiencing the bond issuance as a snare.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Long-term investors, such as pension funds, are somewhat constrained. They need to invest in bonds but may face lower returns or higher risk due to the increased supply and potential credit deterioration. They experience a tangled rope situation: a coordination function with extraction.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Issuing corporations benefit from the low-interest rates and easy access to capital, enabling them to fund projects and expansions. They perceive the situation as a rope: a coordination mechanism that facilitates their financing needs.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Investment banks benefit significantly from the increased bond issuance through underwriting fees and trading profits. They have arbitrage opportunities and see it as a rope: a coordination mechanism that efficiently allocates capital and provides liquidity.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees the global bond issuance record as a tangled rope. It provides short-term benefits through increased economic activity and access to capital, but at the potential expense of long-term financial stability and increased risk for future generations.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trillion_bond_rush_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trillion_bond_rush_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trillion_bond_rush_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trillion_bond_rush_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) as the bond issuance does extract value from future taxpayers and long-term investors. Suppression is also moderate (0.40) because while there aren't explicit restrictions, future generations have little power to alter the current course, and investors face limited alternative investment options given prevailing market conditions. The theater ratio (0.30) is relatively low, indicating that the bond issuance has a substantial functional component related to capital allocation and economic activity.
 *
 * PERSPECTIVAL GAP:
 *   Issuing corporations and investment banks view the bond issuance as a beneficial rope, facilitating capital allocation and generating profits. Long-term investors see a tangled rope, constrained by the need to invest in bonds but potentially facing lower returns. Future taxpayers, lacking power and trapped by future obligations, experience it as a snare. The analytical observer recognizes the complex interplay of benefits and risks, classifying it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Issuing corporations and investment banks benefit from the increased bond issuance, giving them a low 'd' value. Long-term investors are somewhat constrained and receive a moderate 'd' value. Future taxpayers bear the costs and are assigned a high 'd' value, reflecting their lack of agency and the burden they will inherit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainable_debt_levels,
    'What is the sustainable level of global debt relative to GDP?',
    'Economic modeling, historical analysis of debt crises, and scenario planning',
    'Determines the long-term viability of the current debt trajectory. High debt levels may lead to financial instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainable_debt_levels, empirical, 'Determination of sustainable global debt levels').

omega_variable(
    risk_assessment_accuracy,
    'Are current risk assessment models accurately capturing the systemic risks associated with high levels of bond issuance?',
    'Stress testing, backtesting of model performance, and expert reviews',
    'Inaccurate risk assessments could lead to underestimation of potential losses and increased vulnerability to financial shocks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_assessment_accuracy, empirical, 'Accuracy of risk assessment models in capturing systemic risks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trillion_bond_rush_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tril_tr_t0, trillion_bond_rush_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tril_tr_t5, trillion_bond_rush_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(tril_tr_t10, trillion_bond_rush_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(tril_be_t0, trillion_bond_rush_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(tril_be_t5, trillion_bond_rush_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tril_be_t10, trillion_bond_rush_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trillion_bond_rush_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
