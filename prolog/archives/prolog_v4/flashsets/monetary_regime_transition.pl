% ============================================================================
% CONSTRAINT STORY: monetary_regime_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_regime_transition, []).

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
 *   constraint_id: monetary_regime_transition
 *   human_readable: Sovereign Fiat Currency Regime
 *   domain: economic/political
 *
 * SUMMARY:
 *   The sovereign fiat currency regime is a monetary system where the value
 *   of money is not tied to a physical commodity but is declared by the
 *   government. This system allows for monetary policy and economic
 *   management, but it also creates opportunities for inflation and
 *   government manipulation. The transition from commodity-backed currencies
 *   to fiat currencies fundamentally altered the relationship between money
 *   and its perceived value, creating new dynamics of power and control.
 *
 * KEY AGENTS:
 *   - Government: Primary beneficiary (institutional/arbitrage) - benefits from seigniorage and monetary policy control.
 *   - Commercial Banks: Secondary beneficiary (powerful/arbitrage) - benefits from fractional reserve banking and credit creation.
 *   - General Population: Primary victim (powerless/trapped) - bears the costs of inflation and currency devaluation.
 *   - Fixed Income Earners: Secondary victim (moderate/constrained) - vulnerable to inflation eroding the value of their fixed payments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_regime_transition, 0.6).
domain_priors:suppression_score(monetary_regime_transition, 0.7).
domain_priors:theater_ratio(monetary_regime_transition, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_regime_transition, extractiveness, 0.6).
narrative_ontology:constraint_metric(monetary_regime_transition, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(monetary_regime_transition, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_regime_transition, tangled_rope).
narrative_ontology:human_readable(monetary_regime_transition, "Sovereign Fiat Currency Regime").
narrative_ontology:topic_domain(monetary_regime_transition, "economic/political").

domain_priors:requires_active_enforcement(monetary_regime_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_regime_transition, government).
narrative_ontology:constraint_beneficiary(monetary_regime_transition, commercial_banks).
narrative_ontology:constraint_victim(monetary_regime_transition, general_population).
narrative_ontology:constraint_victim(monetary_regime_transition, fixed_income_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general population is largely trapped within the fiat currency system due to legal tender laws and the lack of viable alternatives for day-to-day transactions. They bear the costs of inflation and potential devaluation.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Fixed income earners (e.g., pensioners) are constrained by their reliance on predetermined payments that may not keep pace with inflation, but they also benefit from the stability provided by a centrally managed currency in normal times.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The government benefits from the ability to create money to finance its operations and manage the economy, but it is also constrained by the need to maintain confidence in the currency. They can essentially arbitrage their position by controlling the supply and demand levers.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Commercial banks benefit from the fractional reserve system and the ability to create credit, but they are also subject to regulation and the risk of bank runs. They benefit from the system, but face constraints and risks. They are able to arbitrage the system through leveraging the money supply.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the fiat currency system is a complex arrangement with both benefits and drawbacks. It enables monetary policy and economic management, but it also creates opportunities for government mismanagement and inflation. Its overall long-term sustainability is uncertain.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_regime_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_regime_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_regime_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_regime_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_regime_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-High. The government benefits from the seigniorage (profit from issuing currency) and the ability to manage the economy through monetary policy, but this comes at the expense of the general population, who bear the costs of inflation and potential currency devaluation.  The system extracts value through inflation, which redistributes wealth from savers and fixed-income earners to borrowers and the government. Suppression (0.70): High. The legal tender laws and the dominance of the national currency make it difficult for individuals and businesses to opt out of the fiat system. Alternative currencies exist, but they face regulatory hurdles and lack widespread acceptance. Theater Ratio (0.30): Low. While there is some theater involved in maintaining confidence in the currency (e.g., central bank announcements, government assurances), the system is largely driven by real economic forces and policy decisions.
 *
 * PERSPECTIVAL GAP:
 *   The government sees the fiat currency as a tool for economic management and stability (Rope). Commercial banks see it as an opportunity for profit through credit creation (Tangled Rope). The general population experiences it as a system that can erode their purchasing power (Snare). Fixed income earners are particularly vulnerable to inflation (Tangled Rope). The analytical observer recognizes the complex trade-offs and potential risks of the system (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The government benefits from the ability to create money and control monetary policy (low d). Commercial banks benefit from the fractional reserve system and credit creation (low d). The general population bears the costs of inflation and currency devaluation (high d). Fixed income earners are particularly vulnerable to inflation (high d). The analytical observer sees both the benefits and drawbacks of the system (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The fiat currency regime appears superficially as a tool (Rope) for government. However, the extractive component (inflation) is significant, and the lack of exit options for the general population suggests a Snare. The Tangled Rope classification captures the dual nature of the system: coordination for the government and extraction from the population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_tolerance,
    'What level of inflation is politically and economically tolerable before the system''s legitimacy is undermined?',
    'Empirical analysis of historical inflation rates and their impact on political stability and economic growth.',
    'If the tolerance is high, the government has more flexibility in managing the economy. If it is low, the government is more constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_tolerance, empirical, 'The acceptable level of inflation within the system.').

omega_variable(
    alternative_currencies,
    'To what extent can alternative currencies (e.g., cryptocurrencies, commodity-backed currencies) provide a viable exit option from the fiat system?',
    'Analysis of the adoption rates, stability, and regulatory treatment of alternative currencies.',
    'If alternatives are viable, the government''s control over the monetary system is weakened. If they are not, the population is more dependent on the fiat system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_currencies, empirical, 'The viability of alternative currency systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_regime_transition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_regime_transition, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mone_tr_t50, monetary_regime_transition, theater_ratio, 50, 0.3).
narrative_ontology:measurement(mone_tr_t100, monetary_regime_transition, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_regime_transition, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mone_be_t50, monetary_regime_transition, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(mone_be_t100, monetary_regime_transition, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_regime_transition, resource_allocation).
narrative_ontology:affects_constraint(monetary_regime_transition, central_banking_independence).
narrative_ontology:affects_constraint(monetary_regime_transition, government_debt_sustainability).

% DUAL FORMULATION NOTE:
% This story focuses on the transition to and properties of a fiat currency regime. Related constraints such as central bank independence and government debt sustainability explore specific aspects and risks within this regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
