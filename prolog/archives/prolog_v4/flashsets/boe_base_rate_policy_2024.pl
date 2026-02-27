% ============================================================================
% CONSTRAINT STORY: boe_base_rate_policy_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-11-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boe_base_rate_policy_2024, []).

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
 *   constraint_id: boe_base_rate_policy_2024
 *   human_readable: Bank of England's 5.25% Base Interest Rate Policy (2024)
 *   domain: economic
 *
 * SUMMARY:
 *   The Bank of England's decision to maintain the base interest rate at
 *   5.25% is a complex economic intervention aimed at controlling inflation.
 *   While intended to stabilize the economy, the policy creates winners and
 *   losers, with varying degrees of impact depending on their economic
 *   position. Mortgage holders and small businesses bear the brunt of the
 *   higher costs, while the BoE and bondholders benefit from reduced
 *   inflation and potentially higher returns.
 *
 * KEY AGENTS:
 *   - Bank of England: Institutional actor responsible for setting monetary policy.
 *   - Mortgage Holders: Individuals and families with variable-rate mortgages.
 *   - Small Businesses: Companies reliant on borrowing for investment and operations.
 *   - Bondholders: Investors holding government and corporate bonds.
 *   - Currency Speculators: Actors betting on the value of the British pound.
 *   - Unemployed: Those negatively impacted by suppressed economic activity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boe_base_rate_policy_2024, 0.65).
domain_priors:suppression_score(boe_base_rate_policy_2024, 0.75).
domain_priors:theater_ratio(boe_base_rate_policy_2024, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, extractiveness, 0.65).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boe_base_rate_policy_2024, tangled_rope).
narrative_ontology:human_readable(boe_base_rate_policy_2024, "Bank of England's 5.25% Base Interest Rate Policy (2024)").
narrative_ontology:topic_domain(boe_base_rate_policy_2024, "economic").

domain_priors:requires_active_enforcement(boe_base_rate_policy_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, boe).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, bondholders).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, currency_speculators).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, mortgage_holders).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, small_businesses).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, unemployed).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of mortgage holders trapped by high interest rates, facing increased costs and potential foreclosure.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of small businesses constrained by higher borrowing costs but potentially benefiting from reduced inflation.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the Bank of England aiming to control inflation through interest rate policy.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% Perspective of currency speculators benefiting from a stronger pound due to high interest rates, able to leverage arbitrage opportunities.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical perspective recognizing the complex interplay of coordination and extraction inherent in the policy.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boe_base_rate_policy_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boe_base_rate_policy_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(boe_base_rate_policy_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the policy directly extracts value from mortgage holders and small businesses through increased borrowing costs. The suppression is also high as individuals and businesses have limited alternatives to the prevailing interest rates. The theater ratio is relatively low as the policy has a direct and intended effect on economic activity.
 *
 * PERSPECTIVAL GAP:
 *   Mortgage holders view the policy as a snare, trapping them with unaffordable payments. Small businesses experience a tangled rope, facing higher costs but hoping for long-term stability. The Bank of England sees it as a necessary rope, coordinating monetary policy to achieve its inflation target. Currency speculators benefit from increased value and arbitrage opportunities, making them tangled rope as they are reliant on BoE policy but can exit at will. The analytical observer sees the tangled rope, balancing the need for inflation control with the risk of economic recession.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic is determined by who benefits from the policy and who bears the costs. The BoE benefits from achieving its inflation target, bondholders gain from reduced inflation, and currency speculators profit from arbitrage. Mortgage holders and small businesses bear the costs of higher interest rates. This informs the extractiveness value, which is higher for those who are trapped and powerless.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_persistence,
    'How persistent is the underlying inflation, and how effectively does the base rate policy address it?',
    'Monitoring inflation trends, analyzing wage growth, and assessing global supply chain dynamics.',
    'If inflation proves persistent, the policy may need to be more aggressive, leading to greater economic pain. If inflation is transitory, the policy could be eased sooner.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_persistence, empirical, 'The degree to which inflation proves resistant to monetary policy.').

omega_variable(
    economic_recession_risk,
    'What is the likelihood that the high interest rates will trigger a significant economic recession?',
    'Tracking GDP growth, unemployment rates, and consumer confidence.',
    'If a recession occurs, the policy may need to be reversed, potentially undermining credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_recession_risk, empirical, 'The risk that the policy will induce a recession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boe_base_rate_policy_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boe__tr_t0, boe_base_rate_policy_2024, theater_ratio, 0, 0.25).
narrative_ontology:measurement(boe__tr_t6, boe_base_rate_policy_2024, theater_ratio, 6, 0.3).
narrative_ontology:measurement(boe__tr_t12, boe_base_rate_policy_2024, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(boe__be_t0, boe_base_rate_policy_2024, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(boe__be_t6, boe_base_rate_policy_2024, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(boe__be_t12, boe_base_rate_policy_2024, base_extractiveness, 12, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boe_base_rate_policy_2024, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
