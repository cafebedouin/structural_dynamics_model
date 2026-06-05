% ============================================================================
% CONSTRAINT STORY: pe_rental_market_lockin
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pe_rental_market_lockin, []).

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
 *   constraint_id: pe_rental_market_lockin
 *   human_readable: Private Equity Lock-in of Single-Family Rental Market
 *   domain: economic
 *
 * SUMMARY:
 *   Following the 2008 financial crisis, large private equity firms began
 *   buying vast portfolios of single-family homes, converting them into
 *   rentals. This creates a lock-in effect, limiting housing options for
 *   low-income renters and first-time homebuyers, while benefiting private
 *   equity firms and their shareholders. This lock-in is a snare from the
 *   perspective of those trapped in the rental market.
 *
 * KEY AGENTS:
 *   - Private Equity Firms: Primary beneficiary (institutional/arbitrage)
 *   - Shareholders: Secondary beneficiary (powerful/arbitrage)
 *   - Low-Income Renters: Primary victim (powerless/trapped)
 *   - First-Time Homebuyers: Secondary victim (powerless/trapped)
 *   - Local Governments: Constrained actor (moderate/constrained)
 *   - Traditional Mortgage Lenders: Inertial actor (institutional/constrained)
 *   - Analytical Observer: Neutral observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_rental_market_lockin, 0.7).
domain_priors:suppression_score(pe_rental_market_lockin, 0.8).
domain_priors:theater_ratio(pe_rental_market_lockin, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_rental_market_lockin, extractiveness, 0.7).
narrative_ontology:constraint_metric(pe_rental_market_lockin, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(pe_rental_market_lockin, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pe_rental_market_lockin, tangled_rope).
narrative_ontology:human_readable(pe_rental_market_lockin, "Private Equity Lock-in of Single-Family Rental Market").
narrative_ontology:topic_domain(pe_rental_market_lockin, "economic").

domain_priors:requires_active_enforcement(pe_rental_market_lockin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, private_equity_firms).
narrative_ontology:constraint_beneficiary(pe_rental_market_lockin, shareholders).
narrative_ontology:constraint_victim(pe_rental_market_lockin, low_income_renters).
narrative_ontology:constraint_victim(pe_rental_market_lockin, first_time_homebuyers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income renters face limited housing options and increased rental rates, making it difficult to escape the rental market. Their exit options are severely limited, resulting in a snare classification.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% First-time homebuyers are priced out of the market due to increased competition from private equity firms, restricting their ability to purchase homes and build wealth. Their exit to homeownership is blocked.
constraint_indexing:constraint_classification(pe_rental_market_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Private equity firms benefit from stable rental income and increased property values, creating a positive feedback loop that reinforces their market dominance. They experience this as a rope as it provides a mechanism for predictable returns.
constraint_indexing:constraint_classification(pe_rental_market_lockin, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Local governments face increased pressure to address housing affordability issues, while also benefiting from increased property tax revenue. Their exit options are constrained by political and economic factors. The enforcement is the regulatory environment that allows this market behavior.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Traditional mortgage lenders are finding it difficult to compete with private equity firms that have the resources to purchase properties outright. They are constrained by existing lending practices and regulations. Their function is being degraded.
constraint_indexing:constraint_classification(pe_rental_market_lockin, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observers see a tangled rope due to the mixture of coordination and extraction. The private equity firms provide housing, which is coordination. But the method suppresses alternatives, which is extraction.
constraint_indexing:constraint_classification(pe_rental_market_lockin, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_rental_market_lockin_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pe_rental_market_lockin, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pe_rental_market_lockin, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pe_rental_market_lockin, TR),
    TR >= 0.70.

:- end_tests(pe_rental_market_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) as private equity firms extract significant rents and capital gains from the housing market, limiting access for others. Suppression is also high (0.80) due to the scale of PE acquisitions, which constricts the housing supply and inflates prices. Theater ratio is now at 0.75, reflecting that while the firms genuinely provide housing, a significant portion of their activity is performative in the sense that it maintains the status quo of limited housing options and inflated prices, benefiting shareholders more than renters.
 *
 * PERSPECTIVAL GAP:
 *   Low-income renters and first-time homebuyers experience this as a snare, as they are trapped in a rental market with limited options. Private equity firms see this as a rope, as it provides a predictable stream of income and capital appreciation. Local governments and traditional mortgage lenders experience this as an inertial process. The analytical observer recognizes the combination of coordination and extraction inherent in this arrangement. 
 *
 * DIRECTIONALITY LOGIC:
 *   Private equity firms and their shareholders are beneficiaries, as they gain wealth from the rental market. Low-income renters and first-time homebuyers are victims, as they face increased housing costs and limited opportunities. Local governments are both beneficiaries and victims. The analytical observer is neutral. The directionality aligns with the structural positions of each agent.
 *
 * MANDATROPHY ANALYSIS:
 *   This lock-in is not a rope because while private equity provides housing, it is done in a way that suppresses alternatives, thus qualifying as a snare. It is also not merely a piton because private equity is actively acquiring properties and raising rents. The tangled rope classification from the analytical perspective captures the dual nature of providing housing (coordination) while extracting value and suppressing alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_intervention,
    'Will regulatory interventions effectively limit private equity activity in the single-family rental market?',
    'Analysis of policy effectiveness in limiting PE acquisitions and promoting affordable housing',
    'If effective, shifts towards tangled rope/scaffold; if ineffective, remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention, empirical, 'The effectiveness of regulatory interventions in limiting PE activity.').

omega_variable(
    housing_supply_elasticity,
    'How elastically will the housing supply respond to increased demand?',
    'Econometric analysis of housing supply response to price signals',
    'High elasticity: shifts towards tangled rope/rope. Low elasticity: reinforces the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_supply_elasticity, empirical, 'The elasticity of the housing supply.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pe_rental_market_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pe_r_tr_t0, pe_rental_market_lockin, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pe_r_tr_t5, pe_rental_market_lockin, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pe_r_tr_t10, pe_rental_market_lockin, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(pe_r_be_t0, pe_rental_market_lockin, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pe_r_be_t5, pe_rental_market_lockin, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(pe_r_be_t10, pe_rental_market_lockin, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pe_rental_market_lockin, resource_allocation).
narrative_ontology:affects_constraint(pe_rental_market_lockin, financialization_of_housing).
narrative_ontology:affects_constraint(pe_rental_market_lockin, affordable_housing_shortage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
