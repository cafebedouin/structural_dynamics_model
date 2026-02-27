% ============================================================================
% CONSTRAINT STORY: ukraine_tight_gas_pilot
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukraine_tight_gas_pilot, []).

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
 *   constraint_id: ukraine_tight_gas_pilot
 *   human_readable: Ukraine Tight Gas Pilot Project Framework
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The Ukraine Tight Gas Pilot Project Framework is an exclusive partnership
 *   between Ukraine's national gas company (Naftogaz) and a foreign expert
 *   firm (Expert Petroleum) to develop previously inaccessible "tight gas"
 *   reserves. This arrangement creates a complex set of relationships with
 *   potential benefits for the involved companies, but also risks of
 *   asymmetric extraction from Ukrainian consumers and independent gas
 *   producers.
 *
 * KEY AGENTS:
 *   - Expert Petroleum: Primary beneficiary (institutional/arbitrage) - benefits from exclusive access and expertise.
 *   - Naftogaz Executives: Secondary beneficiary (institutional/constrained) - benefits from project success but is constrained by public scrutiny.
 *   - Ukrainian Consumers: Primary victim (powerless/trapped) - trapped by dependence on gas and lack of alternatives.
 *   - Independent Gas Producers: Secondary victim (moderate/constrained) - constrained by the exclusive agreement.
 *   - Analytical Observer: Sees the overall framework (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukraine_tight_gas_pilot, 0.6).
domain_priors:suppression_score(ukraine_tight_gas_pilot, 0.5).
domain_priors:theater_ratio(ukraine_tight_gas_pilot, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, extractiveness, 0.6).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukraine_tight_gas_pilot, tangled_rope).
narrative_ontology:human_readable(ukraine_tight_gas_pilot, "Ukraine Tight Gas Pilot Project Framework").
narrative_ontology:topic_domain(ukraine_tight_gas_pilot, "economic/geopolitical").

domain_priors:requires_active_enforcement(ukraine_tight_gas_pilot).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, expert_petroleum).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, naftogaz_executives).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, ukrainian_consumers).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, independent_gas_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Ukrainian consumers are trapped by the lack of competitive alternatives and bear the cost of potentially inflated gas prices due to the exclusive agreement and lack of price transparency.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Independent gas producers are constrained by the exclusive agreement, limiting their access to the tight gas reserves and market opportunities.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Expert Petroleum benefits from the exclusive partnership, gaining access to potentially lucrative gas reserves with reduced competition. They can arbitrage their expertise and capital.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Naftogaz executives benefit from the project through potential financial gains and political influence, but are constrained by the need to maintain the appearance of serving national interests.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% The project, viewed analytically over a long time horizon, is a tangled rope: a coordination effort to develop tight gas reserves, but with asymmetric extraction due to the exclusive agreement and potential for corruption.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukraine_tight_gas_pilot_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukraine_tight_gas_pilot, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ukraine_tight_gas_pilot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is assessed at 0.60 because the exclusive agreement allows Expert Petroleum to potentially extract significant profits without competitive pressure, and Naftogaz executives may benefit through corruption. The suppression is 0.50 because the agreement limits access to the reserves for other producers, suppressing competition. The theater ratio is 0.30, reflecting a genuine effort to develop tight gas reserves, but some performative elements are also present, like highlighting the supposed benefits for Ukrainian energy independence without addressing the pricing concerns.
 *
 * PERSPECTIVAL GAP:
 *   Ukrainian consumers see the framework as a snare, potentially leading to higher gas prices without significant benefits. Independent gas producers also see a snare, limiting their access to valuable reserves. Expert Petroleum views it as a rope, enabling efficient development and profit generation. Naftogaz executives view it as a tangled rope, with potential benefits and political risks. An analytical observer sees a tangled rope: a coordination mechanism with inherent risks of asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Expert Petroleum benefits from exclusive access and arbitrage opportunities, resulting in a low d value. Ukrainian consumers are trapped with limited alternatives, resulting in a high d value. Naftogaz executives occupy a middle ground, constrained by public scrutiny and benefiting from the project, giving them a moderate d value.
 *
 * MANDATROPHY ANALYSIS:
 *   The project risks being misclassified as a purely extractive snare. The tangled rope classification accounts for the fact that there is a genuine coordination problem involved in developing tight gas reserves, but the exclusive agreement and potential for corruption create significant extraction risks. The mandatrophy is resolved by acknowledging both the coordination and extraction elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_reserve_potential,
    'What is the true economic potential of the tight gas reserves?',
    'Independent geological surveys and production audits.',
    'If the reserves are less productive than claimed, the project''s economic justification weakens, revealing it as primarily rent-seeking. If highly productive, the exclusive agreement''s extraction becomes more problematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_reserve_potential, empirical, 'The true economic potential of the tight gas reserves.').

omega_variable(
    contract_transparency,
    'To what extent are the financial terms of the contract transparent and subject to public scrutiny?',
    'Public disclosure of the contract details and independent audits.',
    'Greater transparency reduces the potential for corruption and rent-seeking. Lack of transparency increases the likelihood of asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contract_transparency, empirical, 'The degree of transparency in the contract terms.').

omega_variable(
    regulatory_capture_level,
    'To what extent is the regulatory environment susceptible to capture by Naftogaz and Expert Petroleum?',
    'Analysis of lobbying activities, regulatory decisions, and revolving door appointments.',
    'High regulatory capture enables preferential treatment for the partnership and suppresses competition. Low capture ensures a level playing field for all gas producers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_level, empirical, 'The susceptibility of the regulatory environment to capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukraine_tight_gas_pilot, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukra_tr_t0, ukraine_tight_gas_pilot, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ukra_tr_t5, ukraine_tight_gas_pilot, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ukra_tr_t10, ukraine_tight_gas_pilot, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ukra_be_t0, ukraine_tight_gas_pilot, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ukra_be_t5, ukraine_tight_gas_pilot, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ukra_be_t10, ukraine_tight_gas_pilot, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukraine_tight_gas_pilot, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
