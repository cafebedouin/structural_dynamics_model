% ============================================================================
% CONSTRAINT STORY: israel_egypt_gas_deal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_egypt_gas_deal, []).

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
 *   constraint_id: israel_egypt_gas_deal
 *   human_readable: Geopolitical Gas Supply Agreement between Israel and Egypt
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Geopolitical Gas Supply Agreement between Israel and Egypt is a
 *   bilateral agreement where Israel supplies natural gas to Egypt. This
 *   agreement presents a complex interplay of economic and political
 *   interests, with both beneficiary and victim agents affected. The
 *   agreement aims to provide Egypt with a stable energy source, while also
 *   providing Israel with a profitable export market.
 *
 * KEY AGENTS:
 *   - Israeli Gas Companies: Primary beneficiary (institutional/arbitrage) – Benefits from a stable export market and increased revenue.
 *   - Egyptian Industrial Sector: Secondary beneficiary (institutional/constrained) - Benefits from reliable gas supply
 *   - Egyptian Consumers: Primary target (powerless/trapped) – Face potentially higher energy costs and limited energy choices.
 *   - Alternative Energy Suppliers (Egypt): Secondary target (moderate/constrained) - Limited growth because of low gas prices.
 *   - Egyptian Government: Institutional actor (institutional/constrained) - constrained by the agreement terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_egypt_gas_deal, 0.55).
domain_priors:suppression_score(israel_egypt_gas_deal, 0.4).
domain_priors:theater_ratio(israel_egypt_gas_deal, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_egypt_gas_deal, extractiveness, 0.55).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_egypt_gas_deal, tangled_rope).
narrative_ontology:human_readable(israel_egypt_gas_deal, "Geopolitical Gas Supply Agreement between Israel and Egypt").
narrative_ontology:topic_domain(israel_egypt_gas_deal, "geopolitical/economic").

domain_priors:requires_active_enforcement(israel_egypt_gas_deal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, israeli_gas_companies).
narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, egyptian_industrial_sector).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, egyptian_consumers).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, alternative_energy_suppliers_egypt).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Egyptian consumers face higher energy costs and limited choice due to the deal. They are trapped as their consumption is necessary and alternatives are limited.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Israeli gas companies benefit from a stable export market and increased revenue. They can arbitrage this position due to the agreement's long-term nature.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Egyptian government benefits from securing a stable gas supply for industry but is constrained by the agreement's terms and geopolitical implications.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a global perspective, the agreement represents a complex interplay of economic and political factors with both benefits and drawbacks.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_egypt_gas_deal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_egypt_gas_deal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(israel_egypt_gas_deal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The agreement extracts value from Egyptian consumers through potentially higher energy costs and reduced availability of alternative energy sources. The suppression (0.40) arises from the long-term nature of the agreement and the limited options for Egyptian consumers to switch to alternative energy sources in the short term. The relatively low theater ratio reflects the practical, rather than performative, nature of this gas supply agreement. Egypt depends on the gas, and Israel profits from selling it.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge based on the agent's position within the agreement. Israeli gas companies view it as a beneficial coordination mechanism (Rope), securing a stable export market. Egyptian consumers experience it as a Snare, facing higher costs and limited alternatives. The Egyptian government perceives it as a Tangled Rope, balancing the need for a stable energy supply with potential economic and political constraints. The analytical observer sees the complex interplay of factors that produces both winners and losers.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from agent relationships. Israeli companies are beneficiaries with arbitrage options (low d); Egyptian consumers are victims with trapped options (high d). The Egyptian government is constrained, resulting in a mid-range d reflecting a mix of benefits and costs. The analytical observer has a perspective from both positive and negative sides, therefore having an analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of Tangled Rope resolves the mandatrophy by acknowledging both the coordination benefits (stable energy supply) and extractive aspects (potentially higher costs for consumers) of the agreement. It prevents mislabeling the agreement as pure coordination (Rope) by recognizing the negative impacts on Egyptian consumers and alternative energy suppliers. It also avoids classifying it as pure extraction (Snare) by acknowledging the benefits for Israeli gas companies and the Egyptian industrial sector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_price_volatility,
    'How will long-term gas price volatility affect the economic viability and geopolitical balance of the agreement?',
    'Economic modeling and scenario planning based on projected gas market trends.',
    'Significant price fluctuations could strain the agreement, leading to renegotiation or even cancellation, altering the regional energy landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_price_volatility, empirical, 'Impact of long-term gas price volatility').

omega_variable(
    alternative_energy_development,
    'To what extent will the agreement inhibit the development of alternative energy sources within Egypt?',
    'Analysis of investment trends in renewable energy projects in Egypt and comparison with other countries in the region.',
    'If the agreement discourages investment in renewables, it could create a long-term energy dependency and hinder Egypt''s transition to a more sustainable energy mix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_energy_development, empirical, 'Impact on alternative energy development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_egypt_gas_deal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israel_egypt_gas_deal, theater_ratio, 0, 0.1).
narrative_ontology:measurement(isra_tr_t5, israel_egypt_gas_deal, theater_ratio, 5, 0.15).
narrative_ontology:measurement(isra_tr_t10, israel_egypt_gas_deal, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israel_egypt_gas_deal, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(isra_be_t5, israel_egypt_gas_deal, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(isra_be_t10, israel_egypt_gas_deal, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_egypt_gas_deal, resource_allocation).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, egypt_israel_relations).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, regional_energy_security).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
