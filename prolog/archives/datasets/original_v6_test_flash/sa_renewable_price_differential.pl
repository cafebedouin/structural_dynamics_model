% ============================================================================
% CONSTRAINT STORY: sa_renewable_price_differential
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sa_renewable_price_differential, []).

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
 *   constraint_id: sa_renewable_price_differential
 *   human_readable: SA Renewable Price Arbitrage Proxy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The significant price differential between South Australia (SA), with its
 *   high renewable energy penetration, and New South Wales (NSW) creates an
 *   arbitrage opportunity. SA's low wholesale prices ($37/MWh) contrast
 *   sharply with NSW's higher prices ($75/MWh). This differential acts as a
 *   proxy for underlying structural issues and creates both opportunities and
 *   challenges.
 *
 * KEY AGENTS:
 *   - Renewable Energy Generators in SA: Primary beneficiary (institutional/arbitrage)
 *   - Energy Storage Operators in SA: Secondary beneficiary (powerful/mobile)
 *   - Consumers in NSW: Primary victim (powerless/trapped)
 *   - Fossil Fuel Generators in NSW: Secondary victim (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sa_renewable_price_differential, 0.5).
domain_priors:suppression_score(sa_renewable_price_differential, 0.6).
domain_priors:theater_ratio(sa_renewable_price_differential, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sa_renewable_price_differential, extractiveness, 0.5).
narrative_ontology:constraint_metric(sa_renewable_price_differential, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sa_renewable_price_differential, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sa_renewable_price_differential, tangled_rope).
narrative_ontology:human_readable(sa_renewable_price_differential, "SA Renewable Price Arbitrage Proxy").
narrative_ontology:topic_domain(sa_renewable_price_differential, "economic/technological").

domain_priors:requires_active_enforcement(sa_renewable_price_differential).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, renewable_energy_generators_sa).
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, energy_storage_operators_sa).
narrative_ontology:constraint_victim(sa_renewable_price_differential, consumers_nsw).
narrative_ontology:constraint_victim(sa_renewable_price_differential, fossil_fuel_generators_nsw).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% NSW consumers are trapped due to lack of alternative energy sources and infrastructure limitations. The price differential acts as a snare, extracting wealth from them.
constraint_indexing:constraint_classification(sa_renewable_price_differential, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Fossil fuel generators in NSW are constrained by the renewable energy penetration in SA. They can't fully exit the market, but they still extract some value from existing infrastructure and contracts. Their power is eroding over time but isn't entirely gone yet.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Renewable energy generators in SA benefit from the high renewable penetration, driving down wholesale prices and potentially creating arbitrage opportunities. They can sell excess energy to other regions.
constraint_indexing:constraint_classification(sa_renewable_price_differential, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Energy storage operators (batteries, pumped hydro) benefit by buying energy when prices are low and selling back when prices are high. This exploits the price differential and stabilizes the grid but relies on active enforcement of grid regulations and contractual commitments from energy users.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the price differential as a tangled rope, with some coordination benefits (efficient energy allocation) and some extraction (wealth transfer from NSW to SA).
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sa_renewable_price_differential_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sa_renewable_price_differential, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sa_renewable_price_differential, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sa_renewable_price_differential_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.5): Significant wealth transfer from NSW consumers to SA energy producers. Suppression (0.6): NSW consumers have limited alternatives. Theater ratio (0.3): Limited performative activity; mostly real economic extraction.
 *
 * PERSPECTIVAL GAP:
 *   NSW consumers experience the differential as a snare, while SA generators see it as a coordination mechanism. Fossil fuel generators in NSW are losing ground but remain operational extracting value. The analytical observer sees a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (SA renewables and storage) gain from the differential. Victims (NSW consumers and fossil fuel generators) bear the costs. This creates a structural asymmetry.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interconnector_capacity,
    'What is the maximum capacity of the interconnector between SA and NSW?',
    'Engineering studies, historical data on interconnector flows',
    'Higher capacity reduces price differential, shifting classification towards rope. Lower capacity exacerbates price differential, shifting classification towards snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interconnector_capacity, empirical, 'Maximum capacity of the SA-NSW interconnector').

omega_variable(
    regulatory_intervention,
    'Will regulators intervene to reduce the price differential?',
    'Policy analysis, monitoring regulatory announcements',
    'Regulatory intervention reduces price differential, shifting classification towards rope or scaffold. Lack of intervention allows price differential to persist, shifting classification towards snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention, preference, 'Likelihood of regulatory intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sa_renewable_price_differential, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_r_tr_t0, sa_renewable_price_differential, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sa_r_tr_t5, sa_renewable_price_differential, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sa_r_tr_t10, sa_renewable_price_differential, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sa_r_be_t0, sa_renewable_price_differential, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sa_r_be_t5, sa_renewable_price_differential, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(sa_r_be_t10, sa_renewable_price_differential, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sa_renewable_price_differential, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
