% ============================================================================
% CONSTRAINT STORY: eu_renewable_energy_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_renewable_energy_mandate, []).

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
 *   constraint_id: eu_renewable_energy_mandate
 *   human_readable: EU Renewable Energy Directive and Support Schemes
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Renewable Energy Directive and associated national support schemes
 *   represent a complex effort to transition towards renewable energy
 *   sources. This constraint involves multiple stakeholders with differing
 *   perspectives and experiences. The policy aims to reduce carbon emissions
 *   and promote technological innovation in the renewable energy sector, but
 *   also leads to increased energy costs and market distortions.
 *
 * KEY AGENTS:
 *   - Renewable Energy Companies: Primary beneficiary (institutional/arbitrage) - benefits from subsidies and guaranteed market access.
 *   - Fossil Fuel Industry: Primary victim (powerful/constrained) - faces declining market share and increased competition.
 *   - Energy Intensive Industries: Secondary victim (powerless/trapped) - face higher energy costs and reduced competitiveness.
 *   - Taxpayers: Secondary victim (moderate/constrained) - bear the cost of subsidies and higher energy prices.
 *   - EU Bureaucracy: Enforcer and Beneficiary (institutional/constrained) - gains control over energy policy but faces coordination challenges.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_renewable_energy_mandate, 0.55).
domain_priors:suppression_score(eu_renewable_energy_mandate, 0.45).
domain_priors:theater_ratio(eu_renewable_energy_mandate, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, extractiveness, 0.55).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_renewable_energy_mandate, tangled_rope).
narrative_ontology:human_readable(eu_renewable_energy_mandate, "EU Renewable Energy Directive and Support Schemes").
narrative_ontology:topic_domain(eu_renewable_energy_mandate, "economic/political").

domain_priors:requires_active_enforcement(eu_renewable_energy_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, renewable_energy_companies).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, eu_bureaucracy).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, fossil_fuel_industry).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, energy_intensive_industries).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Energy intensive industries face higher energy costs due to the mandate, limiting their competitiveness and threatening jobs. They are often trapped due to the high capital costs of relocating or adapting production.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Renewable energy companies benefit from guaranteed prices and market access, allowing them to expand rapidly and attract investment. They can arbitrage different national support schemes.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% An analytical observer sees the mandate as a complex system of incentives and regulations with both positive effects (reduced emissions, technological innovation) and negative effects (increased costs, market distortions).
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% EU bureaucracy benefits from increased power and control over energy policy, but faces challenges in coordinating national policies and addressing unintended consequences. Their exit is constrained by political commitments.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Taxpayers bear the costs of subsidies and higher energy prices. Their ability to exit is constrained by national policies. Some taxpayers may be locked in as voters.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_renewable_energy_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_renewable_energy_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_renewable_energy_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high (0.55) due to the significant costs imposed on fossil fuel industries, energy-intensive industries, and taxpayers. Suppression is moderate (0.45) as the policy restricts the development and use of fossil fuels and favors renewable energy sources. The theater ratio is relatively low (0.30) as the policy has a substantial impact on energy production and consumption, but there is some performative aspect in terms of demonstrating commitment to climate goals.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ significantly depending on the stakeholders involved. Renewable energy companies see the policy as a positive driver of growth and innovation, while energy-intensive industries and taxpayers see it as a burden. The EU bureaucracy sees it as a necessary step towards achieving climate goals, while acknowledging the challenges of implementation and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the structural relationships between the stakeholders and the policy. Renewable energy companies benefit from the policy, resulting in a low directionality value. Fossil fuel industries, energy-intensive industries, and taxpayers bear the costs of the policy, resulting in high directionality values. The EU bureaucracy has a more complex relationship, benefiting from increased control but facing challenges in implementation, resulting in a moderate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification reflects the complex mix of coordination (incentivizing renewable energy production) and extraction (imposing costs on various actors). It prevents mislabeling the policy as pure extraction by acknowledging the genuine benefits of reduced emissions and technological innovation. It also prevents mislabeling it as pure coordination by recognizing the significant costs and market distortions involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_leakage_extent,
    'To what extent does the policy lead to carbon leakage (relocation of energy-intensive industries to countries with less stringent regulations)?',
    'Empirical studies analyzing industry relocation patterns and trade flows.',
    'High carbon leakage would undermine the environmental benefits of the policy and increase its economic costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_leakage_extent, empirical, 'Quantifies the degree to which the policy causes carbon leakage.').

omega_variable(
    technological_lock_in,
    'Does the policy create a technological lock-in by favoring specific renewable energy technologies over others?',
    'Analysis of innovation patterns and investment flows.',
    'Technological lock-in would hinder the development of more efficient and cost-effective renewable energy technologies in the future.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in, conceptual, 'Determines whether the policy favors specific renewable energy technologies over others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_renewable_energy_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_r_tr_t0, eu_renewable_energy_mandate, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_r_tr_t5, eu_renewable_energy_mandate, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eu_r_tr_t10, eu_renewable_energy_mandate, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(eu_r_be_t0, eu_renewable_energy_mandate, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(eu_r_be_t5, eu_renewable_energy_mandate, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(eu_r_be_t10, eu_renewable_energy_mandate, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_renewable_energy_mandate, resource_allocation).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, national_grid_infrastructure).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, carbon_pricing_mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
