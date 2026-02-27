% ============================================================================
% CONSTRAINT STORY: car_ownership_norm_us
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_car_ownership_norm_us, []).

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
 *   constraint_id: car_ownership_norm_us
 *   human_readable: The Norm of Individual Car Ownership in the US
 *   domain: economic
 *
 * SUMMARY:
 *   Based on economist Dean Baker's analysis, this constraint models the
 *   socio-economic system in the United States that normalizes and often
 *   necessitates individual car ownership. It examines the various actors
 *   involved and how they are affected by this constraint.
 *
 * KEY AGENTS:
 *   - Low-Income Individuals: Primary target (powerless/trapped) - bear the costs of car ownership while lacking alternatives
 *   - Automotive Industry: Primary beneficiary (institutional/arbitrage) - benefits from car sales and related services
 *   - Public Transportation Users: Secondary target (moderate/constrained) - suffer from underfunded and inadequate public transportation systems
 *   - Environment: Abstract target (powerless/trapped) - bears the costs of pollution and resource depletion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(car_ownership_norm_us, 0.6).
domain_priors:suppression_score(car_ownership_norm_us, 0.7).
domain_priors:theater_ratio(car_ownership_norm_us, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(car_ownership_norm_us, extractiveness, 0.6).
narrative_ontology:constraint_metric(car_ownership_norm_us, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(car_ownership_norm_us, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(car_ownership_norm_us, tangled_rope).
narrative_ontology:human_readable(car_ownership_norm_us, "The Norm of Individual Car Ownership in the US").
narrative_ontology:topic_domain(car_ownership_norm_us, "economic").

domain_priors:requires_active_enforcement(car_ownership_norm_us).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, automotive_industry).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, oil_industry).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, road_construction_companies).
narrative_ontology:constraint_victim(car_ownership_norm_us, low_income_individuals).
narrative_ontology:constraint_victim(car_ownership_norm_us, public_transportation_users).
narrative_ontology:constraint_victim(car_ownership_norm_us, environment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-income individuals are often trapped by the need for a car to access employment, healthcare, and other essential services, even though they can least afford it. Public transportation is often inadequate in many areas, especially suburban and rural ones.
constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Middle-class commuters are constrained by the need for a car to access employment, but also benefit from the infrastructure and services that support car ownership. They have some degree of mobility but are also subject to the costs of car ownership.
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The automotive industry benefits from the norm of individual car ownership, as it creates a large and stable market for its products. The industry actively lobbies for policies that support car ownership, such as highway construction and low gas taxes.
constraint_indexing:constraint_classification(car_ownership_norm_us, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, the norm of individual car ownership is a tangled rope, as it provides mobility and economic benefits but also creates environmental problems and social inequalities. Alternative models like robust public transport are suppressed.
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(car_ownership_norm_us_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(car_ownership_norm_us, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(car_ownership_norm_us_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Moderate-High. Significant costs associated with car ownership including purchase price, maintenance, insurance, and fuel. These costs disproportionately affect low-income individuals. Suppression (0.70): High. Limited access to affordable housing near employment centers and inadequate public transportation options in many areas force individuals to rely on cars. Theater Ratio (0.30): Low-Moderate. While there are some performative aspects, car ownership primarily serves a functional purpose of transportation, so theater is low.
 *
 * PERSPECTIVAL GAP:
 *   Low-income individuals view the norm as a snare, as they are trapped by the need for a car to access basic services. The automotive industry views the norm as a rope, as it facilitates their business operations. The analytical observer sees a tangled rope, recognizing both the benefits and drawbacks of the norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries such as the automotive industry have low directionality, as they actively shape the system. Victims such as low-income individuals have high directionality, as they bear the costs. Analytical observer balances the benefits and costs across the entire system, leading to a moderate extractiveness value.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling car ownership as pure extraction or pure coordination. It acknowledges both the extractive and coordinating aspects of the norm. It is not pure extraction because cars offer mobility. It is not pure coordination because car usage imposes externalities (pollution, traffic) and forces the poor to bear high fixed costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_transport_feasibility,
    'To what extent can public transportation realistically replace individual car ownership in different regions of the US?',
    'Cost-benefit analysis of public transportation investments in different regions, taking into account population density, geography, and existing infrastructure.',
    'If public transportation is feasible in many regions, the norm of individual car ownership may be weakened. If it is not, the norm may persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_transport_feasibility, empirical, 'Feasibility of public transportation alternatives.').

omega_variable(
    environmental_costs_acceptance,
    'To what extent are Americans willing to accept the environmental costs of individual car ownership?',
    'Public opinion surveys and analysis of consumer behavior regarding fuel efficiency and alternative transportation options.',
    'If Americans are willing to accept the environmental costs, the norm of individual car ownership may persist. If they are not, there may be more pressure for policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_costs_acceptance, preference, 'Acceptance of environmental costs of car ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(car_ownership_norm_us, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(car__tr_t0, car_ownership_norm_us, theater_ratio, 0, 0.2).
narrative_ontology:measurement(car__tr_t10, car_ownership_norm_us, theater_ratio, 10, 0.3).
narrative_ontology:measurement(car__tr_t20, car_ownership_norm_us, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(car__be_t0, car_ownership_norm_us, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(car__be_t10, car_ownership_norm_us, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(car__be_t20, car_ownership_norm_us, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(car_ownership_norm_us, resource_allocation).
narrative_ontology:affects_constraint(car_ownership_norm_us, highway_funding_model).
narrative_ontology:affects_constraint(car_ownership_norm_us, suburban_sprawl).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
