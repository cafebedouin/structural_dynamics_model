% ============================================================================
% CONSTRAINT STORY: condiment_tyranny
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_condiment_tyranny, []).

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
 *   constraint_id: condiment_tyranny
 *   human_readable: The Tyranny of the Default Condiment Offering
 *   domain: social/economic
 *
 * SUMMARY:
 *   Social and economic pressures from large-scale food service operations
 *   result in a standardized, limited set of default condiments (e.g.,
 *   ketchup, mustard, mayonnaise). This 'condiment tyranny' can suppress
 *   individual consumer preferences and limit opportunities for smaller, more
 *   diverse condiment producers.
 *
 * KEY AGENTS:
 *   - Large Food Service Operators: Primary beneficiary (institutional/arbitrage) - benefits from bulk purchasing and streamlined operations
 *   - Condiment Manufacturers: Primary beneficiary (institutional/arbitrage) - benefits from bulk contracts and brand dominance
 *   - Individual Consumers: Primary target (powerless/trapped) - faces limited condiment options and suppressed preferences
 *   - Small Condiment Producers: Secondary target (moderate/constrained) - faces barriers to entry and limited distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(condiment_tyranny, 0.5).
domain_priors:suppression_score(condiment_tyranny, 0.6).
domain_priors:theater_ratio(condiment_tyranny, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(condiment_tyranny, extractiveness, 0.5).
narrative_ontology:constraint_metric(condiment_tyranny, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(condiment_tyranny, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(condiment_tyranny, tangled_rope).
narrative_ontology:human_readable(condiment_tyranny, "The Tyranny of the Default Condiment Offering").
narrative_ontology:topic_domain(condiment_tyranny, "social/economic").

domain_priors:requires_active_enforcement(condiment_tyranny).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(condiment_tyranny, large_food_service_operators).
narrative_ontology:constraint_beneficiary(condiment_tyranny, condiment_manufacturers).
narrative_ontology:constraint_victim(condiment_tyranny, individual_consumers).
narrative_ontology:constraint_victim(condiment_tyranny, small_condiment_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual consumer is often trapped with the default condiment options, unable to easily access or afford alternatives when dining out.
constraint_indexing:constraint_classification(condiment_tyranny, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Small producers face barriers to entry due to established distribution networks and bulk purchasing agreements, but benefit from niche markets and local consumer preferences.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Benefit from bulk purchasing and standardized offerings, streamlining operations and reducing costs. Standardization can be viewed as a form of coordination.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analyzes the system as a whole, seeing a complex interplay of economic incentives, standardization practices, and consumer preferences, resulting in a tangled rope.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(condiment_tyranny_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(condiment_tyranny, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(condiment_tyranny, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(condiment_tyranny, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(condiment_tyranny_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.5) because while consumers are limited, they are not completely without choice. Suppression is moderate-high (0.6) reflecting the difficulty in accessing alternative condiments in many settings. Theater Ratio is relatively low (0.3) as the dominant condiments generally serve their function.
 *
 * PERSPECTIVAL GAP:
 *   Consumers see a snare because they often have no alternative. Large food service operators see a rope because standardization allows them to coordinate efficiently. Small producers see a mixed arrangement (tangled rope) because they are both constrained by the dominance of larger players but also may benefit from niche markets and local consumer preference. The analytical perspective considers all actors and forces.
 *
 * DIRECTIONALITY LOGIC:
 *   Large food service and condiment manufacturers benefit from cost reductions, while consumers bear the cost of limited choice, and small producers suffer reduced access to markets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_preference_diversity,
    'How diverse are consumer condiment preferences?',
    'Market research and surveys on condiment preferences.',
    'Determines the extent to which standardized condiment offerings suppress individual tastes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preference_diversity, empirical, 'The diversity of consumer condiment preferences.').

omega_variable(
    distribution_network_accessibility,
    'How accessible are distribution networks to small condiment producers?',
    'Analysis of distribution contracts and supply chain dynamics.',
    'Influences the ability of small producers to compete with established brands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distribution_network_accessibility, empirical, 'Accessibility of distribution networks for small producers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(condiment_tyranny, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cond_tr_t0, condiment_tyranny, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cond_tr_t5, condiment_tyranny, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cond_tr_t10, condiment_tyranny, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cond_be_t0, condiment_tyranny, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cond_be_t5, condiment_tyranny, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cond_be_t10, condiment_tyranny, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(condiment_tyranny, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
