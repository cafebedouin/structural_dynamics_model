% ============================================================================
% CONSTRAINT STORY: roman_bath_system
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_bath_system, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_bath_system
 *   human_readable: The Roman System of Public Baths
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Roman Empire developed a massive, state-subsidized network of public
 *   baths (thermae) that provided hygiene, recreation, and social centers for
 *   the populace at little to no cost. This system fostered social cohesion,
 *   improved public health, and acted as a display of imperial power.
 *
 * KEY AGENTS:
 *   - Roman Citizens: Primary beneficiaries (powerless/mobile) - Gained access to hygiene and social interaction.
 *   - Roman State: Secondary beneficiary (institutional/arbitrage) - Benefited from improved public health and social cohesion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_bath_system, 0.15).
domain_priors:suppression_score(roman_bath_system, 0.1).
domain_priors:theater_ratio(roman_bath_system, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_bath_system, extractiveness, 0.15).
narrative_ontology:constraint_metric(roman_bath_system, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(roman_bath_system, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_bath_system, rope).
narrative_ontology:human_readable(roman_bath_system, "The Roman System of Public Baths").
narrative_ontology:topic_domain(roman_bath_system, "technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_bath_system, roman_citizens).
narrative_ontology:constraint_beneficiary(roman_bath_system, roman_state).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The average citizen benefits from access to hygiene and social interaction, with the ability to move between different baths or choose to forgo them entirely.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% The Roman state benefits from improved public health and social cohesion, with the ability to adjust the bath system as needed.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a historical perspective, the bath system served as a widespread social coordination mechanism across the Roman Empire, facilitating communication and cultural exchange.
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_bath_system_tests).
:- end_tests(roman_bath_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. The bath system was heavily subsidized, providing significant benefits to citizens at minimal direct cost. The extraction that did occur was largely in the form of taxes, which were used to fund the construction and maintenance of the baths, benefitting society as a whole. Suppression (0.10): Low. While the state provided and maintained the bath system, citizens were generally free to choose whether or not to use them, with limited coercion involved. Theater ratio (0.30): Low. The baths were highly functional, providing genuine benefits to the population in terms of hygiene and social interaction. While there was a certain level of grandeur and display associated with some of the larger bath complexes, this was secondary to their practical function.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the system as a rope as the Roman bath system was primarily a coordination mechanism, facilitating hygiene, social interaction, and cultural exchange at a minimal cost to individuals and with clear benefits for the Roman state.
 *
 * DIRECTIONALITY LOGIC:
 *   Roman citizens (powerless, mobile) benefit through public amenities and socialization. The roman state (institutional, arbitrage) leverages the bath system as infrastructure for power display and public health. The historian (analytical, analytical) recognizes the empire-wide coordinating function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a rope from all perspectives, reflecting its primary function as a social coordination mechanism. This prevents mislabeling as extraction because the actual goal of the Roman bath system was social and economic benefit to the citizens of Rome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_bath_system, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_bath_system, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
