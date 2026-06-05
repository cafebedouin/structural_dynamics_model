% ============================================================================
% CONSTRAINT STORY: hilberts_hotel
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hilberts_hotel, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hilberts_hotel
 *   human_readable: Hilbert's Hotel Infinite Capacity Accommodation
 *   domain: technological
 *
 * SUMMARY:
 *   Hilbert's Hotel is a thought experiment illustrating the
 *   counter-intuitive properties of infinite sets. It posits a hotel with an
 *   infinite number of rooms, all of which are occupied. The paradox arises
 *   from the fact that even though the hotel is fully booked, it can always
 *   accommodate more guests by shifting existing guests to a higher numbered
 *   room (e.g., the guest in room 1 moves to room 2, the guest in room 2
 *   moves to room 3, and so on).
 *
 * KEY AGENTS:
 *   - Guests: Occupy the rooms (powerless/trapped)
 *   - Hotel Manager: Reallocates rooms (institutional/constrained)
 *   - Mathematician: Analyzes the possibility (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilberts_hotel, 0.1).
domain_priors:suppression_score(hilberts_hotel, 0.05).
domain_priors:theater_ratio(hilberts_hotel, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel, extractiveness, 0.1).
narrative_ontology:constraint_metric(hilberts_hotel, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hilberts_hotel, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hilberts_hotel, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hilberts_hotel, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hilberts_hotel, mountain).
narrative_ontology:human_readable(hilberts_hotel, "Hilbert's Hotel Infinite Capacity Accommodation").
narrative_ontology:topic_domain(hilberts_hotel, "technological").

domain_priors:emerges_naturally(hilberts_hotel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a purely mathematical standpoint, the infinite nature of the hotel's capacity means there are no constraints on new guests.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The structure is a consequence of set theory and cardinality. No physical instantiation could violate the abstract principle.
constraint_indexing:constraint_classification(hilberts_hotel, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hilberts_hotel, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilberts_hotel, ExtMetricName, E),
    domain_priors:suppression_score(hilberts_hotel, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hilberts_hotel),
    narrative_ontology:constraint_metric(hilberts_hotel, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hilberts_hotel, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hilberts_hotel_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because there is essentially no cost to accommodating new guests. Suppression is minimal due to the nature of infinity. Theater ratio is low as the functionality is primarily conceptual rather than practical.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives classify as mountain because the constraints stem from the mathematical structure. There's no difference in observed type.
 *
 * DIRECTIONALITY LOGIC:
 *   In this scenario, there isn't a clear directionality of power or benefit. New guests don't negatively affect the existing guests and the hotel manager is merely facilitating the mathematical function.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A - Classifies as a mountain, therefore mandatrophy is not relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hilberts_hotel, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
