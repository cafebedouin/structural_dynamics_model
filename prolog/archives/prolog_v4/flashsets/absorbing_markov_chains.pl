% ============================================================================
% CONSTRAINT STORY: absorbing_markov_chains
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absorbing_markov_chains, []).

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
 *   constraint_id: absorbing_markov_chains
 *   human_readable: State Space Trap in Absorbing Markov Chain
 *   domain: technological
 *
 * SUMMARY:
 *   An absorbing Markov chain possesses one or more absorbing states. Once
 *   the chain enters an absorbing state, it is impossible to leave. This is a
 *   fundamental property of the mathematical structure.
 *
 * KEY AGENTS:
 *   - Trapped State: Powerless/Trapped
 *   - System Designer: Institutional/Analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absorbing_markov_chains, 0.1).
domain_priors:suppression_score(absorbing_markov_chains, 0.0).
domain_priors:theater_ratio(absorbing_markov_chains, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, extractiveness, 0.1).
narrative_ontology:constraint_metric(absorbing_markov_chains, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(absorbing_markov_chains, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(absorbing_markov_chains, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absorbing_markov_chains, mountain).
narrative_ontology:human_readable(absorbing_markov_chains, "State Space Trap in Absorbing Markov Chain").
narrative_ontology:topic_domain(absorbing_markov_chains, "technological").

domain_priors:emerges_naturally(absorbing_markov_chains).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Once entered, the absorbing state is inescapable.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The system designer understands the properties of the absorbing Markov chain.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absorbing_markov_chains_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(absorbing_markov_chains, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absorbing_markov_chains, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(absorbing_markov_chains, ExtMetricName, E),
    domain_priors:suppression_score(absorbing_markov_chains, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(absorbing_markov_chains),
    narrative_ontology:constraint_metric(absorbing_markov_chains, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(absorbing_markov_chains, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(absorbing_markov_chains_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The chain's mathematical structure ensures that it remains in the absorbing state.
 *
 * PERSPECTIVAL GAP:
 *   Both the trapped state and the system designer view this as a fundamental limitation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absorbing_markov_chains, 0, 100).

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
