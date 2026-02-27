% ============================================================================
% CONSTRAINT STORY: conways_game_of_life_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conways_game_of_life_dynamics, []).

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
 *   constraint_id: conways_game_of_life_dynamics
 *   human_readable: Conway's Game of Life Dynamics
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   Conway's Game of Life is a zero-player cellular automaton where simple
 *   local rules (survival, birth, death) applied to a 2D grid lead to complex
 *   emergent behaviors. The game exhibits a wide range of patterns, including
 *   still lifes, oscillators, and spaceships. The dynamics of the game are
 *   fully deterministic, given the initial state and the rules.
 *
 * KEY AGENTS:
 *   - Analytical Observer: views the system from a detached, mathematical perspective
 *   - Individual Cell: passively reacts to its neighbors according to fixed rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conways_game_of_life_dynamics, 0.1).
domain_priors:suppression_score(conways_game_of_life_dynamics, 0.01).
domain_priors:theater_ratio(conways_game_of_life_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, extractiveness, 0.1).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conways_game_of_life_dynamics, mountain).
narrative_ontology:human_readable(conways_game_of_life_dynamics, "Conway's Game of Life Dynamics").
narrative_ontology:topic_domain(conways_game_of_life_dynamics, "mathematical/computational").

domain_priors:emerges_naturally(conways_game_of_life_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the emergent dynamics of Conway's Game of Life represent a fixed set of computational possibilities given the initial conditions and rules. These dynamics are a consequence of the specified rules and are not subject to alteration. Therefore, it is a mountain.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% An individual cell within the grid has no control over its state; it is entirely determined by its neighbors and the rules of the game. Trapped in the local dynamics. Sees it as a fixed rule.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conways_game_of_life_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(conways_game_of_life_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(conways_game_of_life_dynamics),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(conways_game_of_life_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.10): Minimal extraction. The dynamics arise solely from the initial conditions and fixed rules; no agent actively extracts from any other. Suppression (0.01): Near-zero suppression. No alternative behaviors are possible, given the game's deterministic nature. Theater ratio (0.05): Minimal theater. The simulation evolves solely based on the fixed rules, with no performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives see the dynamics as fixed. The analytical observer understands the global implications of these fixed rules, while the individual cell experiences these rules directly and locally. The perspectives converge on the interpretation as a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The dynamics are an inherent consequence of the rules and initial conditions, not actively driven by agents. Therefore, directionality is not a primary factor in this classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The game's dynamics could not be mistaken for a Snare because there is no identifiable agent that actively extracts resources or suppresses alternatives. The dynamics are a consequence of fixed rules and cannot be manipulated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conways_game_of_life_dynamics, 0, 100).

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
