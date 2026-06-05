% ============================================================================
% CONSTRAINT STORY: hydra_game
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hydra_game, []).

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
 *   constraint_id: hydra_game
 *   human_readable: The Hydra Game (Kirby-Paris Theorem)
 *   domain: technological
 *
 * SUMMARY:
 *   The Hydra Game is a mathematical game on a rooted tree. The game involves
 *   a Hydra, represented as a finite rooted tree, and a hero who chops off
 *   heads of the Hydra. For each head chopped, the Hydra grows new heads
 *   according to certain rules. Despite the Hydra appearing to grow without
 *   bound, the Kirby-Paris theorem demonstrates that no matter how the hero
 *   chops, the Hydra will eventually be reduced to nothing. This outcome is
 *   counterintuitive, as it cannot be proven within Peano arithmetic, a
 *   standard axiom system for number theory.
 *
 * KEY AGENTS:
 *   - Mathematician: Analytical observer (analytical/analytical) - understands the underlying mathematical theorem
 *   - Hydra Node: Subject to the rules (powerless/trapped) - experiences the game deterministically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hydra_game, 0.1).
domain_priors:suppression_score(hydra_game, 0.01).
domain_priors:theater_ratio(hydra_game, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hydra_game, extractiveness, 0.1).
narrative_ontology:constraint_metric(hydra_game, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(hydra_game, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hydra_game, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hydra_game, mountain).
narrative_ontology:human_readable(hydra_game, "The Hydra Game (Kirby-Paris Theorem)").
narrative_ontology:topic_domain(hydra_game, "technological").

domain_priors:emerges_naturally(hydra_game).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The mathematician views the Hydra game as a manifestation of the Kirby-Paris theorem, a result in mathematical logic that demonstrates the independence of certain combinatorial statements from Peano arithmetic. The game's termination, while counterintuitive, is guaranteed by this theorem, which is a fundamental property of the underlying mathematical structure.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of an individual node in the Hydra tree, the rules of the game and the process of its evolution are fixed and unchangeable. The node is trapped within the game's mechanics and experiences the unfolding sequence as a deterministic process.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hydra_game_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hydra_game, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hydra_game, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hydra_game, ExtMetricName, E),
    domain_priors:suppression_score(hydra_game, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hydra_game),
    narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hydra_game, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hydra_game_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The game's termination is a mathematical certainty due to the Kirby-Paris theorem. The extractiveness is low because the game itself doesn't extract resources or impose coercive rules.
 *
 * PERSPECTIVAL GAP:
 *   The mathematician views the Hydra game as a manifestation of the Kirby-Paris theorem, a fundamental result in mathematical logic, highlighting its termination as a consequence of the underlying mathematical structure. From the Hydra's perspective, the game unfolds deterministically, governed by fixed rules, without agency or escape.
 *
 * DIRECTIONALITY LOGIC:
 *   The analytical perspective (mathematician) experiences no extraction or suppression; this is a discovered property of a formal system. The 'powerless' hydra node experiences the unfolding game deterministically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a Mountain from both perspectives because the termination of the Hydra Game is a mathematical certainty guaranteed by the Kirby-Paris theorem. This mathematical truth exists independently of any agent's actions or beliefs. Mandatrophy is resolved because of mathematical certainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hydra_game, 0, 100).

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
