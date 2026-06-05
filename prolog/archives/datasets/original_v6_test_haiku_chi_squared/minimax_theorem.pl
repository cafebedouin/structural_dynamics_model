% ============================================================================
% CONSTRAINT STORY: minimax_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_minimax_theorem, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: minimax_theorem
 *   human_readable: Minimax Theorem Enforcement in Strategic Games
 *   domain: economic/game_theory/mathematical_economics
 *
 * SUMMARY:
 *   The minimax theorem is a mathematical law: for every finite zero-sum
 *   game, there exists a mixed strategy profile where each player's payoff
 *   equals their maxmin value (the best they can guarantee against any
 *   opponent strategy). Von Neumann's theorem (1928) establishes this as a
 *   consequence of linear algebra and convex analysis. The theorem is not
 *   enforced by any regulatory body, discovered through experiment, or
 *   subject to measurement error — it is a logical necessity that follows
 *   from the structure of finite games. Every perspective — from the
 *   mathematician proving it, to the game designer building games, to the
 *   strategic player executing strategies, to the losing player suffering its
 *   consequences — encounters the minimax theorem as an immutable constraint.
 *   No player can do better than their maxmin; no designer can create a game
 *   that violates it; no alternative framing circumvents it. This is the
 *   canonical example of a constraint with zero degrees of freedom.
 *
 * KEY AGENTS:
 *   - Von Neumann and Mathematical Tradition: Discoverers (institutional/arbitrage) — established the theorem through rigorous proof; experience it as mathematical necessity
 *   - Game Designers and Mechanism Designers: Institutional actors (institutional/arbitrage) — work within the constraint; cannot violate it but can exploit its properties
 *   - Strategic Players (Competitors): Moderate agents (moderate/constrained) — encounter the minimax principle empirically as a lower bound on their payoffs
 *   - Losing Players: Victims in any specific game (powerless/trapped) — experience the constraint as fate; cannot escape their maxmin payoff
 *   - Economics Profession: Analytical consensus (analytical/analytical) — treats the minimax theorem as foundational; all game theory builds on it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(minimax_theorem, 0.08).
domain_priors:suppression_score(minimax_theorem, 0.02).
domain_priors:theater_ratio(minimax_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(minimax_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(minimax_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(minimax_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(minimax_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(minimax_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(minimax_theorem, mountain).
narrative_ontology:human_readable(minimax_theorem, "Minimax Theorem Enforcement in Strategic Games").
narrative_ontology:topic_domain(minimax_theorem, "economic/game_theory/mathematical_economics").

domain_priors:emerges_naturally(minimax_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL PROOF (MOUNTAIN) — The minimax theorem is a formal consequence of linear algebra and convex analysis. Von Neumann's proof (1928) establishes that for any finite zero-sum game, a mixed-strategy Nash equilibrium exists. This is true regardless of the number of players, strategy space size, or payoff magnitudes. Mathematical necessity, not contingent institutional arrangement. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The theorem is invariant across all observables.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: GAME DESIGNER (MOUNTAIN) — A player designing a zero-sum game encounters the minimax theorem as an absolute constraint on what equilibrium outcomes are possible. No matter how skillfully they design payoffs, the theorem guarantees that an equilibrium exists at the intersection of each player's maxmin strategy. They cannot escape it through clever design; they can only work within its structure. d=0.50, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: STRATEGIC PLAYER (MOUNTAIN) — An agent competing in a zero-sum game (chess, poker, military conflict modeled as game theory) discovers the minimax principle empirically: their opponent has a guaranteed strategy that prevents them from doing better than the equilibrium payoff. No amount of cleverness, deception, or resource advantage can overcome this lower bound. The constraint is invisible but absolute. d=0.85, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ECONOMICS PROFESSION (MOUNTAIN) — The minimax theorem is foundational to game theory as a discipline. All subsequent work in strategic analysis (cooperative games, auction design, mechanism design, behavioral game theory) builds on the assumption that zero-sum equilibria exist and can be computed. The theorem is not enforced by any authority; it is discovered as a mathematical fact. Professional practice embeds it because deviation is logically impossible. d=0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.004. Negligible effective extraction.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LOSING PLAYER (MOUNTAIN) — A player in a zero-sum game who loses discovers the minimax theorem as fate: their opponent found the equilibrium strategy, and no escape exists. The constraint operates through pure mathematical necessity. Even if they had perfect information and unlimited resources, the theorem guarantees they cannot exceed their minimax payoff. d=0.95, f(d)≈1.42, σ=0.8 → χ≈0.11. High directionality but low base extractiveness makes χ still low.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(minimax_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(minimax_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(minimax_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(minimax_theorem, ExtMetricName, E),
    domain_priors:suppression_score(minimax_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(minimax_theorem),
    narrative_ontology:constraint_metric(minimax_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(minimax_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(minimax_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The minimax theorem does not extract value from one agent to another — it is a constraint on all agents equally. Each player receives exactly their maxmin payoff; the game is zero-sum by definition. The nonzero value (0.08 rather than 0.0) reflects measurement precision: the theorem's proof involves floating-point approximations in computational applications, and some game implementations use discrete approximations. But the theoretical constraint has ε → 0. Suppression (0.02): Minimal. There is no suppression mechanism. Players are not prevented from discovering the minimax principle; they are prevented by mathematical necessity from exceeding it. The small value reflects that explicit knowledge of the theorem may be asymmetric (one player knows game theory, the other does not), but this is information advantage, not suppression in the constraint sense. Theater ratio (0.15): Very low. Zero theater. The minimax theorem has no performative content. When applied, it is purely functional: compute equilibrium strategies, execute them, receive payoff. No ritual, no symbolic compliance, no display.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the same classification (mountain) because the constraint is mathematically invariant. However, the directionality and time horizons differ. The mathematician sees a civilizational truth (d=0.72, analytical). The designer sees a structural boundary they must respect (d=0.50, powerful). The strategic player sees an empirical discovered-at-play constraint (d=0.85, moderate). The losing player sees it as fate (d=0.95, powerless). The profession sees it as foundational (d=0.10, institutional). All perspectives produce mountain, confirming that the minimax theorem is a uniform-type constraint: it classifies identically from every index. This is the definitive feature of a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The minimax theorem has no beneficiaries and no victims in the structural sense because it applies equally to all players. In any specific game instance, one player wins and one loses relative to the equilibrium, but the minimax theorem guarantees that outcome — it is not an extraction mechanism, but a fair division rule embedded in game structure. The losing player's directionality is high (d=0.95) not because they are exploited, but because they have no exit: the minimax constraint applies regardless of their wishes. The beneficiary's directionality is low (d=0.10) not because they extract unfair advantage, but because they have arbitrage options: they can leave the game, change its structure, or reframe the payoffs. But neither directionality difference creates extraction (χ remains ≈0.08 across all perspectives) because the base extractiveness is mathematically zero.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(minimax_theorem, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(minimax_theorem, information_standard).
narrative_ontology:affects_constraint(minimax_theorem, nash_equilibrium_existence).
narrative_ontology:affects_constraint(minimax_theorem, zero_sum_payoff_structure).

% DUAL FORMULATION NOTE:
% The minimax theorem is upstream of all equilibrium-based game theory. Nash's 1950 generalization (existence of Nash equilibrium in all finite games, not just zero-sum) is a distinct constraint with higher extractiveness (ε≈0.15) because it allows non-zero-sum games where bargaining and coalition formation introduce structural extraction. The minimax theorem is the special case where extraction is mathematically eliminated by the zero-sum payoff structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
