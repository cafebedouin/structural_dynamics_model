% ============================================================================
% CONSTRAINT STORY: minimax_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: minimax_theorem
 *   human_readable: Minimax Theorem Enforcement in Strategic Games
 *   domain: economic/game_theory
 *
 * SUMMARY:
 *   The Minimax Theorem, proved by John von Neumann in 1928, establishes that
 *   for any finite, zero-sum, two-player game, there exists a Nash
 *   equilibrium in mixed strategies where each player's payoff equals their
 *   minimax value — the value they can guarantee against an optimal opponent.
 *   This theorem is a cornerstone of game theory and economic equilibrium
 *   analysis. From a Deferential Realism perspective, the minimax theorem
 *   represents a natural law constraint: a logical necessity that emerges
 *   from the formal structure of simultaneous-move games with perfect payoff
 *   information. No agent can escape the constraint through strategic
 *   innovation, coalition formation, or institutional redesign. The
 *   constraint applies uniformly across all zero-sum game contexts — poker,
 *   zero-sum trading, competitive bidding, military strategy, sports
 *   competition — and is invariant across all observable measurement
 *   methodologies. The accessibility collapse is near-total (0.92):
 *   understanding the minimax theorem requires only knowledge of game theory,
 *   available to any agent with access to educational resources or
 *   computational tools. The resistance to the constraint is minimal (0.08):
 *   agents cannot resist the logical necessity of the theorem, though they
 *   may fail to compute or implement the equilibrium due to computational
 *   complexity.
 *
 * KEY AGENTS:
 *   - The Player in Zero-Sum Competition: Constrained by the theorem to converge to minimax equilibrium (powerless/analytical) — cannot escape rational response logic
 *   - The Opponent: Equally constrained by symmetric minimax structure (powerless/analytical) — locked into best-response dynamics
 *   - The Game Theorist: Observes the theorem as a mathematical fact (analytical/analytical) — sees logical necessity, not contingent coordination
 *   - The Market Institution: Enforces zero-sum payoff structure (institutional/analytical) — constraint applies regardless of institutional form
 *   - The Practitioner: Experiences the constraint in strategic competition (moderate/biographical) — optimal play approximates minimax equilibrium over time
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
narrative_ontology:constraint_metric(minimax_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(minimax_theorem, mountain).
narrative_ontology:human_readable(minimax_theorem, "Minimax Theorem Enforcement in Strategic Games").
narrative_ontology:topic_domain(minimax_theorem, "economic/game_theory").

domain_priors:emerges_naturally(minimax_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLAYER CONSTRAINED BY RATIONALITY (MOUNTAIN) — An agent in a zero-sum game cannot escape the minimax equilibrium through any strategy or coalition. The constraint emerges from the logical structure of simultaneous-move games with perfect payoff information. No degree of freedom exists; the theorem guarantees the existence and uniqueness of mixed-strategy equilibrium regardless of the player's preferences, bargaining power, or coalition options. The constraint is invariant across all observable measurement approaches.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: FORMAL ANALYST (MOUNTAIN) — From the perspective of game-theoretic analysis, the minimax theorem is a mathematical fact: for any finite, zero-sum, two-player game, a Nash equilibrium exists and can be computed via linear programming or iterative methods. The theorem's existence proof is independent of institutional arrangements, enforcement mechanisms, or player behavior. It is a logical necessity, not a contingent coordination mechanism.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL IMPLEMENTER (MOUNTAIN) — Markets, auctions, and strategic competition operate under the minimax constraint universally. Even institutions that attempt to circumvent equilibrium through coordination, collusion, or regulatory intervention cannot escape the theorem's logical necessity. The constraint applies to any strategic interaction with zero-sum payoff structure, regardless of institutional form.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: PRACTITIONER IN COMPETITION (MOUNTAIN) — A business competitor or trader in zero-sum markets (commodities futures, zero-sum betting, head-to-head auctions) experiences the minimax constraint as an immutable structure: their optimal strategy is mathematically determined by the opponent's rational response. No amount of bargaining, side-payments, or coalition-building changes the underlying equilibrium structure for symmetric zero-sum games.
constraint_indexing:constraint_classification(minimax_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

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
 *   Extractiveness (0.08): The minimax theorem does not extract value from one agent to another in the manner of snares or tangled ropes. Rather, it distributes payoffs according to strategic competence and payoff structure. The low extractiveness reflects that no agent gains structural asymmetry from the constraint — both players converge to mathematically identical equilibrium values under their respective strategies. The slight nonzero value (0.08 rather than 0.00) accounts for the theoretical necessity that in practice, one agent's gain is exactly mirrored by the other's loss, with computational implementation costs. Suppression (0.02): Minimal suppression. Agents can freely explore all mixed strategies; the constraint does not block alternatives but rather determines which alternatives are optimal. The theorem guarantees that experimentation converges to equilibrium, so 'suppression' is not a relevant structural feature. Theater ratio (0.15): Very low. The minimax constraint is functional, not performative. The computational and strategic analysis required to implement equilibrium is directly instrumental — it serves prediction and optimization, not legitimation or ritual. The slight nonzero value reflects that practical game implementation may include some theatrical elements (e.g., announcing strategic randomization to preserve credibility), but the core constraint is purely logical.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, all four perspectives converge on the same classification (Mountain) with minimal perspectival gap. This uniformity is diagnostic of a true natural law constraint: the minimax theorem applies to all agents, all time horizons, and all institutional contexts identically. The powerless player and the analytical observer reach the same conclusion — the constraint is universal and inescapable. The institutional actor cannot circumvent the theorem through regulatory or organizational design. Even the practitioner with biographical time horizons eventually converges to minimax play through learning and market pressure. The absence of perspectival disagreement confirms the mountain classification: all observation angles reveal the same logical structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The minimax theorem is immune to mandatrophy concerns because it does not conflate coordination with extraction. The theorem proves that a unique (up to indifference) equilibrium exists where each player's payoff equals their minimax value — exactly what they can guarantee against rational opposition. There is no hidden extraction or coordination surplus. The constraint is neither a snare (no agent is worse off than they could guarantee themselves) nor a rope masquerading as a snare. The theorem's logical necessity precludes mislabeling. The only residual uncertainty (captured in the omegas) concerns boundary cases: games with incomplete information, computational intractability, and behavioral deviation. But in the core case of finite, zero-sum, perfect-information games, the minimax theorem is a pure logical constraint with no mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_accessibility_gap,
    'Does the gap between theoretical existence (guaranteed by minimax) and practical computation of equilibrium constitute an effective escape from the constraint?',
    'Empirical comparison: outcomes in real strategic games vs minimax predictions; measurement of computational complexity barriers vs actual agent behavior',
    'If gap is material: the constraint may degrade to Rope or Scaffold (agents use heuristics and coordination). If gap is negligible: Mountain classification confirmed (agents approximate equilibrium despite computational cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_accessibility_gap, empirical, 'Whether computational intractability of minimax creates practical escape from the constraint').

omega_variable(
    information_structure_sensitivity,
    'Does the minimax theorem''s guarantee hold when agents have asymmetric or incomplete information?',
    'Comparison of theoretical minimax equilibria with empirical outcomes in games with information asymmetry; examination of Bayesian game theory extensions',
    'If theorem fails under asymmetric info: the mountain classification applies only to perfect-information subclass. If theorem extends: constraint is more universal than initially classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_structure_sensitivity, conceptual, 'Whether minimax extends beyond perfect-information games').

omega_variable(
    behavioral_deviation_prevalence,
    'How frequently do real agents deviate from minimax equilibrium in experimental and field settings?',
    'Meta-analysis of behavioral game theory experiments; measurement of deviation rates across agent sophistication levels',
    'If deviations are rare (< 10%): Mountain classification confirmed. If deviations are systematic (> 30%): constraint may be Piton (theoretically mandated but behaviorally degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_deviation_prevalence, empirical, 'Empirical frequency of deviation from minimax play in strategic games').

omega_variable(
    zero_sum_boundary_ambiguity,
    'Where does the constraint boundary lie for games that are nearly zero-sum, or zero-sum in some aspects but not others?',
    'Analysis of equilibrium robustness as payoff structure deviates from perfect zero-sum (epsilon-zero-sum games); measurement of minimax prediction accuracy',
    'If minimax predictions degrade sharply: constraint applies only to mathematically pure zero-sum. If predictions remain robust: constraint extends to quasi-zero-sum and mixed-motive games.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_boundary_ambiguity, conceptual, 'Applicability of minimax to approximately zero-sum games').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(minimax_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minimax_tr_t0, minimax_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(minimax_tr_t50, minimax_theorem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(minimax_tr_t100, minimax_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(minimax_be_t0, minimax_theorem, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(minimax_be_t50, minimax_theorem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(minimax_be_t100, minimax_theorem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(minimax_theorem, information_standard).
narrative_ontology:affects_constraint(minimax_theorem, nash_equilibrium_existence).
narrative_ontology:affects_constraint(minimax_theorem, zero_sum_game_dynamics).
narrative_ontology:affects_constraint(minimax_theorem, strategic_dominance).

% DUAL FORMULATION NOTE:
% The minimax theorem is a special case of the broader Nash equilibrium existence theorem. This constraint story focuses on zero-sum games specifically; the upstream Nash constraint covers general n-player games. The minimax theorem's stronger guarantee (existence of pure strategy minimax values plus mixed strategy equilibrium) is downstream of but more restrictive than Nash existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
