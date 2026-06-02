% ============================================================================
% CONSTRAINT STORY: conways_game_of_life_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: conways_game_of_life_dynamics
 *   human_readable: Conway's Game of Life Dynamics
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   Conway's Game of Life is a zero-player cellular automaton created by John
 *   Conway in 1970. It operates on a 2D grid where each cell is either alive
 *   or dead. At each time step, the state of each cell is determined by four
 *   simple rules: (1) a live cell with 2-3 live neighbors survives; (2) a
 *   live cell with fewer than 2 or more than 3 neighbors dies; (3) a dead
 *   cell with exactly 3 live neighbors becomes alive; (4) all other cells
 *   remain dead. Despite these minimal rules, the system generates rich
 *   emergent behavior: stable patterns (blocks, beehives), oscillators
 *   (blinkers, toads), moving patterns (gliders), and complex
 *   metaconstructions (gossip guns, universal Turing machines). The
 *   constraint is the rule set itself: no agent, context, or interpretation
 *   can escape the logical consequences of these four rules. This makes Game
 *   of Life a paradigmatic mountain constraint — a natural law of the
 *   mathematical domain it inhabits.
 *
 * KEY AGENTS:
 *   - The Mathematical Rules: The constraint itself (institutional/arbitrage) — defines the problem space; no agent can modify or escape them
 *   - Computational Researchers: Observers of emergent patterns (powerful/mobile) — can compute, visualize, and analyze, but cannot alter the underlying dynamics
 *   - Students and Educators: Learners within the system (moderate/constrained) — encounter the rules as fundamental; can understand but not negotiate
 *   - Academic Institutions: Beneficiaries of research and pedagogy (institutional/arbitrage) — benefit from publications and prestige, but constrained by the immutable rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conways_game_of_life_dynamics, 0.08).
domain_priors:suppression_score(conways_game_of_life_dynamics, 0.02).
domain_priors:theater_ratio(conways_game_of_life_dynamics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, extractiveness, 0.08).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, 0.08).

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

% PERSPECTIVE 1: MATHEMATICAL ANALYST (MOUNTAIN) — From the perspective of formal logic and mathematics, the dynamics of Conway's Game of Life are an inescapable logical consequence of the four birth/survival/death rules applied deterministically to a 2D grid. The emergence of complex patterns (gliders, blinkers, gospers) from simple local rules is a mathematical fact, not subject to negotiation, suppression, or alternative interpretation. The constraint is the rule set itself — it permits no degrees of freedom once initial conditions are specified.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL RESEARCHER (MOUNTAIN) — A researcher investigating Game of Life dynamics faces an unchangeable constraint: the four rules are axiomatic. No amount of funding, institutional pressure, or alternative methodology can alter the fact that a live cell with 2-3 neighbors survives, or that a dead cell with exactly 3 neighbors births a live cell. The researcher can compute, visualize, and analyze patterns, but cannot change the underlying dynamical rules. The mountain persists across all attempts to observe or manipulate.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: STUDENT (MOUNTAIN) — A student learning cellular automata discovers that Game of Life rules are non-negotiable: they cannot wish away the emergence of complex patterns, cannot opt out of the logical consequences of the rules, and cannot find an alternative formalism that avoids the constraint. The rules are presented as fundamental — barriers to exit from this learning context are low, but the constraint itself is immutable.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC INSTITUTION (MOUNTAIN) — Universities teaching or researching cellular automata recognize that Conway's Game of Life rules are invariant across all institutional contexts. A computer science department cannot negotiate with the mathematics of emergence. The constraint defines the problem space within which research occurs. The institution benefits from teaching and research on Game of Life (publication, prestige), but cannot modify the underlying rules — it can only explore their consequences.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

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
 *   Extractiveness (0.08): Very low. The Game of Life rules do not extract value from any agent — they are logical constraints, not institutional mechanisms. The rules simply define the problem space. No agent is made worse off by the existence of the rules; rather, they enable computational and mathematical exploration. Suppression (0.02): Minimal. There are no alternatives to suppress; the rules do not constrain agent choices because they define a mathematical domain, not a social or economic system. No agent is forced into the Game of Life; it is studied voluntarily. Theater ratio (0.15): Low. The presentation of Game of Life rules is straightforward and non-performative. Teaching may include visualization, animation, or pedagogical narrative, but the core rules are stated plainly as axioms. The theater reflects only the inevitable gap between formal statements and intuitive explanation.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain with near-perfect unanimity. This is the hallmark of a true natural law: no matter the observer's power, time horizon, exit options, or spatial scope, the constraint is perceived identically. The mathematical analyst sees immutable logic. The computational researcher sees unchangeable dynamics. The student sees non-negotiable rules. The institution sees an invariant problem space. The absence of perspectival disagreement is diagnostic evidence of mountainhood — if the constraint were social or economic in nature, we would expect beneficiaries and victims to perceive it differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The standard directionality derivation (beneficiary/victim + exit options → d) does not apply to mountain constraints. Game of Life has no beneficiaries or victims — it is a pure logical constraint. The engine derives d uniformly across all perspectives from the canonical fallback for the power atom. For analytical contexts (d ≈ 0.73), the high f(d) ≈ 1.15 would suggest high experienced extractiveness, but the mountain classification gates on accessibility_collapse ≥ 0.85 and resistance ≤ 0.15, which override any extractiveness scaling. The constraint is mountain-identified by its intrinsic properties, not by directionality derivation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    computational_universality_threshold,
    'At what grid scale and configuration complexity do the emergent computational properties of Game of Life become undecidable?',
    'Formal proof of Turing completeness threshold; identification of specific patterns that encode halting-problem instances',
    'If threshold is low (small grid, few patterns): universality is a fundamental feature of the rules. If threshold is high: universality is an artifact of scale and may not apply to physically realizable systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_universality_threshold, empirical, 'Whether Game of Life universality requires unbounded computational resources').

omega_variable(
    pattern_emergence_predictability,
    'Can the long-term behavior of arbitrary Game of Life configurations be predicted without simulation, or is the problem fundamentally uncomputable?',
    'Proof or disproof of the decidability of the Game of Life halting problem; algorithmic analysis of pattern classes',
    'If decidable: Game of Life is analyzable in principle, even if computation is hard. If undecidable: the mountain constraint includes irreducible computational unpredictability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pattern_emergence_predictability, empirical, 'Whether Game of Life behavior is computationally decidable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conways_game_of_life_dynamics, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgol_tr_t0, conways_game_of_life_dynamics, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cgol_tr_t25, conways_game_of_life_dynamics, theater_ratio, 25, 0.15).
narrative_ontology:measurement(cgol_tr_t50, conways_game_of_life_dynamics, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(cgol_be_t0, conways_game_of_life_dynamics, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cgol_be_t25, conways_game_of_life_dynamics, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(cgol_be_t50, conways_game_of_life_dynamics, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conways_game_of_life_dynamics, information_standard).
narrative_ontology:affects_constraint(conways_game_of_life_dynamics, cellular_automaton_universality).
narrative_ontology:affects_constraint(conways_game_of_life_dynamics, emergence_and_reducibility).
narrative_ontology:affects_constraint(conways_game_of_life_dynamics, computational_halting_problem).

% DUAL FORMULATION NOTE:
% Game of Life is upstream of broader constraints on cellular automaton behavior and computational universality. The four rules are fundamental; they are not downstream of more primitive constraints. Claims about the 'simplicity' vs 'complexity' of Game of Life behavior depend on measurement (state space enumeration vs visual complexity vs computational expressiveness), but the underlying rules remain invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
