% ============================================================================
% CONSTRAINT STORY: conways_game_of_life_dynamics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

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
 *   Conway's Game of Life is a zero-player cellular automaton where simple local
 *   rules (survival, birth, death) applied to a 2D grid lead to complex emergent
 *   behaviors. This constraint represents the fixed, deterministic rule-set
 *   (B3/S23) itself, which functions as a fundamental law of its universe,
 *   independent of any observer.
 *
 * KEY AGENTS (by structural relationship):
 *   This is a uniform-type Mountain constraint (a mathematical law), so there are
 *   no structural beneficiaries or victims. Instead, agents are defined by how
 *   they interact with this unchangeable law:
 *   - The Individual Cell (powerless/trapped): An entity whose existence is
 *     entirely determined by the rules.
 *   - The Pattern Architect (institutional/mobile): A designer who uses the
 *     predictable nature of the rules to build complex computational structures.
 *   - The Analytical Predictor (powerless/constrained): An observer who finds
 *     the rules' emergent complexity computationally irreducible, making long-term
 *     prediction without simulation impossible.
 *   - The Analytical Observer (analytical/analytical): Sees the complete structure
 *     as a fixed mathematical object.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: As a set of mathematical rules, the base extractiveness and
% suppression are effectively zero. The values are set to a minimal non-zero
% value to represent the logical "cost" of enforcing determinism.
domain_priors:base_extractiveness(conways_game_of_life_dynamics, 0.05).
domain_priors:suppression_score(conways_game_of_life_dynamics, 0.05).
domain_priors:theater_ratio(conways_game_of_life_dynamics, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, extractiveness, 0.05).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: Within the universe of the game, the rules are absolute.
% No alternative physics is conceivable or accessible.
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, 1.0).
% Resistance: It is incoherent to "resist" a mathematical rule. One can only
% operate within its confines.
narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(conways_game_of_life_dynamics, mountain).

% --- Emergence flag (required for mountain constraints) ---
% The rules emerge from mathematical logic, not human design or enforcement.
% This is required for the mountain metric gate to fire.
domain_priors:emerges_naturally(conways_game_of_life_dynamics).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a uniform-type Mountain (mathematical law), there
% are no structural beneficiaries or victims. The rules are symmetric and apply
% universally within their domain.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL CELL
% For a cell, the rules are an absolute, unchangeable law of nature. Its fate
% is determined entirely by its local neighborhood, with no possibility of
% appeal or deviation. This is the classic Mountain perspective.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PATTERN ARCHITECT
% For a designer of complex patterns (like glider guns), the rules are not a
% tool to be wielded (Rope) but the fixed landscape upon which tools can be
% built. The rules themselves remain a Mountain; the architect's skill is in
% arranging initial conditions to achieve desired outcomes within those fixed laws.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL PREDICTOR
% For an observer trying to predict a pattern's ultimate fate, the rules'
% computational irreducibility feels like a Snare, extracting effort. However,
% the constraint is the rule-set itself, not the difficulty of prediction. The
% rules are still a fixed Mountain; the "snare-like" property is an emergent
% consequence (undecidability), which would be a separate constraint.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The default analytical context sees the rule-set as a pure mathematical
% object: fixed, deterministic, and unchangeable. This is the canonical
% Mountain classification.
constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conways_game_of_life_dynamics_tests).

test(classification_invariance) :-
    % Verify that this is a uniform-type Mountain, invariant across perspectives.
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(conways_game_of_life_dynamics, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify metrics are within the Mountain classification thresholds.
    domain_priors:base_extractiveness(conways_game_of_life_dynamics, E),
    domain_priors:suppression_score(conways_game_of_life_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    % Verify that the required metrics for NL certification are present.
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(conways_game_of_life_dynamics, resistance, R),
    domain_priors:emerges_naturally(conways_game_of_life_dynamics),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(conways_game_of_life_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Game of Life rules are modeled as a pure, uniform-type Mountain.
 *   The base extractiveness (0.05) and suppression (0.05) are set to minimal
 *   non-zero values, reflecting the logical "cost" of determinism (i.e., it
 *   "extracts" randomness and "suppresses" alternative physics). These values
 *   are well within the Mountain thresholds (ε ≤ 0.25, S ≤ 0.05).
 *   The constraint passes the Natural Law certification chain by declaring
 *   `emerges_naturally`, `accessibility_collapse` (1.0, as no other physics
 *   is possible within the system), and `resistance` (0.0, as resistance is
 *   incoherent).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The constraint is a Mountain from all
 *   perspectives. The different experiences of the "architect" (who uses the
 *   law) or the "predictor" (who is stymied by it) are consequences of
 *   interacting with the Mountain, not different classifications of the law
 *   itself. A more complex model would decompose these consequences into
 *   separate, linked constraints (e.g., a Rope for 'computation_via_gliders'
 *   and a Snare for 'pattern_undecidability').
 *
 * DIRECTIONALITY LOGIC:
 *   As a uniform-type Mountain, there are no structural beneficiaries or
 *   victims, so no directionality is derived. The rules apply symmetrically
 *   to all entities within the system.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a pure Mountain, we avoid mislabeling a fundamental
 *   law as having an extractive or coordinative function. The coordination
 *   (Rope) and extraction (Snare) are emergent phenomena built *on top of*
 *   the Mountain, not properties of the Mountain itself. This maintains a
 *   clear distinction between foundational rules and their application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_conways_game_of_life_dynamics,
    'Does the classification as a Mountain hold for all possible cellular automata rule-sets, or is B3/S23 uniquely stable?',
    'A systematic survey of the rule space (e.g., using computational complexity metrics) to determine if other simple rules generate similarly stable, complex universes.',
    'If B3/S23 is unique, it remains a Mountain. If many such rule-sets exist, the choice of B3/S23 could be seen as a weak Rope (a convention).',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(conways_game_of_life_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal measurements are required. The base extractiveness (0.05) is
% below the 0.46 threshold for mandatory drift detection. As a mathematical
% constant, its properties do not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(conways_game_of_life_dynamics, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(conways_game_of_life_dynamics, 0.05).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(conways_game_of_life_dynamics, halting_problem_undecidability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed. As a uniform-type Mountain with no
% declared beneficiaries or victims, the directionality derivation chain is
% not engaged.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */