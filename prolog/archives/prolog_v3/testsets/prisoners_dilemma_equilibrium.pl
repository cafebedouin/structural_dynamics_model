% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_prisoners_dilemma_equilibrium, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    omega_variable/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prisoners_dilemma_equilibrium
 *   human_readable: The Prisoner's Dilemma (Nash Equilibrium)
 *   domain: logical/economic
 *
 * SUMMARY:
 *   The Prisoner's Dilemma is a foundational concept in game theory demonstrating
 *   why two rational individuals might not cooperate, even if it appears to be in
 *   their best interest. The constraint is the Nash Equilibrium itself—the logical
 *   gravity that pulls isolated, self-interested agents toward mutual defection.
 *   This equilibrium is a structural limit on non-cooperative games, functioning
 *   as a Mountain of logic.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Prisoner: Primary subject (powerless/trapped) — bears the cost of lost cooperation.
 *   - The System Designer: Institutional observer (institutional/arbitrage) — observes the constraint to design around it.
 *   - The Game Theorist: Analytical observer (analytical/analytical) — sees the full logical structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The dilemma's logical structure itself is not extractive; it merely
% describes a scenario where potential value is lost. The extraction occurs in
% real-world instantiations, not the abstract model. This is a pure Mountain.
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.10).
% Rationale: The equilibrium doesn't suppress alternatives through coercion,
% but through pure logic, making cooperation appear irrational.
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.05).
% Rationale: The concept is purely functional, with no performative aspect.
domain_priors:theater_ratio(prisoners_dilemma_equilibrium, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.10).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in structural_signatures.pl.
% Accessibility Collapse: The logical path to defection is almost total, though
% cooperation is conceivable (e.g., in iterated games).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, 0.95).
% Resistance: One cannot "resist" a logical equilibrium.
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
% The Nash Equilibrium is presented as a logical, mathematical inevitability.
narrative_ontology:constraint_claim(prisoners_dilemma_equilibrium, mountain).
narrative_ontology:human_readable(prisoners_dilemma_equilibrium, "The Prisoner's Dilemma (Nash Equilibrium)").
narrative_ontology:topic_domain(prisoners_dilemma_equilibrium, "logical/economic").

% --- Emergence flag (required for mountain constraints) ---
% The equilibrium emerges from the mathematical structure of the game without
% human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(prisoners_dilemma_equilibrium).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain, this constraint is a feature of the
% logical landscape and does not have beneficiaries or victims in the sense
% of a designed system. All agents face the same immutable structure.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   As a Mountain, the classification is invariant across all perspectives.
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED PRISONER (MOUNTAIN)
% For the prisoner, the incentive to defect is a natural law of survival.
% The logic of the payoff matrix is an unyielding, inescapable Mountain.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SYSTEM DESIGNER (MOUNTAIN)
% For a legislator or system designer, the dilemma is a fixed feature of the
% landscape (a Mountain) that must be engineered around with new constraints
% (e.g., laws, which are Ropes or Snares).
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a detached analytical view, the dilemma is a pure mathematical and
% logical limit, the very definition of a Mountain in this system.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prisoners_dilemma_equilibrium_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(mountain_threshold_adherence) :-
    % Verify the base metrics are within the defined limits for a Mountain.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, ExtMetricName, E),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(prisoners_dilemma_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This file models the Prisoner's Dilemma equilibrium as a pure Mountain. The
 *   constraint is the logical structure, which is non-extractive and non-coercive;
 *   it is a mathematical truth. By classifying it as a Mountain with low base
 *   extraction (0.10) and low suppression (0.05), the model becomes internally
 *   consistent. The addition of the Natural Law (NL) Profile metrics
 *   (accessibility_collapse, resistance) and the emerges_naturally flag
 *   certifies this classification, ensuring the engine's mountain gate fires
 *   correctly.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain, the constraint is perceived
 *   identically by all agents as an immutable feature of the logical landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain, the constraint does not have beneficiaries or
 *   victims in the structural sense required for directionality derivation. The
 *   concept of directionality (d) does not apply to natural laws.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mandatrophy issue is resolved by correctly identifying the constraint as
 *   a Mountain. Initial misinterpretations might conflate the *outcome* of the
 *   dilemma (lost cooperative surplus) with extraction *by the constraint itself*.
 *   Since the constraint is a logical limit, not an extractive mechanism, there
 *   is no perspectival gap to resolve. All agents perceive the same immutable
 *   logical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_prisoners_dilemma_equilibrium,
    'At what discount factor does the iterated Prisoner\'s Dilemma escape the one-shot Nash Equilibrium, and is that threshold stable across population structures?',
    'Evolutionary game theory simulations across varying discount factors, memory lengths, and network topologies.',
    'If shadow of the future reliably flips, the constraint becomes a context-dependent Rope. If the threshold is fragile, the Mountain persists as the dominant attractor.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(prisoners_dilemma_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a low-extraction constraint (ε=0.10), temporal measurements for drift
% detection are not required by the linter. The logical structure of the
% dilemma is considered static over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% As a Mountain, this constraint has no coordination function and therefore no
% coordination_type. It represents a fundamental limit *around which*
% coordination mechanisms are built.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. Directionality is not applicable to a Mountain.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */