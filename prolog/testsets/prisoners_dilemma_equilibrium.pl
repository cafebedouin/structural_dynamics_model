% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-25
% ============================================================================

:- module(constraint_prisoners_dilemma_equilibrium, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: prisoners_dilemma_equilibrium
 * human_readable: The Prisoner's Dilemma (Nash Equilibrium)
 * domain: logical/economic/social
 * * SUMMARY:
 * The Prisoner's Dilemma is a foundational concept in game theory demonstrating
 * why two rational individuals might not cooperate, even if it appears to be in
 * their best interest. The constraint is the Nash Equilibrium itself—the logical
 * gravity that pulls isolated, self-interested agents toward mutual defection.
 * This equilibrium is a structural limit on non-cooperative games, functioning
 * as a Mountain of logic.
 * * KEY AGENTS:
 * - The Prisoner (Subject): An agent trapped by the logic of the payoff matrix.
 * - The Institutional Designer: An agent observing the dilemma's structure.
 * - The Analytical Observer: An auditor classifying the dilemma's abstract form.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: The dilemma's logical structure itself is not extractive; it merely
% describes a scenario where potential value is lost. The extraction occurs in
% real-world instantiations, not the abstract model. This is a pure Mountain.
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.1).
% Rationale: The equilibrium doesn't suppress alternatives through coercion,
% but through pure logic, making cooperation appear irrational.
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.05).
% Rationale: The concept is purely functional, with no performative aspect.
domain_priors:theater_ratio(prisoners_dilemma_equilibrium, 0.0).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.1).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% The Nash Equilibrium is presented as a logical, mathematical inevitability.
narrative_ontology:constraint_claim(prisoners_dilemma_equilibrium, mountain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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

% PERSPECTIVE 2: THE INSTITUTIONAL DESIGNER (MOUNTAIN)
% For a legislator or system designer, the dilemma is a fixed feature of the
% landscape (a Mountain) that must be engineered around with new constraints
% (e.g., laws, which are Ropes or Snares).
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
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
    E =< 0.15,
    S =< 0.05.

:- end_tests(prisoners_dilemma_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This file models the Prisoner's Dilemma equilibrium as a pure Mountain. The
 * original file made a category error by assigning a high extraction score (0.7),
 * conflating the *outcome* of the dilemma (lost cooperative surplus) with
 * extraction *by the constraint itself*. The constraint is the logical structure,
 * which is non-extractive and non-coercive; it is a mathematical truth.
 *
 * By reclassifying it as a Mountain with low base extraction (0.1) and low
 * suppression (0.05), the model becomes internally consistent. Real-world
 * scenarios that instantiate the dilemma (like an arms race) are separate
 * constraints (often Snares) that are built upon this foundational Mountain.
 * Laws and contracts are Ropes designed to allow agents to escape the Mountain's
 * logical gravity.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Mandatrophy issue is resolved by correctly
 * identifying the constraint as a Mountain. The high extraction score that
 * triggered the alert was based on a misinterpretation. Since the constraint
 * is a logical limit, not an extractive mechanism, there is no perspectival
 * gap between a "beneficiary" and a "victim" to resolve. All agents perceive
 * the same immutable logical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% As this is a low-extraction Mountain based on a formal model, there are no
% significant structural ambiguities requiring an omega_variable. The linter
% requirement for high-extraction constraints does not apply.

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(prisoners_dilemma_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a low-extraction constraint, temporal measurements for drift detection
% are not required by the linter. The logical structure of the dilemma is
% considered static over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% As a Mountain, this constraint has no coordination function and therefore no
% coordination_type. It represents a fundamental limit *around which*
% coordination mechanisms are built.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Beneficiary/Victim (required by audit) ---
% The equilibrium benefits analytical clarity — it reveals the structure of
% non-cooperative interaction. The "victim" is the cooperative surplus that
% rational agents cannot access without external coordination mechanisms.
narrative_ontology:constraint_beneficiary(prisoners_dilemma_equilibrium, analytical_clarity).
narrative_ontology:constraint_victim(prisoners_dilemma_equilibrium, cooperative_surplus).

% --- Omega variable (irreducible uncertainty) ---
omega_variable(
    iteration_shadow_threshold,
    "At what discount factor does the iterated Prisoner's Dilemma escape the one-shot Nash Equilibrium, and is that threshold stable across population structures?",
    resolution_mechanism("Evolutionary game theory simulations across varying discount factors, memory lengths, and network topologies"),
    impact("If shadow of the future reliably flips: Mountain becomes context-dependent Rope. If threshold is fragile: Mountain persists as the dominant attractor."),
    confidence_without_resolution(high)
).