% ============================================================================
% CONSTRAINT STORY: prisoners_dilemma_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
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
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
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
 *   constraint_id: prisoners_dilemma_equilibrium
 *   human_readable: The Prisoner's Dilemma (Nash Equilibrium)
 *   domain: logical/economic
 *
 * SUMMARY:
 *   The Prisoner's Dilemma is a foundational concept in game theory
 *   demonstrating why two rational individuals might not cooperate, even if
 *   it appears to be in their best interest. The dilemma arises from the Nash
 *   equilibrium, where each player's best strategy is to defect, regardless
 *   of the other player's action, leading to a suboptimal outcome for both.
 *
 * KEY AGENTS:
 *   - Incarcerated Prisoner: powerless/trapped - constrained by the dilemma's logic
 *   - Game Theorist: powerful/analytical - understands the mathematical implications
 *   - Society: Benefits from the model but suffers from its application in real-world scenarios
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prisoners_dilemma_equilibrium, 0.15).
domain_priors:suppression_score(prisoners_dilemma_equilibrium, 0.01).
domain_priors:theater_ratio(prisoners_dilemma_equilibrium, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, extractiveness, 0.15).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prisoners_dilemma_equilibrium, mountain).
narrative_ontology:human_readable(prisoners_dilemma_equilibrium, "The Prisoner's Dilemma (Nash Equilibrium)").
narrative_ontology:topic_domain(prisoners_dilemma_equilibrium, "logical/economic").

domain_priors:emerges_naturally(prisoners_dilemma_equilibrium).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The prisoner, facing interrogation, is trapped in the immediate decision to cooperate or defect. The logic of the dilemma constrains their action.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Analyzes the game and sees the dominant strategy. Understands the implications of non-cooperation even if it's suboptimal from a global perspective.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% From a universal perspective, the prisoner's dilemma equilibrium represents a logical consequence of rational self-interest under specific conditions. It is a mathematical truth that applies across all instances of the game.
constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prisoners_dilemma_equilibrium_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prisoners_dilemma_equilibrium, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, ExtMetricName, E),
    domain_priors:suppression_score(prisoners_dilemma_equilibrium, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prisoners_dilemma_equilibrium),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prisoners_dilemma_equilibrium, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prisoners_dilemma_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Nash equilibrium in the Prisoner's Dilemma is a robust prediction under the assumptions of rationality and self-interest. The extractiveness is low because the model does not actively extract value, it just reveals a potentially undesirable outcome. Suppression is low because players are free to choose, though influenced by the incentives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives agree that the Nash equilibrium emerges from the structure of the game. Each player is constrained to select their dominant strategy regardless of the other players choice. There is no significant variation amongst the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each player (prisoner) is facing the direct effects of the dilemma. The analytical view sees a logical conclusion from self-interest that creates tension between individual vs collective action.
 *
 * MANDATROPHY ANALYSIS:
 *   Since the classification is consistently a mountain across different agent perspectives, there is not a mandatrophy problem to resolve. The dilemma accurately reflects the incentives for individual actors that lead to an adverse group outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prisoners_dilemma_equilibrium, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prisoners_dilemma_equilibrium, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
