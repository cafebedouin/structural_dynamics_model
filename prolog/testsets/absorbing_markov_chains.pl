% ============================================================================
% CONSTRAINT STORY: absorbing_markov_chains
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
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
    narrative_ontology:affects_constraint/2,
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
 *   domain: mathematics/systems_theory
 *
 * SUMMARY:
 *   An absorbing Markov chain is a mathematical model for a system of states
 *   where at least one state is 'absorbing'—once entered, it can never be
 *   left. The transition probability from an absorbing state to itself is 1.
 *   This creates a 'state space trap.' This constraint is not a policy or an
 *   institution but a fundamental property of the mathematical structure
 *   itself. Any system modeled as such, from a software process entering a
 *   terminal error state to a physical system reaching equilibrium, is
 *   subject to this logical inevitability.
 *
 * KEY AGENTS:
 *   - System Architect (analytical/analytical): Designs the system and understands its mathematical properties.
 *   - Computational Process (powerless/trapped): The entity that traverses the state space and can become trapped.
 *   - System Operator (moderate/constrained): Monitors the system and must deal with the consequences of a process becoming trapped.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absorbing_markov_chains, 0.02).
domain_priors:suppression_score(absorbing_markov_chains, 0.01).
domain_priors:theater_ratio(absorbing_markov_chains, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, extractiveness, 0.02).
narrative_ontology:constraint_metric(absorbing_markov_chains, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(absorbing_markov_chains, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(absorbing_markov_chains, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(absorbing_markov_chains, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absorbing_markov_chains, mountain).
narrative_ontology:human_readable(absorbing_markov_chains, "State Space Trap in Absorbing Markov Chain").
narrative_ontology:topic_domain(absorbing_markov_chains, "mathematics/systems_theory").

domain_priors:emerges_naturally(absorbing_markov_chains).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SYSTEM ARCHITECT (MOUNTAIN) — The architect understands that the absorbing state is a fundamental, logical consequence of the transition probabilities defining the system. It is a mathematical law, not a policy choice. There is no exit from the logic itself, only the option to design a different system.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE TRAPPED PROCESS (MOUNTAIN) — For a computational process or algorithm that enters an absorbing state (e.g., an infinite loop, a terminal failure mode), the state is an immutable reality. It cannot exit. This is not a Snare because the trap is not imposed by another agent for extraction; it is a law of the system's own 'physics'.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SYSTEM OPERATOR (MOUNTAIN) — An operator observing a system stuck in an absorbing state perceives it as an unchangeable fact. Their only recourse is action external to the Markov chain's logic (e.g., a hard reset), which does not alter the nature of the constraint but rather terminates and restarts the process. The trap itself remains a fixed property of the system design.
constraint_indexing:constraint_classification(absorbing_markov_chains, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

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
 *   This constraint is a canonical Mountain. Extractiveness (ε=0.02) and Suppression (0.01) are near zero because a mathematical law does not extract value or suppress alternatives; it defines the space of possibilities. It 'emerges_naturally' from the axioms of probability theory. Resistance (0.05) is minimal; one cannot 'resist' a mathematical truth, only design systems that avoid its consequences. Accessibility Collapse (0.95) is high because once the definition of an absorbing state is understood, its implications are clear and unambiguous. There is no room for interpretation.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is the hallmark of a Mountain constraint. All observers, regardless of their power, time horizon, or exit options, correctly identify the constraint as an unchangeable, fundamental limit. The trapped process, the system designer, and the external operator all converge on the same classification. This invariance across perspectives confirms its status as a natural (in this case, logical) law.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is directionless. As a mathematical principle, it has no beneficiaries or victims. The *consequences* of a specific system design hitting an absorbing state can create victims (e.g., a user whose software crashes), but the principle itself is neutral and applies symmetrically to all systems that fit the model. The lack of beneficiary/victim declarations reflects this neutrality.
 *
 * MANDATROPHY ANALYSIS:
 *   This case correctly identifies a fundamental mathematical limit as a Mountain. A common error would be to misclassify the *outcome* of the trap (e.g., a failed business process) as a Snare. The Deferential Realism framework correctly distinguishes between the underlying, immutable principle (the Mountain) and the contingent, designed system that runs into it. The mandatrophy is resolved by focusing on the properties of the constraint itself, not its effects in a specific application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absorbing_markov_chains, 1906, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(absorbing_markov_chains, system_reliability_model).
narrative_ontology:affects_constraint(absorbing_markov_chains, algorithmic_convergence_guarantees).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
