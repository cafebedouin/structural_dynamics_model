% ============================================================================
% CONSTRAINT STORY: p_vs_np
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_p_vs_np, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: p_vs_np
 *   human_readable: The P versus NP Problem
 *   domain: technological
 *
 * SUMMARY:
 *   The P vs NP problem is a major unsolved question in computer science, asking
 *   whether every problem whose solution can be quickly verified (NP) can also
 *   be quickly solved (P). The widespread belief that P != NP functions as a
 *   fundamental limit on computation, akin to a law of physics. This constraint
 *   represents that perceived barrier between "easy" (P) and "hard" (NP-complete)
 *   problems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Optimization Engineers: Primary target (powerless/constrained) — face the "exponential wall" of NP-hard problems, forcing them to use heuristics instead of optimal solutions.
 *   - Cryptographers: Primary beneficiary (institutional/arbitrage) — rely on the hardness of certain NP problems (e.g., integer factorization) to build secure cryptographic systems.
 *   - Complexity Theorists: Analytical observer (analytical/trapped) — study the problem as a fundamental, objective feature of computation, trapped within the axioms of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% If P != NP, this is a fundamental feature of mathematics, not a system of extraction.
% The "cost" is the irreducible energy/time for computation, not a rent.
domain_priors:base_extractiveness(p_vs_np, 0.05).
% There are no known, suppressed alternatives to the laws of computation.
domain_priors:suppression_score(p_vs_np, 0.01).
% The constraint is purely functional; there is no performative aspect.
domain_priors:theater_ratio(p_vs_np, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(p_vs_np, extractiveness, 0.05).
narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(p_vs_np, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain certification) ---
% Accessibility collapse: widely believed to be an absolute barrier, though
% unproven. No known constructive path to P=NP exists.
narrative_ontology:constraint_metric(p_vs_np, accessibility_collapse, 0.95).
% Resistance: some researchers pursue P=NP proofs, but the barrier itself
% faces no meaningful structural opposition — it's a mathematical fact.
narrative_ontology:constraint_metric(p_vs_np, resistance, 0.02).

% --- Emergence flag (required for mountain metric gate) ---
domain_priors:emerges_naturally(p_vs_np).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(p_vs_np, mountain).
narrative_ontology:human_readable(p_vs_np, "The P versus NP Problem").

% --- Binary flags ---
% No enforcement needed; it's a property of logic.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These are included to explain the perspectival gaps, even though the base
% constraint is a Mountain. The social/economic systems built *on top* of
% this mountain have beneficiaries and victims.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(p_vs_np, cryptographers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(p_vs_np, optimization_engineers).

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

% PERSPECTIVE 1: THE OPTIMIZATION ENGINEER (SNARE)
% Agent who bears the most cost. The practical inability to find optimal
% solutions for NP-hard problems within a deadline feels like a trap.
% Engine derives d from victim status + constrained exit -> high d -> high χ.
constraint_indexing:constraint_classification(p_vs_np, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CRYPTOGRAPHER (ROPE)
% Agent who benefits most. The hardness of NP problems is the coordination
% mechanism that enables public-key cryptography and digital trust.
% Engine derives d from beneficiary status + arbitrage exit -> low d -> negative χ.
constraint_indexing:constraint_classification(p_vs_np, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE COMPLEXITY THEORIST (MOUNTAIN)
% The analytical observer sees a fundamental, unchangeable feature of logic.
% This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(p_vs_np, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(p_vs_np_tests).

test(perspectival_gap) :-
    % Verify the gap between the theorist, cryptographer, and engineer.
    constraint_indexing:constraint_classification(p_vs_np, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(p_vs_np, TypeTarget, context(agent_power(powerless), _, _, _)),
    TypeAnalyst == mountain,
    TypeBeneficiary == rope,
    TypeTarget == snare,
    TypeAnalyst \= TypeBeneficiary,
    TypeBeneficiary \= TypeTarget.

test(threshold_validation) :-
    % Verify that the base metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(p_vs_np, extractiveness, E),
    narrative_ontology:constraint_metric(p_vs_np, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(p_vs_np_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics (ε=0.05, suppression=0.01) were set to reflect the
 *   analytical view that P vs NP is a fundamental property of mathematics,
 *   not a system of social extraction. This resolves the MOUNTAIN_METRIC_CONFLICT
 *   lint error. The "cost" it imposes is the irreducible physical cost of
 *   computation, which is characteristic of a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and illustrates the core of the framework.
 *   - A Complexity Theorist sees a Mountain: an objective, unchangeable feature
 *     of the logical universe.
 *   - A Cryptographer sees a Rope: they leverage the Mountain's slope as a
 *     coordination tool to create digital trust. The difficulty is the point.
 *   - An Optimization Engineer sees a Snare: they are trapped by the problem's
 *     intractability, which strangles their ability to deliver optimal solutions
 *     under real-world deadlines and budget constraints.
 *   The same underlying reality produces three different classifications based on
 *   the agent's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'cryptographers' benefit because the difficulty of NP problems
 *     is the foundation of modern public-key cryptography.
 *   - Victim: 'optimization_engineers' bear the cost, as they are constantly
 *     forced to find imperfect, heuristic solutions to NP-hard problems like
 *     logistics, scheduling, and protein folding.
 *   This data allows the engine to derive the correct directionality (d) for
 *   the Rope and Snare perspectives, even though the base constraint is a Mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying the base constraint as a Mountain, we avoid mislabeling a
 *   law of nature as a system of extraction. The Rope and Snare classifications
 *   emerge from how social and economic systems are built *around* the Mountain,
 *   not from the Mountain itself. This correctly separates the fundamental
 *   constraint from the secondary constraints it enables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_p_vs_np,
    'If P=NP were proven constructively, would the polynomial algorithm be practically efficient (e.g., low constants/degree) or just theoretically so?',
    'A formal, constructive proof of P=NP.',
    'If efficient, modern cryptography collapses (Rope becomes Snare for everyone). If inefficient, the "Mountain" effectively remains for practical purposes.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(p_vs_np, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.05) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. As a mathematical constant, its
% properties are not expected to drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships declared at this time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The standard derivation chain using the declared
% beneficiary/victim groups and exit options correctly models the perspectival
% gaps for cryptographers and engineers.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */