% ============================================================================
% CONSTRAINT STORY: constrained_optimization_problems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constrained_optimization_problems, []).

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
 *   constraint_id: constrained_optimization_problems
 *   human_readable: The Mathematical Structure of Constrained Optimization Problems
 *   domain: mathematical_foundations/optimization_theory
 *
 * SUMMARY:
 *   Constrained optimization problems represent a fundamental mathematical
 *   structure that emerges universally whenever an agent seeks to maximize or
 *   minimize an objective function subject to restrictions on feasible
 *   choices. This constraint is invariant across domains: it appears in
 *   classical mechanics (Lagrangian mechanics), economics (consumer choice),
 *   engineering (design optimization), control theory, machine learning, and
 *   resource allocation. The structure is not a policy choice or
 *   institutional arrangement — it is a logical necessity arising from the
 *   definition of an optimization problem with restrictions. No agent,
 *   regardless of power or position, can escape the mathematical structure:
 *   the feasible region exists, the objective function has or lacks extrema
 *   within it, and the optimality conditions (Karush-Kuhn-Tucker, Lagrange
 *   multiplier conditions) are invariant. This constraint exhibits all
 *   hallmarks of a mountain: zero degrees of freedom across all indices,
 *   accessibility collapse (0.92) reflecting that no alternative problem
 *   formulation dissolves the structure, resistance (0.08) reflecting minimal
 *   contestation of the mathematical fact, and extractiveness (0.18)
 *   reflecting that the constraint is a neutral feature of problem structure,
 *   not an extractive mechanism.
 *
 * KEY AGENTS:
 *   - Constrained Agent (powerless/trapped): Any actor seeking to optimize within bounded choices — mathematically forced to respect the constraint structure
 *   - Powerful Optimizer (powerful/mobile): Even actors with resources and mobility face the same mathematical constraints — power does not exempt from logical limits
 *   - Analytical Observer (analytical/analytical): Sees the constraint as irreducible structure, invariant across formulations and implementations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constrained_optimization_problems, 0.18).
domain_priors:suppression_score(constrained_optimization_problems, 0.03).
domain_priors:theater_ratio(constrained_optimization_problems, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constrained_optimization_problems, extractiveness, 0.18).
narrative_ontology:constraint_metric(constrained_optimization_problems, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(constrained_optimization_problems, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constrained_optimization_problems, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(constrained_optimization_problems, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constrained_optimization_problems, mountain).
narrative_ontology:human_readable(constrained_optimization_problems, "The Mathematical Structure of Constrained Optimization Problems").
narrative_ontology:topic_domain(constrained_optimization_problems, "mathematical_foundations/optimization_theory").

domain_priors:emerges_naturally(constrained_optimization_problems).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED AGENT (MOUNTAIN) — Any agent attempting to optimize within a bounded domain faces irreducible mathematical constraints. These are not contingent institutional arrangements but structural features of the problem space itself. The agent cannot escape the constraint through exit, negotiation, or reorganization — the mathematics is invariant across all possible implementations and contexts.
constraint_indexing:constraint_classification(constrained_optimization_problems, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: POWERFUL OPTIMIZER (MOUNTAIN) — Even actors with significant resources and flexibility face the same mathematical structure. Wealth, power, and mobility do not exempt agents from the logical limits of constrained optimization. The constraint applies equally regardless of the agent's structural position — a mark of true natural law.
constraint_indexing:constraint_classification(constrained_optimization_problems, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the logical/mathematical vantage, constrained optimization problems represent irreducible features of the problem space. The feasible region, objective function, and constraint manifold are structural invariants that no observer position can dissolve or work around. The mathematics is the same from all perspectives.
constraint_indexing:constraint_classification(constrained_optimization_problems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constrained_optimization_problems_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(constrained_optimization_problems, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constrained_optimization_problems, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constrained_optimization_problems, ExtMetricName, E),
    domain_priors:suppression_score(constrained_optimization_problems, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constrained_optimization_problems),
    narrative_ontology:constraint_metric(constrained_optimization_problems, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constrained_optimization_problems, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constrained_optimization_problems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint structure does not extract from agents in the sense of an asymmetric flow of resources. Rather, it is a neutral structural feature describing how optimization problems are constituted. The non-zero value reflects that the constraint does structure the agent's choice set — there is a reduction in feasible space compared to an unconstrained problem. But this is not extraction in the sense of one agent benefiting at another's expense; it is a mutual structural fact applying equally to all agents. Suppression (0.03): Minimal. There are no coercive mechanisms, alternatives suppressed, or barriers to understanding the constraint. The constraint is transparent and mathematically tractable. Theater ratio (0.05): Minimal. There is almost no performative activity obscuring the mathematical structure. The constraint is what it appears to be. Claimed type (Mountain): The constraint qualifies as a mountain because (1) extractiveness ≤ 0.25, (2) suppression ≤ 0.05, (3) accessibility_collapse ≥ 0.85 (no alternative formulation dissolves the feasible region structure), (4) resistance ≤ 0.15 (mathematicians universally accept the structure), and (5) emerges_naturally = true (the constraint arises from the logical definition of a constrained optimization problem, not from human design or enforcement).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits ZERO perspectival gap. All three perspectives (powerless, powerful, analytical) classify the problem identically as a mountain. This is exactly what we expect for a true natural law — the classification is invariant across observer positions, time horizons, exit options, and spatial scopes. The universality of the mountain classification is the diagnostic signature that the constraint is indeed a feature of the problem structure itself, not a contingent institutional arrangement that appears differently from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint because it is not an extraction mechanism. There are no beneficiaries and no victims — the constraint applies equally to all agents. The feasible region, objective function, and optimality conditions are universal features of the problem space. No agent holds an asymmetric structural position relative to this constraint; all agents are equally constrained by the mathematical structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feasibility_vs_optimality_boundary,
    'Is the distinction between feasible and optimal solutions in constrained optimization a mathematical law or a contingent framing choice?',
    'Formal proof of the necessity of the feasible region under any well-defined optimization problem; examination of alternative problem formulations that dissolve this distinction',
    'If the boundary is necessary: mountain classification confirmed. If the boundary depends on problem formulation: constraint may be partially contingent and should be decomposed into separate stories (mathematical structure vs. choice of problem formulation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feasibility_vs_optimality_boundary, conceptual, 'Whether feasible/optimal boundary is mathematical necessity or formulation choice').

omega_variable(
    lagrange_multiplier_interpretation,
    'Do Lagrange multipliers represent natural shadow prices (revealing scarcity inherent in the constraint) or imposed accounting structures (revealing only the optimization formulation)?',
    'Economic interpretation: compare shadow prices across different problem formulations of the same real-world scenario; test whether shadow price magnitudes reflect actual resource scarcity independent of formulation',
    'If multipliers reveal inherent scarcity: supports mountain classification (constraint structure is given). If multipliers are artifacts of formulation: suggests constraint is partially institutional and should be decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lagrange_multiplier_interpretation, conceptual, 'Interpretation of Lagrange multipliers as natural vs. conventional').

omega_variable(
    kkt_conditions_universality,
    'Are the Karush-Kuhn-Tucker conditions universal necessary conditions for optimality in constrained problems, or do they encode specific assumptions about convexity and regularity?',
    'Examine constraint qualification conditions; test whether KKT conditions hold without regularity assumptions; identify alternative optimality characterizations in non-regular or non-convex settings',
    'If KKT is universal: mountain classification holds. If KKT requires assumptions: the constraint may have substructure that should be separated into stories (the mathematical structure vs. the regularity assumptions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kkt_conditions_universality, empirical, 'Universality of Karush-Kuhn-Tucker optimality conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constrained_optimization_problems, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copt_tr_t0, constrained_optimization_problems, theater_ratio, 0, 0.05).
narrative_ontology:measurement(copt_tr_t50, constrained_optimization_problems, theater_ratio, 50, 0.05).
narrative_ontology:measurement(copt_tr_t100, constrained_optimization_problems, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(copt_be_t0, constrained_optimization_problems, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(copt_be_t50, constrained_optimization_problems, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(copt_be_t100, constrained_optimization_problems, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constrained_optimization_problems, information_standard).
narrative_ontology:affects_constraint(constrained_optimization_problems, lagrange_multiplier_interpretation).
narrative_ontology:affects_constraint(constrained_optimization_problems, convexity_assumptions).
narrative_ontology:affects_constraint(constrained_optimization_problems, constraint_qualification).

% DUAL FORMULATION NOTE:
% Constrained optimization problems form a foundational layer for other constraints in optimization theory. Specific instantiations (convex optimization, non-convex problems, integer programming) may decompose this into distinct stories with different extractiveness values depending on additional assumptions or application domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
