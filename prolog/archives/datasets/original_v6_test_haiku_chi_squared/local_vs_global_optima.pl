% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_local_vs_global_optima, []).

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
 *   constraint_id: local_vs_global_optima
 *   human_readable: The Existence of Local Optima in Non-Convex Spaces
 *   domain: mathematics/computational_optimization
 *
 * SUMMARY:
 *   The existence of local optima in non-convex optimization landscapes is a
 *   mathematical constraint of the first order: it follows from the
 *   definition of local optimality and the topology of non-convex spaces.
 *   Unlike institutional constraints that can be reformed, negotiated, or
 *   engineered away, local optima are features of the mathematical object
 *   itself. The constraint classifies as Mountain from all perspectives
 *   because no agent—whether powerless, powerful, or organized—can change the
 *   fundamental structure of non-convex landscapes. This is the canonical
 *   example of an invariant natural law in the computational domain. The
 *   minimal theater ratio (0.15) reflects that the constraint makes no claims
 *   about superiority or legitimacy; it is simply true. The extractiveness
 *   (0.12) is low because no agent extracts value from the constraint
 *   itself—it is a structural limit on all agents equally.
 *
 * KEY AGENTS:
 *   - The Pure Mathematician: Observer of formal structure — recognizes the constraint as a necessary consequence of non-convex topology
 *   - The Algorithm Designer: Powerful agent with global reach — cannot circumvent the constraint through computational power or sophistication
 *   - The Gradient-Based Optimizer: Powerless agent with no alternatives — trapped by the constraint; no exit options exist
 *   - The Research Community: Organized collective action — cannot eliminate local optima through coordination or pooled knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(local_vs_global_optima, 0.12).
domain_priors:suppression_score(local_vs_global_optima, 0.03).
domain_priors:theater_ratio(local_vs_global_optima, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(local_vs_global_optima, extractiveness, 0.12).
narrative_ontology:constraint_metric(local_vs_global_optima, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(local_vs_global_optima, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(local_vs_global_optima, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(local_vs_global_optima, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(local_vs_global_optima, mountain).
narrative_ontology:human_readable(local_vs_global_optima, "The Existence of Local Optima in Non-Convex Spaces").
narrative_ontology:topic_domain(local_vs_global_optima, "mathematics/computational_optimization").

domain_priors:emerges_naturally(local_vs_global_optima).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PURE MATHEMATICIAN (MOUNTAIN) — From the standpoint of formal topology and calculus, the existence of local optima in non-convex spaces is a direct consequence of the definition of local optimality and the connectivity structure of non-convex domains. This is not a constraint imposed by any agent; it is a structural feature of the mathematical objects themselves. No amount of computational power, institutional reorganization, or clever algorithm design can eliminate local optima—only navigate them differently. d=0.72, f(d)≈1.15, σ(universal)=1.0 → χ≈0.14. The mountain classification is exact.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Even with unlimited compute, parallelism, and sophisticated search heuristics (simulated annealing, genetic algorithms, gradient descent variants), an algorithm cannot guarantee escape from local optima in the general non-convex case. The constraint is not imposed by lack of computational resources—it is imposed by the topology itself. A powerful agent with global reach sees this as a physical law of information landscapes, not as an extractive constraint. d=0.50, f(d)≈0.65, σ(global)=1.2 → χ≈0.10. Still mountain.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: GRADIENT-BASED OPTIMIZER (MOUNTAIN) — Even an optimizer with no alternatives—no stochasticity, no restarts, no global knowledge—cannot escape the fundamental constraint. Local optima exist whether the optimizer is aware of them or not. The constraint is not enforced against the optimizer; it exists independently. The optimizer's powerlessness does not make the constraint more severe—it merely makes non-convexity more salient. d=0.95, f(d)≈1.42, σ(local)=0.8 → χ≈0.17. Still mountain. The constraint's severity does not vary with observer power.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (MOUNTAIN) — Even organized research communities (through collaborative discovery, pooled computational resources, and accumulated knowledge) cannot eliminate local optima from the landscape itself. They can learn to navigate around them more skillfully, but the constraint persists across all institutional arrangements. No coordination mechanism bypasses the topology. d=0.40, f(d)≈0.40, σ(global)=1.2 → χ≈0.06. The mountain persists regardless of collective action.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(local_vs_global_optima_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(local_vs_global_optima, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(local_vs_global_optima, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(local_vs_global_optima, ExtMetricName, E),
    domain_priors:suppression_score(local_vs_global_optima, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(local_vs_global_optima),
    narrative_ontology:constraint_metric(local_vs_global_optima, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(local_vs_global_optima, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(local_vs_global_optima_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint does not extract value from any agent in an asymmetric way. All optimization processes—regardless of their nature or power—are subject to the possibility of local optima. There is no beneficiary and no victim because the constraint is not an institutional arrangement; it is a property of the solution space itself. The low value reflects that this is not an extractive relationship but a natural limit. Suppression (0.03): Minimal. There is no coercion here. Agents face the constraint as an inescapable mathematical fact, not as enforced suppression. Theater ratio (0.15): Minimal. The constraint makes no performative claims. Its statement is simple and formal: in non-convex spaces, local optima exist. No ritual, tradition, or institutional theater surrounds this mathematical fact—it is transparent and invariant.
 *
 * PERSPECTIVAL GAP:
 *   Remarkably, this constraint exhibits NO perspectival gap. All observers—from powerless to organized, from immediate to civilizational time horizons—classify the constraint identically as Mountain. This invariance is the signature of a true natural law. The pure mathematician sees it as topological necessity. The algorithm designer sees it as an irreducible information-theoretic limit. The powerless optimizer sees it as an immutable feature of the landscape. The organized research community sees it as a shared structural condition, not something any collaboration can overcome. The absence of perspectival disagreement is itself the proof that this is a mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality override is needed or appropriate. The constraint exhibits no asymmetric extraction because there is no extraction relationship. All agents are equally subject to the constraint. The 'observer power' parameter in the perspectives shows that even powerful agents cannot escape local optima—this is why agent_power varies across perspectives (to demonstrate the invariance despite power differences). The exit_options parameter similarly demonstrates that even mobile agents with arbitrary exit options cannot escape the constraint—it applies regardless of the agent's structural position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convexity_boundary_definition,
    'Does the distinction between ''convex'' and ''non-convex'' reflect a sharp natural boundary or a continuous spectrum of landscape properties?',
    'Formal analysis of intermediate landscape structures (weakly convex, quasi-convex, pseudoconvex regions); empirical measurement of local optima density and basin depth as functions of landscape geometry',
    'If sharp boundary: the mountain classification is exact. If spectrum: local optima are continuous with landscape complexity rather than a discrete natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convexity_boundary_definition, conceptual, 'Whether convexity is a discrete property or continuous spectrum').

omega_variable(
    escapeability_in_practice,
    'In practice, do randomly initialized gradient descent runs converge to local optima that differ significantly in quality, or do they tend to find similar-quality solutions?',
    'Large-scale empirical study: thousands of random initializations on benchmark non-convex problems; measurement of variance in final solution quality and basin volume distribution',
    'If variance is low: local optima are empirically indistinguishable (constraint is less severe). If variance is high: quality-relevant local optima proliferate (constraint is more severe in practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(escapeability_in_practice, empirical, 'Empirical variance in quality of distinct local optima').

omega_variable(
    algorithmic_transcendence_possibility,
    'Could an algorithmic oracle with access to higher-dimensional gradient information or non-local landscape structure escape local optima in polynomial time?',
    'Formal complexity analysis of oracle-aided optimization; proof-theoretic investigation of whether polynomial-time algorithms exist for non-convex global optimization given unlimited first-order oracle queries',
    'If oracle still fails: local optima are a constraint on information-theoretic grounds. If oracle succeeds: constraint is only about computational resources, not topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transcendence_possibility, conceptual, 'Whether non-local information access enables global optimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(local_vs_global_optima, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgo_tr_t0, local_vs_global_optima, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lgo_tr_t50, local_vs_global_optima, theater_ratio, 50, 0.14).
narrative_ontology:measurement(lgo_tr_t100, local_vs_global_optima, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(lgo_be_t0, local_vs_global_optima, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lgo_be_t50, local_vs_global_optima, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(lgo_be_t100, local_vs_global_optima, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(local_vs_global_optima, information_standard).
narrative_ontology:affects_constraint(local_vs_global_optima, convex_optimization_solvability).
narrative_ontology:affects_constraint(local_vs_global_optima, np_completeness_of_nonconvex_problems).

% DUAL FORMULATION NOTE:
% Local optima are upstream of computational complexity constraints. The existence of local optima (this constraint) is a structural property of non-convex spaces that directly implies the NP-hardness of global optimization in the non-convex case. Convex optimization represents the downstream domain where this constraint does not apply—it is a separate constraint story with different ε and properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
