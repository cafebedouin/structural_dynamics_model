% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The existence of local optima in non-convex optimization spaces is a
 *   fundamental mathematical constraint inherent to the topology of
 *   non-convex sets and the structure of optimization itself. It is not a
 *   feature of any particular algorithm, implementation, or application
 *   domain — it is a property of the mathematical landscape itself. In any
 *   space where the neighborhood structure deviates from convexity (which is
 *   to say, virtually all realistic optimization problems), there must exist
 *   points that are superior to all immediate neighbors but are not the
 *   global best. This constraint appears identically across all structural
 *   perspectives because it emerges necessarily from the definition of
 *   non-convexity. The constraint has zero degrees of freedom for all
 *   indices: no algorithm, no computational power, no reformulation can
 *   eliminate the existence of local optima without fundamentally changing
 *   the problem space (e.g., by imposing convexity constraints, which changes
 *   the problem to something different). The theater_ratio is extremely low
 *   (0.15) because the constraint has no performative component — it is pure
 *   mathematical fact with no institutional, social, or observational
 *   ambiguity. The suppression is minimal (0.03) because there is nothing to
 *   suppress — the constraint does not coerce anyone; it simply describes the
 *   topology of optimization landscapes. The extractiveness value (0.12)
 *   reflects that this is a constraint on the problem space itself, not an
 *   extraction mechanism from any agent. All perspectives classify
 *   identically as Mountain because the constraint's structure is invariant
 *   across all observables and all measurement methodologies.
 *
 * KEY AGENTS:
 *   - Algorithm Designers: Computational agents (powerful/mobile) — encounter the constraint as an immovable limit on what algorithms can guarantee; can design better heuristics but cannot escape the fundamental limit
 *   - Machine Learning Practitioners: Applied agents (moderate/mobile) — experience local optima empirically in training; adapt through restarts, regularization, and architecture choices but cannot eliminate the constraint
 *   - Machine Learning Industry: Institutional actor (institutional/arbitrage) — has arbitraged the constraint by accepting 'good enough' solutions instead of global optima; profits from workarounds while the constraint remains immutable
 *   - Mathematical Community: Analytical observer (analytical/analytical) — understands local optima as a topological inevitability; the constraint is central to optimization theory and complexity theory
 *   - Optimization Theory: Foundational knowledge domain (analytical/analytical) — local optima are a core research topic precisely because they are unavoidable; the body of optimization theory is largely about understanding, characterizing, and mitigating the effects of local optima
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
narrative_ontology:topic_domain(local_vs_global_optima, "mathematical/computational").

domain_priors:emerges_naturally(local_vs_global_optima).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOPOLOGICAL ANALYST (MOUNTAIN) — From the vantage of mathematical topology and dynamical systems theory, local optima are an inevitable structural feature of non-convex landscapes. No algorithm, no matter how sophisticated, can escape this constraint without changing the problem space itself (imposing convexity assumptions). The existence of local optima follows necessarily from the definition of non-convexity. Zero degrees of freedom.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNER (MOUNTAIN) — Even the most advanced optimization algorithms (SGD variants, evolutionary strategies, simulated annealing, neural networks) cannot guarantee global optimality in non-convex spaces with finite computational resources. This is not a limitation of current techniques but a fundamental mathematical constraint. Better algorithms can *reduce* the probability of getting trapped but cannot eliminate local optima. The landscape itself enforces the constraint.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: MACHINE LEARNING INDUSTRY (MOUNTAIN) — From the practical standpoint of training neural networks at scale, local optima are observed as a fundamental phenomenon, yet the industry operates successfully by treating them as manageable noise rather than blockers. The constraint manifests as the empirical observation that networks train to reasonable solutions despite the theoretical guarantee of local optima. The industry has 'arbitraged' the constraint by accepting that good solutions (not necessarily global optima) are sufficient for most applications. Yet the mathematical constraint remains immutable — local optima exist whether the industry profits from workarounds or not.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICAL FOUNDATIONALIST (MOUNTAIN) — Local optima are a logical consequence of the axioms of topology and set theory. In any space where the neighborhood structure differs from convexity (e.g., any realistic loss landscape with millions of parameters), the existence of points that are locally maximal but not globally maximal is a tautology. No escape, no workaround, no contemporary research will change this. It is as immutable as the Halting Problem.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: PRACTITIONER (MOUNTAIN) — A researcher training a deep neural network encounters local optima as an immovable fact of practice. They can restart from different initializations, use learning rate schedules, apply regularization, or switch algorithms — but they cannot escape the existence of local optima in the underlying loss surface. Every workaround is an adaptation to the constraint, not a negation of it. The practitioner's agency is real but operates within the constraint's boundaries.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UNIVERSAL OBSERVER (MOUNTAIN) — Across all conceivable non-convex optimization problems — in physics (protein folding), economics (equilibrium computation), biology (neural development), engineering (control systems design) — local optima are universally present. No measurement basis, no change in perspective, no new theory can alter this fact. The constraint is invariant across all observables and application domains.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.12): Very low. The constraint does not extract value from anyone; it is a property of the mathematical landscape. The value is not zero because some agents (practitioners, algorithm designers) must expend resources (computational time, research effort) to work around or adapt to the constraint. But this is not extraction in the DR sense — it is not asymmetric transfer of value. It is the cost of engaging with a constrained problem space. Suppression (0.03): Minimal. There is nothing suppressed. No agent can be 'freed' from this constraint through different framing, exit, or negotiation. The constraint is identical for all participants. Theater_ratio (0.15): Very low. The constraint is purely functional — there is no performative institutional component. The mathematical literature does not contain 'theater' around local optima; it contains rigorous proofs of their existence and analysis of their properties. The small non-zero value reflects only minor presentation/pedagogical choices in how the constraint is communicated, but these do not change the underlying reality.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All six perspectives classify as Mountain identically because the constraint's existence is independent of the observer's position, power, time horizon, exit options, or scope. The constraint does not vary based on who measures it or how they measure it. This is the defining feature of a true Mountain constraint in the DR framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is not applicable for Mountain constraints because there is no extraction mechanism. All agents are equally 'subject to' the constraint in the sense that it describes the topology of their problem space, not because it extracts from them. There is no beneficiary and no victim — the constraint simply exists as a boundary condition on all non-convex optimization problems.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not arise for this constraint because it is uniformly classified as Mountain from all perspectives. There is no risk of mislabeling coordination as extraction (or vice versa) because the constraint has no coordination function and no extraction mechanism. It is pure mathematical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    global_vs_local_definition_precision,
    'Does the mathematical definition of ''global optimum'' and ''local optimum'' require discrete/continuous topology assumptions that might vary across problem classes?',
    'Formal review of topology textbooks and Morse theory; analysis of whether definitions hold equally in discrete optimization (NP-complete problems) and continuous (differentiable) optimization',
    'If definitions are universal: constraint is mountain across all problem classes. If definitions require assumption sets: local optima might not exist in some formally distinct problem spaces (e.g., fully discrete, or non-Hausdorff topologies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_vs_local_definition_precision, conceptual, 'Precision of global vs local optima definitions across topology classes').

omega_variable(
    convexity_escape_mechanisms,
    'Can a non-convex optimization problem be transformed into an equivalent convex problem through change of variables or problem reformulation?',
    'Survey of convex relaxation techniques in operations research; analysis of when transformations preserve optimality vs when they create new constraints',
    'If transformations preserve optimality universally: the constraint becomes variable-dependent (might be a Rope or Tangled Rope for problem designers). If transformations always lose information: local optima are truly immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convexity_escape_mechanisms, empirical, 'Whether non-convex problems can be transformed to convex equivalents').

omega_variable(
    landscape_concentration_limits,
    'In very high-dimensional spaces (e.g., neural networks with billions of parameters), do concentration phenomena (high-dimensional Gaussian measure concentration, loss landscape smoothness at scale) reduce the practical relevance of local optima even if they theoretically exist?',
    'Analysis of recent work on neural network loss landscape geometry (e.g., mode connectivity, loss landscape visualization); empirical measurement of how often gradient descent gets stuck vs reaches acceptable solutions',
    'If concentration makes local optima rare or harmless in high dimensions: the constraint becomes less binding in practice (Scaffold or Piton). If local optima remain pervasive and harmful: the constraint stays Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(landscape_concentration_limits, empirical, 'Relevance of local optima in high-dimensional neural network optimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(local_vs_global_optima, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgo_tr_t0, local_vs_global_optima, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lgo_tr_t25, local_vs_global_optima, theater_ratio, 25, 0.14).
narrative_ontology:measurement(lgo_tr_t50, local_vs_global_optima, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(lgo_be_t0, local_vs_global_optima, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lgo_be_t25, local_vs_global_optima, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(lgo_be_t50, local_vs_global_optima, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(local_vs_global_optima, information_standard).
narrative_ontology:affects_constraint(local_vs_global_optima, no_free_lunch_theorem).
narrative_ontology:affects_constraint(local_vs_global_optima, np_hardness_computational).
narrative_ontology:affects_constraint(local_vs_global_optima, convergence_rate_bounds).

% DUAL FORMULATION NOTE:
% Local optima are a structural feature of non-convex spaces; they are upstream of many computational complexity results (NP-hardness, no-free-lunch theorem, convergence rate bounds). Decomposition into separate constraint stories would be inappropriate — local optima is a single, unified mathematical phenomenon with invariant ε across all measurement bases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
