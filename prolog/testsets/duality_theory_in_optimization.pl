% ============================================================================
% CONSTRAINT STORY: duality_theory_in_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_duality_theory_in_optimization, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: duality_theory_in_optimization
 *   human_readable: Duality Theory in Optimization
 *   domain: mathematics/optimization_theory
 *
 * SUMMARY:
 *   Duality theory in optimization is a mathematical structure that emerges
 *   from the geometry of convex sets and the properties of conjugate
 *   functions. For any convex optimization problem, there exists a dual
 *   problem such that the optimal value of the dual bounds the optimal value
 *   of the primal from below (weak duality). Under constraint qualifications
 *   (e.g., Slater's condition), primal and dual optimal values are equal
 *   (strong duality). This structural relationship has remained unchanged for
 *   over 60 years of algorithmic and theoretical development. Duality is not
 *   a policy, regulation, social norm, or institutional arrangement — it is a
 *   logical consequence of convex analysis. The constraint is invariant
 *   across all measurement methodologies, observables, and computational
 *   contexts. It represents a true natural law of optimization.
 *
 * KEY AGENTS:
 *   - Computational Solvers: Any algorithm operating on convex problems encounters duality as a logical boundary — extractiveness flows one direction only (toward the mathematical structure, not toward any agent)
 *   - Optimization Researchers: Benefit from duality through improved algorithms and tighter bounds; cannot modify the duality structure itself
 *   - Mathematical Theory: Defines the constraint; unchangeable by any social or institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(duality_theory_in_optimization, 0.12).
domain_priors:suppression_score(duality_theory_in_optimization, 0.03).
domain_priors:theater_ratio(duality_theory_in_optimization, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(duality_theory_in_optimization, extractiveness, 0.12).
narrative_ontology:constraint_metric(duality_theory_in_optimization, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(duality_theory_in_optimization, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(duality_theory_in_optimization, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(duality_theory_in_optimization, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(duality_theory_in_optimization, mountain).
narrative_ontology:human_readable(duality_theory_in_optimization, "Duality Theory in Optimization").
narrative_ontology:topic_domain(duality_theory_in_optimization, "mathematics/optimization_theory").

domain_priors:emerges_naturally(duality_theory_in_optimization).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTATIONAL SOLVER (MOUNTAIN) — Any optimization algorithm seeking solutions to convex problems encounters duality theory as an immutable logical structure. The weak duality bound (primal value ≥ dual value) is a theorem, not a policy choice. No exit from this constraint exists — it is inherent to the problem formulation itself.
constraint_indexing:constraint_classification(duality_theory_in_optimization, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL OBSERVER (MOUNTAIN) — Duality theory is a structural property of convex optimization that arises from the Legendre-Fenchel transform and the separation theorem. These are theorems, not social arrangements. Strong duality holds under constraint qualifications (Slater's condition, etc.), producing a logical equivalence between primal and dual problems. This logical equivalence is unchangeable — it follows from the axioms of convex analysis.
constraint_indexing:constraint_classification(duality_theory_in_optimization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: OPTIMIZATION COMMUNITY (MOUNTAIN) — Practitioners and researchers in optimization discover duality theory as a structural feature that enables algorithmic advances (dual decomposition, cutting-plane methods, etc.). The duality gap provides a computable bound on solution quality regardless of implementation choices. The constraint persists across 60+ years of algorithmic development because it is rooted in mathematics, not institutional design.
constraint_indexing:constraint_classification(duality_theory_in_optimization, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(duality_theory_in_optimization_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(duality_theory_in_optimization, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(duality_theory_in_optimization, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(duality_theory_in_optimization, ExtMetricName, E),
    domain_priors:suppression_score(duality_theory_in_optimization, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(duality_theory_in_optimization),
    narrative_ontology:constraint_metric(duality_theory_in_optimization, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(duality_theory_in_optimization, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(duality_theory_in_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. Duality theory involves no asymmetric extraction between agents — it is a mathematical fact about convex problems. The low value reflects that there is no beneficiary or victim, only a structural property that all agents must accommodate. Suppression (0.03): Extremely low. Duality is not suppressive — agents are not coerced into accepting it; they simply encounter it as a logical fact. Any agent can walk away from convex optimization entirely and use non-convex or heuristic approaches. Theater ratio (0.15): Very low. Duality has minimal performative content — the theory directly enables practical algorithmic advances (dual decomposition, cutting-plane methods, interior-point methods). The low theater reflects that the constraint's function is transparently related to its mechanism.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify as mountain because duality theory is logically invariant across observer positions. The computational solver sees an immutable boundary; the mathematical observer sees a logical theorem; the optimization community sees a persistent structural feature across 60 years of algorithmic development. No perspectival gap arises because the constraint has no beneficiaries or victims — it is a structural property that all agents accommodate identically.
 *
 * DIRECTIONALITY LOGIC:
 *   Duality theory has no directionality structure in the sense of extraction or beneficiary/victim relationships. Every agent — whether solver, researcher, or mathematician — encounters the same logical boundary. There is no d value to compute because there is no asymmetric relationship between agents and the constraint. The constraint is observer-invariant: it is the same constraint from all perspectives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_convex_extension_frontier,
    'Do duality-like bounds exist for non-convex optimization, or is strong duality specific to convex problems?',
    'Empirical development: advances in non-convex duality theory, semidefinite relaxations, or proof that duality is fundamentally limited to convex domains',
    'If non-convex duality: mountain classification extends to broader optimization class. If duality is convex-specific: classification remains mountain but scope is bounded to convex problems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_convex_extension_frontier, empirical, 'Whether duality extends to non-convex optimization').

omega_variable(
    numerical_stability_vs_logical_equivalence,
    'When strong duality holds logically but numerical solvers fail to find dual solutions, does the logical equivalence remain a mountain or become a piton (theater-masked degradation)?',
    'Case studies in interior-point methods, cutting-plane algorithms, and distributed optimization where strong duality is theoretically guaranteed but numerically inaccessible',
    'If numerical failure is merely implementation difficulty: mountain persists (constraint is logical, not computational). If numerical failure reveals that duality is not practically accessible: classify as piton with high theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(numerical_stability_vs_logical_equivalence, empirical, 'Gap between logical and numerical accessibility of duality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(duality_theory_in_optimization, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duality_tr_t0, duality_theory_in_optimization, theater_ratio, 0, 0.15).
narrative_ontology:measurement(duality_tr_t20, duality_theory_in_optimization, theater_ratio, 20, 0.15).
narrative_ontology:measurement(duality_tr_t60, duality_theory_in_optimization, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(duality_be_t0, duality_theory_in_optimization, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(duality_be_t20, duality_theory_in_optimization, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(duality_be_t60, duality_theory_in_optimization, base_extractiveness, 60, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(duality_theory_in_optimization, information_standard).

% DUAL FORMULATION NOTE:
% Duality theory in optimization is a foundational mathematical structure with no natural constraint decomposition. The theory itself is monolithic — weak duality and strong duality are logical consequences of the same geometric properties. Unlike BGS spectral universality (which decomposes into spectral and eigenstate components), duality does not split into structurally distinct claims with different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
