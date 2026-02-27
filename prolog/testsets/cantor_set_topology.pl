% ============================================================================
% CONSTRAINT STORY: cantor_set_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cantor_set_topology, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cantor_set_topology
 *   human_readable: Topological Properties of the Cantor Ternary Set
 *   domain: mathematical/topology
 *
 * SUMMARY:
 *   The Cantor Ternary Set is constructed by iteratively removing the open
 *   middle third from every remaining closed interval. Starting with [0,1],
 *   remove (1/3, 2/3), leaving [0, 1/3] ∪ [2/3, 1]. Repeat for each segment
 *   ad infinitum. The limiting set is closed, nowhere dense, has Lebesgue
 *   measure zero, yet contains uncountably many points. These properties are
 *   topological invariants — they hold necessarily from the definition and
 *   cannot be otherwise. The constraint operates at the level of mathematical
 *   possibility: the topology of the Cantor Set is a mountain (unchangeable,
 *   no degrees of freedom) from every perspective. No agent, framework, or
 *   mathematical interpretation can make the set connected without changing
 *   the definition itself.
 *
 * KEY AGENTS:
 *   - Mathematical axioms (ZFC): Immutable foundation — the properties follow deductively from the axiom set
 *   - The recursive algorithm: Invariant generator — defines the set uniquely and deterministically
 *   - Topological properties (closure, nowhere-denseness, measure-zero): Forced consequences — logically entailed by the definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantor_set_topology, 0.08).
domain_priors:suppression_score(cantor_set_topology, 0.02).
domain_priors:theater_ratio(cantor_set_topology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantor_set_topology, extractiveness, 0.08).
narrative_ontology:constraint_metric(cantor_set_topology, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cantor_set_topology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cantor_set_topology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cantor_set_topology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cantor_set_topology, mountain).
narrative_ontology:human_readable(cantor_set_topology, "Topological Properties of the Cantor Ternary Set").
narrative_ontology:topic_domain(cantor_set_topology, "mathematical/topology").

domain_priors:emerges_naturally(cantor_set_topology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING MATHEMATICIAN (MOUNTAIN) — Cannot escape the topological properties of the Cantor Set. The set's closure, nowhere-denseness, and measure-zero property are invariant across all mathematical frameworks. These properties emerge necessarily from the recursive removal algorithm and the axioms of real analysis. No alternative interpretation or escape route exists.
constraint_indexing:constraint_classification(cantor_set_topology, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / PROOF VERIFICATION (MOUNTAIN) — From the perspective of formal proof and logical consistency, the Cantor Set's topological structure is a mathematical necessity. The properties follow deductively from the definition and the axioms of real analysis. The constraint is the logical dependency: once you accept the definition and the axioms, the topological properties are forced. Zero degrees of freedom for all indices — the mathematics is completely determined.
constraint_indexing:constraint_classification(cantor_set_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: STUDENT LEARNING TOPOLOGY (MOUNTAIN) — The student encounters the Cantor Set as an immutable constraint on what is possible in topology. No amount of effort or desire can make the Cantor Set connected, measurable (in the Lebesgue sense), or countable in its elements. The properties are fixed facts that must be accepted and worked with, not negotiated or circumvented.
constraint_indexing:constraint_classification(cantor_set_topology, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantor_set_topology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cantor_set_topology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantor_set_topology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cantor_set_topology, ExtMetricName, E),
    domain_priors:suppression_score(cantor_set_topology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cantor_set_topology),
    narrative_ontology:constraint_metric(cantor_set_topology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cantor_set_topology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cantor_set_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. There is no extraction occurring — no agent bears cost and no agent benefits disproportionately. The Cantor Set is a pure mathematical object whose properties are invariant and necessitated. All agents (mathematicians, students, analytical observers) encounter the same immutable constraint. Suppression (0.02): Negligible. No coercion is present. The properties cannot be suppressed or negotiated because they are logical consequences. The structure emerges naturally from the recursive definition. Theater ratio (0.15): Very low. There is minimal performative content. The proofs of the Cantor Set's properties are direct logical derivations with no extraneous ritual or theater. The mathematics is functional and transparent.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists. All three perspectives classify the Cantor Set as a mountain because the topological properties are invariant across all frameworks and all observers. The practicing mathematician, the analytical observer verifying proofs, and the student learning the material all encounter identical immutable constraints. The set's closure, nowhere-denseness, measure-zero property, and uncountability are not negotiable or context-dependent. They hold in all standard mathematical frameworks (standard topology on ℝ, Lebesgue measure, etc.). This uniformity across all perspectives is the hallmark of a true mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints require no directionality analysis because no extraction flow exists. There are no beneficiaries or victims, no power asymmetries, and no exit options that would produce a d value. All agents stand in identical structural relationship to the constraint: they must accept the topological properties as fixed facts. The constraint is non-extractive, non-coercive, and universally binding.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cantor_set_topology, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cantor_set_topology, information_standard).
narrative_ontology:affects_constraint(cantor_set_topology, fractal_self_similarity).
narrative_ontology:affects_constraint(cantor_set_topology, nowhere_dense_sets).
narrative_ontology:affects_constraint(cantor_set_topology, measure_zero_paradox).

% DUAL FORMULATION NOTE:
% The Cantor Set constraint family includes related topological constraints: the fractal self-similarity property (affects_constraints: cantor_set_topology), nowhere-dense sets more broadly (affects_constraints: cantor_set_topology), and the measure-zero paradox (why a set with uncountably many points has zero measure). The Cantor Set topology is the most fundamental member — the other constraints derive from or exemplify its properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
