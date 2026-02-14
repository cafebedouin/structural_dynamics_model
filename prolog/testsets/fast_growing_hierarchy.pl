% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-01
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

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
    omega_variable/5.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fast_growing_hierarchy
 *   human_readable: The Fast-Growing Hierarchy (FGH)
 *   domain: technological
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy is a family of functions indexed by ordinals that
 *   classifies the growth rate of computable functions. It defines a hard
 *   boundary for what can be computed within specific axiomatic systems; for
 *   example, Peano Arithmetic cannot prove the termination of functions
 *   growing at the rate of f_epsilon_0. It represents an immutable landscape of
 *   computational complexity.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Computer Scientist: Encounters the FGH as a hard limit (powerless/trapped).
 *   - The Proof Theorist: Uses the FGH as a tool to measure system strength (institutional/arbitrage).
 *   - The Analytical Observer: Views the hierarchy as an objective, unchangeable feature of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A mathematical law has no inherent extractiveness or suppressive function.
% It is an objective feature of a logical system.
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.05). % Mountain extraction <= 0.25
domain_priors:suppression_score(fast_growing_hierarchy, 0.01).   % Mountain suppression <= 0.05
domain_priors:theater_ratio(fast_growing_hierarchy, 0.0).       % No performative aspect.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.05).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fast_growing_hierarchy, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% A mathematical hierarchy is completely inescapable within its axiomatic system.
narrative_ontology:constraint_metric(fast_growing_hierarchy, accessibility_collapse, 1.0).
% One cannot "resist" a mathematical fact; resistance is incoherent.
narrative_ontology:constraint_metric(fast_growing_hierarchy, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
% As a mathematical theorem, its claim is to be a natural law of its domain.
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).

% --- Emergence flag (required for mountain constraints) ---
% The hierarchy emerges naturally from the axioms of arithmetic and set theory.
domain_priors:emerges_naturally(fast_growing_hierarchy).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain, this constraint has no beneficiaries or
% victims in the structural sense. It is a feature of the logical landscape.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   This is a uniform-type constraint (Mountain-only). The classification is
   invariant across all perspectives because it is a mathematical fact.
   Directionality (d) is irrelevant as there are no beneficiaries or victims.
   ========================================================================== */

% PERSPECTIVE 1: THE PROGRAMMER (POWERLESS)
% Encounters the hierarchy as an insurmountable wall.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PROOF THEORIST (INSTITUTIONAL)
% Uses the hierarchy as a fixed measuring stick.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Views the hierarchy as a fundamental, unchangeable feature of logic.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fast_growing_hierarchy_tests).

test(perspectival_invariance) :-
    % Verify that for a uniform-type constraint, the classification is the same
    % from all major perspectives.
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fast_growing_hierarchy, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(mountain_threshold_adherence) :-
    % Verify the base metrics adhere to the strict thresholds for a Mountain.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, ExtMetricName, E),
    narrative_ontology:constraint_metric(fast_growing_hierarchy, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   A mathematical concept like the Fast-Growing Hierarchy does not "extract"
 *   resources; it defines an immutable boundary. The feeling of being "trapped"
 *   by it (as a programmer might feel) is a confrontation with a natural limit,
 *   not a constructed, coercive mechanism. To reflect this, the constraint is
 *   classified as a pure Mountain. Its base extractiveness and suppression
 *   scores are set to near-zero, reflecting its status as an objective,
 *   non-agential feature of logic. The required NL Profile metrics
 *   (accessibility_collapse, resistance, emerges_naturally) are included to
 *   pass the natural law certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The classification is `mountain` from all
 *   perspectives, demonstrating the invariance expected of a natural law. This
 *   is a "uniform-type" constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. As a pure Mountain, the FGH has no
 *   structural beneficiaries or victims. It is an objective feature of the
 *   logical landscape, not a mechanism for transferring value between groups.
 *   Therefore, no beneficiary/victim declarations are made.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable here. As a pure Mountain (a mathematical fact),
 *   there is no coordination function to degrade or extractive purpose to disguise.
 *   The system correctly identifies it as an unchangeable feature of the domain,
 *   preventing misclassification as a remediable Snare or a faulty Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fgh_church_turing,
    "Does the FGH hierarchy reflect an absolute limit of computation, or could non-standard models of arithmetic reveal computable functions beyond its reach?",
    "Advances in reverse mathematics and the relationship between proof-theoretic ordinals and computational complexity.",
    "If absolute: The Mountain is permanent. If non-standard models apply: The hierarchy's scope narrows to a specific axiom system.",
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fast_growing_hierarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a low-extraction Mountain representing a mathematical
% constant, there is no lifecycle drift to measure.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable. As a Mountain, this constraint has no coordination function,
% so coordination_type and boltzmann_floor_override are not relevant. It is a
% foundational concept and does not structurally influence other constraints
% in the sense of policy or institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. No overrides are needed as there is no directionality to correct.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */