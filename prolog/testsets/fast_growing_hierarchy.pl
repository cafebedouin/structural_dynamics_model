% ============================================================================
% CONSTRAINT STORY: fast_growing_hierarchy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_fast_growing_hierarchy, []).

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
 * * constraint_id: fast_growing_hierarchy
 * human_readable: The Fast-Growing Hierarchy (FGH)
 * domain: technological
 * * SUMMARY:
 * The Fast-Growing Hierarchy is a family of functions indexed by ordinals that
 * classifies the growth rate of computable functions. It defines a hard
 * boundary for what can be computed within specific axiomatic systems; for
 * example, Peano Arithmetic cannot prove the termination of functions
 * growing at the rate of f_epsilon_0. It represents an immutable landscape of
 * computational complexity.
 * * KEY AGENTS:
 * - The Computer Scientist: Encounters the FGH as a hard physical limit on computation.
 * - The Proof Theorist: Uses the FGH as a tool to measure the strength of mathematical systems.
 * - The Analytical Observer: Views the hierarchy as an objective, unchangeable feature of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% A mathematical law has no inherent extractiveness or suppressive function.
% It is an objective feature of a logical system.
domain_priors:base_extractiveness(fast_growing_hierarchy, 0.05). % Mountain extraction <= 0.15
domain_priors:suppression_score(fast_growing_hierarchy, 0.01).   % Mountain suppression <= 0.05
domain_priors:theater_ratio(fast_growing_hierarchy, 0.0).       % No performative aspect.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(fast_growing_hierarchy, extractiveness, 0.05).
narrative_ontology:constraint_metric(fast_growing_hierarchy, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fast_growing_hierarchy, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% As a mathematical theorem, its claim is to be a natural law of its domain.
narrative_ontology:constraint_claim(fast_growing_hierarchy, mountain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   This is a uniform-type constraint (Mountain-only). The classification is
   invariant across all perspectives because it is a mathematical fact.
   ========================================================================== */

% PERSPECTIVE 1: THE PROGRAMMER (POWERLESS)
% Encounters the hierarchy as an insurmountable wall.
constraint_indexing:constraint_classification(fast_growing_hierarchy, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

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
    E =< 0.15,
    S =< 0.05.

:- end_tests(fast_growing_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file incorrectly assigned a high base extractiveness (0.6) to a
 * mathematical law, creating a logical contradiction. A mathematical concept
 * like the Fast-Growing Hierarchy does not "extract" resources in the sense
 * of a Snare or Tangled Rope; it defines an immutable boundary. The feeling of
 * being "trapped" by it (as a programmer might feel) is a confrontation with
 * a natural limit, not a constructed, coercive mechanism.
 *
 * To resolve this, the constraint has been re-classified as a pure Mountain.
 * Its base extractiveness and suppression scores are set to near-zero,
 * reflecting its status as an objective, non-agential feature of logic.
 * The classification is now `mountain` from all perspectives, demonstrating
 * the invariance expected of a natural law. This is a "uniform-type" constraint.
 *
 * * MANDATROPHY ANALYSIS:
 * Mandatrophy is not applicable here. As a pure Mountain (a mathematical fact),
 * there is no coordination function to degrade or extractive purpose to disguise.
 * The system correctly identifies it as an unchangeable feature of the domain,
 * preventing misclassification as a remediable Snare or a faulty Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% As a low-extraction Mountain, no omega_variable is required by the linter.
% The primary uncertainty is philosophical (related to the Church-Turing thesis)
% rather than structural within the model's scope.

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
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Not applicable. As a Mountain, this constraint has no coordination function,
% so coordination_type and boltzmann_floor_override are not relevant. It is a
% foundational concept and does not structurally influence other constraints
% in the sense of policy or institutional coupling.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (fast_growing_hierarchy)
% ============================================================================
constraint_beneficiary(fast_growing_hierarchy, proof_theorists).
constraint_victim(fast_growing_hierarchy, none).

omega_variable(
    omega_fgh_church_turing,
    "Does the FGH hierarchy reflect an absolute limit of computation, or could non-standard models of arithmetic reveal computable functions beyond its reach?",
    "Advances in reverse mathematics and the relationship between proof-theoretic ordinals and computational complexity.",
    "If absolute: The Mountain is permanent. If non-standard models apply: The hierarchy's scope narrows to a specific axiom system.",
    confidence_without_resolution(high)
).
