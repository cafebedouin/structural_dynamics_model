% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hamiltonian_path_complexity, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hamiltonian_path_complexity
 *   human_readable: Computational Complexity of the Hamiltonian Path Problem
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Hamiltonian Path Problem, a classic problem in computer science and
 *   graph theory, asks whether a path exists in a given graph that visits
 *   each vertex exactly once. It is known to be NP-complete, meaning that no
 *   polynomial-time algorithm is known to solve it. This inherent
 *   computational complexity acts as a constraint on algorithm design and
 *   problem-solving approaches.
 *
 * KEY AGENTS:
 *   - Powerless Algorithm Designer: Unable to find a polynomial-time solution (powerless/trapped)
 *   - Analytical Observer: Recognizes NP-completeness (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.15).
domain_priors:suppression_score(hamiltonian_path_complexity, 0.01).
domain_priors:theater_ratio(hamiltonian_path_complexity, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.15).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hamiltonian_path_complexity, mountain).
narrative_ontology:human_readable(hamiltonian_path_complexity, "Computational Complexity of the Hamiltonian Path Problem").
narrative_ontology:topic_domain(hamiltonian_path_complexity, "technological/mathematical").

domain_priors:emerges_naturally(hamiltonian_path_complexity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The algorithm designer is trapped by the inherent complexity of the problem, unable to find a polynomial-time solution.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The analytical observer recognizes the inherent NP-completeness of the Hamiltonian Path Problem, regardless of perspective.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hamiltonian_path_complexity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, ExtMetricName, E),
    domain_priors:suppression_score(hamiltonian_path_complexity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hamiltonian_path_complexity),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hamiltonian_path_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The problem's inherent complexity (NP-completeness) makes it difficult to solve efficiently. The extractiveness is low because while it does hinder the design of algorithms, alternative methods and approximations exist. Suppression is nearly zero due to the freedom to explore different approaches. Theater is low because the properties are clearly defined and proven, rather than merely claimed or performed.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap as the inherent mathematical nature of the problem results in similar classification from all viewpoints. The problem's difficulty arises from its intrinsic computational characteristics.
 *
 * DIRECTIONALITY LOGIC:
 *   Both agents are constrained by the properties of the problem itself. They cannot alter the inherent complexity or the lack of a known polynomial-time solution.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A - Mountain
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hamiltonian_path_complexity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
