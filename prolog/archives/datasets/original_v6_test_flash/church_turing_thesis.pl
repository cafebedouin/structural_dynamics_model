% ============================================================================
% CONSTRAINT STORY: church_turing_thesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis, []).

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
 *   constraint_id: church_turing_thesis
 *   human_readable: Church-Turing Thesis (Computability Boundary)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Church-Turing Thesis asserts that any function that can be computed
 *   by an algorithm can be computed by a Turing machine. It defines a
 *   boundary on what is computable. There is very little extraction or
 *   suppression because it's a statement about fundamental limits, not about
 *   actively preventing computation. The theater ratio is correspondingly
 *   low, as there's very little performance involved. The thesis is
 *   considered a mountain constraint because it describes a natural limit on
 *   computation.
 *
 * KEY AGENTS:
 *   - Uncomputable Functions: Primary target (powerless/trapped) — cannot be computed.
 *   - Theoretical Computer Science: Beneficiary (institutional/analytical) — defines the field's scope.
 *   - Mathematical Logic: Secondary actor (analytical/analytical) — defines the limits of provability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis, 0.05).
domain_priors:suppression_score(church_turing_thesis, 0.01).
domain_priors:theater_ratio(church_turing_thesis, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis, extractiveness, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(church_turing_thesis, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(church_turing_thesis, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis, mountain).
narrative_ontology:human_readable(church_turing_thesis, "Church-Turing Thesis (Computability Boundary)").
narrative_ontology:topic_domain(church_turing_thesis, "technological/mathematical").

domain_priors:emerges_naturally(church_turing_thesis).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of functions that are inherently uncomputable, the Church-Turing Thesis represents a fundamental limit. These functions are 'trapped' by the boundary and cannot be computed by any Turing machine, regardless of time or resources.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of theoretical computer science, the Church-Turing Thesis is a foundational principle that defines the scope of computation. It serves as a fixed boundary, influencing the design of algorithms and computational models. Analytical exit because the community can investigate consequence within the formal framework of computability.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of mathematical logic, the Church-Turing Thesis is a statement about the limits of what can be proven algorithmically. It is a fixed point that constrains the application of formal systems. This perspective has analytical exit.
constraint_indexing:constraint_classification(church_turing_thesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(church_turing_thesis, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(church_turing_thesis, ExtMetricName, E),
    domain_priors:suppression_score(church_turing_thesis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(church_turing_thesis),
    narrative_ontology:constraint_metric(church_turing_thesis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(church_turing_thesis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(church_turing_thesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Church-Turing Thesis is not actively enforced; it's a descriptive statement about the inherent limits of computation. Extractiveness and suppression are both minimal. The thesis is seen as a fundamental limit on computation, hence the 'mountain' classification.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives presented view the Church-Turing Thesis as a mountain. This is because the thesis describes a fundamental limitation on what can be computed, regardless of the specific computing agent or context.
 *
 * DIRECTIONALITY LOGIC:
 *   The thesis is a limit, not an active extraction mechanism. Functions cannot be computed and mathematics and computer science must work within this boundary. This is a limitation on action.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis, 0, 100).

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
